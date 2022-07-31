(* bin-io-fn.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * QUESTION: what operations should raise exceptions when the stream is
 * closed?
 *)

local
  structure Int = IntImp
  structure Position = PositionImp
in
functor BinIOFn (

    structure OSPrimIO : OS_PRIM_IO
      where PrimIO = BinPrimIO

  ) : BIN_IO = struct

    structure PIO = OSPrimIO.PrimIO
    structure A = Word8Array
    structure AS = Word8ArraySlice
    structure V = Word8Vector
    structure VS = Word8VectorSlice
    structure Pos = Position

  (* an element for initializing buffers *)
    val someElem = (0w0 : Word8.word)

(** Fast, but unsafe version (from Word8Vector) **
    val vecSub = InlineT.Word8Vector.sub
    val arrUpdate = InlineT.Word8Array.update
  (* fast vector extract operation.  This should never be called with
   * a length of 0.
   *)
    fun vecExtract (v, base, optLen) = let
	  val len = V.length v
	  fun newVec n = let
		val newV = Assembly.A.create_s n
		fun fill i = if (i < n)
		      then (
			InlineT.Word8Vector.update(newV, i, vecSub(v, base+i));
			fill(i+1))
		      else ()
		in
		  fill 0; newV
		end
	  in
	    case (base, optLen)
	     of (0, NONE) => v
	      | (_, NONE) => newVec (len - base)
	      | (_, SOME n) => newVec n
	    (* end case *)
	  end
**)
    val vecExtract = VS.vector o VS.slice
    val vecSub = V.sub
    val arrUpdate = A.update
    val empty = V.fromList[]

    structure StreamIO =
      struct
	type vector = V.vector
	type elem = V.elem
	type reader = PIO.reader
	type writer = PIO.writer
	type pos = PIO.pos

      (* maximum size of an input request *)
	val maxInputSz = Position.fromInt V.maxLen

      (*** Functional input streams ***
       ** We represent an instream by a pointer to a buffer and an offset
       ** into the buffer.  The buffers are chained by the "more" field from
       ** the beginning of the stream towards the end.  If the "more" field
       ** is EOS, then it refers to an empty buffer (consuming the EOF marker
       ** involves moving the stream from immeditaly in front of the EOS to
       ** to the empty buffer).  A "more" field of TERMINATED marks a
       ** terminated stream.  We also have the invariant that the "tail"
       ** field of the "info" structure points to a more ref that is either
       ** NOMORE or TERMINATED.
       **)
	datatype instream = ISTRM of (in_buffer * int)
	and in_buffer = IBUF of {
	    basePos : pos option,
	    more : more ref,
	    data : vector,
	    info : info
	  }
	and more
	  = MORE of in_buffer	(* forward link to additional data *)
	  | EOS of in_buffer	(* End of stream marker *)
	  | NOMORE		(* placeholder for forward link *)
	  | TERMINATED		(* termination of the stream *)

	and info = INFO of {
	    reader : reader,
	    readVec : int -> vector,
	    readVecNB : (int -> vector) option,
	    closed : bool ref,
	    getPos : unit -> pos option,
	    tail : more ref ref, (* points to the more cell of the last buffer *)
	    cleanTag : CleanIO.tag
	  }

	fun infoOfIBuf (IBUF{info, ...}) = info
	fun chunkSzOfIBuf buf = let
	      val INFO{reader=PIO.RD{chunkSize, ...}, ...} = infoOfIBuf buf
	      in
		chunkSize
	      end
	fun readVec (IBUF{info=INFO{readVec=f, ...}, ...}) = f

	fun inputExn (INFO{reader=PIO.RD{name, ...}, ...}, mlOp, exn) =
	      raise IO.Io{function=mlOp, name=name, cause=exn}

      (* this exception is raised by readVecNB in the blocking case *)
	exception WouldBlock

	fun extendStream (readFn, mlOp, buf as IBUF{more, info, ...}) = (let
	      val INFO{getPos, tail, ...} = info
              val basePos = getPos()
	      val chunk = readFn (chunkSzOfIBuf buf)
	      val newMore = ref NOMORE
	      val buf' = IBUF{
		      basePos = basePos, data = chunk,
		      more = newMore, info = info
		    }
	      val next = if (V.length chunk = 0) then EOS buf' else MORE buf'
	      in
		more := next;
		tail := newMore;
		next
	      end
		handle ex => inputExn(info, mlOp, ex))

	fun getBuffer (readFn, mlOp) (buf as IBUF{more, info, ...}) = (
	      case !more
	       of TERMINATED => (EOS buf)
		| NOMORE => extendStream (readFn, mlOp, buf)
		| more => more
	      (* end case *))

      (* read a chunk that is at least the specified size *)
	fun readChunk buf = let
	      val INFO{readVec, reader=PIO.RD{chunkSize, ...}, ...} =
		     infoOfIBuf buf
	      in
		case (chunkSize - 1)
		 of 0 => (fn n => readVec n)
(* FIXME: what if the rounded size is > maxInputSz? *)
		  | k => (* round up to next multiple of chunkSize *)
		      (fn n => readVec(Int.quot((n+k), chunkSize) * chunkSize))
		(* end case *)
	      end

	fun generalizedInput getBuf = let
	      fun get (ISTRM(buf as IBUF{data, ...}, pos)) = let
		    val len = V.length data
		    in
		      if (pos < len)
			then (vecExtract(data, pos, NONE), ISTRM(buf, len))
			else (case (getBuf buf)
			   of (EOS buf) => (empty, ISTRM(buf, 0))
			    | (MORE rest) => get (ISTRM(rest, 0))
			    | _ => raise Fail "bogus getBuf"
			  (* end case *))
		    end
	      in
		get
	      end

      (* terminate an input stream *)
	fun terminate (INFO{tail, cleanTag, ...}) = (case !tail
	       of (m as ref NOMORE) => (
		    CleanIO.removeCleaner cleanTag;
		    m := TERMINATED)
		| (m as ref TERMINATED) => ()
		| _ => raise Match (* shut up compiler *)
	      (* end case *))

	fun input (strm as ISTRM(buf, _)) =
	      generalizedInput (getBuffer (readVec buf, "input")) strm
	fun input1 (ISTRM(buf, pos)) = let
	      val IBUF{data, more, ...} = buf
	      in
		if (pos < V.length data)
		  then SOME(vecSub(data, pos), ISTRM(buf, pos+1))
		  else (case !more
		     of (MORE buf) => input1 (ISTRM(buf, 0))
		      | (EOS _) => NONE
		      | NOMORE => (
			  case extendStream (readVec buf, "input1", buf)
			   of (MORE rest) => input1 (ISTRM(rest, 0))
			    | _ => NONE
			  (* end case *))
		      | TERMINATED => NONE
		    (* end case *))
	      end
	fun inputN (ISTRM(buf, pos), n) = let
	      fun join (item, (list, strm)) = (item::list, strm)
	      fun inputList (buf as IBUF{data, ...}, i, n) = let
		    val len = V.length data
		    val remain = len-i
		    in
		      if (remain >= n)
			then ([vecExtract(data, i, SOME n)], ISTRM(buf, i+n))
		      else join (
			vecExtract(data, i, NONE),
			nextBuf(buf, n-remain))
		    end
	      and nextBuf (buf as IBUF{more, data, ...}, n) = (case !more
		     of (MORE buf) => inputList (buf, 0, n)
		      | (EOS buf) => ([], ISTRM(buf, 0))
		      | NOMORE => (
			  case extendStream (readVec buf, "inputN", buf)
			   of (MORE rest) => inputList (rest, 0, n)
			    | _ => ([], ISTRM(buf, V.length data))
			  (* end case *))
		      | TERMINATED => ([], ISTRM(buf, V.length data))
		    (* end case *))
	      val (data, strm) = inputList (buf, pos, n)
	      in
		(V.concat data, strm)
	      end

	fun inputAll (strm as ISTRM(buf, _)) = let
	      val INFO{reader=PIO.RD{avail, ...}, ...} = infoOfIBuf buf
 	    (* Read a chunk that is as large as the available input. *)
	      fun bigChunk _ = let
		    val delta = (case avail()
			   of NONE => chunkSzOfIBuf buf
			    | (SOME n) => if (n > maxInputSz)
				then raise Size
				else Position.toInt n
			  (* end case *))
		    in
		      readChunk buf delta
		    end
	      val bigInput =
		    generalizedInput (getBuffer (bigChunk, "inputAll"))
	      fun loop (v, strm) = if (V.length v = 0)
		    then ([], strm)
		    else let val (l, strm') = loop(bigInput strm)
		      in
			(v :: l, strm')
		      end
	      val (data, strm') = loop (bigInput strm)
	      in
		(V.concat data, strm')
	      end
      (* Return SOME k, if k <= amount characters can be read without blocking. *)
	fun canInput (strm as ISTRM(buf, pos), amount) = let
	      val readVecNB = (case buf
		   of (IBUF{info as INFO{readVecNB=NONE, ...}, ...}) =>
			inputExn(info, "canInput", IO.NonblockingNotSupported)
		    | (IBUF{info=INFO{readVecNB=SOME f, ...}, ...}) => f
		  (* end case *))
	      fun tryInput (buf as IBUF{data, ...}, i, n) = let
		    val len = V.length data
		    val remain = len - i
		    in
		      if (remain >= n)
			then SOME n
			else nextBuf (buf, n - remain)
		    end
	      and nextBuf (IBUF{more, ...}, n) = (case !more
		     of (MORE buf) => tryInput (buf, 0, n)
		      | (EOS _) => SOME(amount - n)
		      | TERMINATED => SOME(amount - n)
		      | NOMORE => ((
			  case extendStream (readVecNB, "canInput", buf)
			   of (MORE b) => tryInput (b, 0, n)
			    | _ => SOME(amount - n)
			  (* end case *))
			    handle IO.Io{cause=WouldBlock, ...} => SOME(amount - n))
		    (* end case *))
	      in
		if (amount < 0)
		  then raise Size
		  else tryInput (buf, pos, amount)
	      end
	fun closeIn (ISTRM(buf, _)) = (case (infoOfIBuf buf)
	       of INFO{closed=ref true, ...} => ()
		| (info as INFO{closed, reader=PIO.RD{close, ...}, ...}) => (
		    terminate info;
		    closed := true;
		    close() handle ex => inputExn(info, "closeIn", ex))
	      (* end case *))
	fun endOfStream (ISTRM(buf, pos)) = (case buf
	       of (IBUF{more=ref(MORE _), ...}) => false
		| (IBUF{more, data, info=INFO{closed, ...}, ...}) =>
		    if (pos = V.length data)
		      then (case (!more, !closed)
			 of (NOMORE, false) => (
			      case extendStream (readVec buf, "endOfStream", buf)
			       of (EOS _) => true
				| _ => false
			    (* end case *))
			  | _ => true
			(* end case *))
		      else false
	      (* end case *))
	fun mkInstream (reader, data) = let
	      val PIO.RD{readVec, readVecNB, getPos, setPos, ...} = reader
	      val readVec' = (case readVec
		     of NONE => (fn _ => raise IO.BlockingNotSupported)
		      | (SOME f) => f
		    (* end case *))
	      val readVecNB' = (case readVecNB
		     of NONE => NONE
		      | (SOME f) => SOME(fn arg => case (f arg)
			   of (SOME x) => x
			    | NONE => raise WouldBlock
			  (* end case *))
		    (* end case *))
	      val getPos = (case (getPos, setPos)
		     of (SOME f, SOME _) => (fn () => SOME(f()))
		      | _ => (fn () => NONE)
		    (* end case *))
	      val more = ref NOMORE
	      val closedFlg = ref false
	      val tag = CleanIO.addCleaner {
		      init = fn () => (closedFlg := true),
		      flush = fn () => (),
		      close = fn () => (closedFlg := true)
		    }
	      val info = INFO{
		      reader=reader, readVec=readVec', readVecNB=readVecNB',
		      closed = closedFlg, getPos = getPos, tail = ref more,
		      cleanTag = tag
		    }
(** What should we do about the position when there is initial data?? **)
(** Suggestion: When building a stream with supplied initial data,
 ** nothing can be said about the positions inside that initial
 ** data (who knows where that data even came from!).
 **)
	      val basePos = if (V.length data = 0)
		    then getPos()
		    else NONE
	      in
		ISTRM(
		  IBUF{basePos = basePos, data = data, info = info, more = more},
		  0)
	      end
	fun getReader (ISTRM(buf, pos)) = let
	      val IBUF{data, info as INFO{reader, ...}, more, ...} = buf
	      fun getData (MORE(IBUF{data, more, ...})) = data :: getData(!more)
		| getData _ = []
	      in
		terminate info;
		if (pos < V.length data)
		  then (
		      reader,
		      V.concat(vecExtract(data, pos, NONE) :: getData(!more))
		    )
		  else (reader, V.concat(getData(!more)))
	      end

      (* Get the underlying file position of a stream *)
	fun filePosIn (ISTRM(buf, pos)) = (case buf
	       of IBUF{basePos=NONE, info, ...} =>
		    inputExn (info, "filePosIn", IO.RandomAccessNotSupported)
		| IBUF{basePos=SOME b, info, ...} =>
		    Position.+(b, Position.fromInt pos)
	      (* end case *))


      (*** Output streams ***)
	datatype outstream = OSTRM of {
	    buf : A.array,			(* output buffer *)
	    pos : int ref,			(* position of next free byte in `buf` *)
	    closed : bool ref,
	    bufferMode : IO.buffer_mode ref,
	    writer : writer,
	    writeArr : AS.slice -> unit,
	    writeVec : VS.slice -> unit,
	    cleanTag : CleanIO.tag
	  }

	fun outputExn (OSTRM{writer=PIO.WR{name, ...}, ...}, mlOp, exn) =
	      raise IO.Io{function=mlOp, name=name, cause=exn}

	fun isClosedOut (strm as OSTRM{closed=ref true, ...}, mlOp) =
	      outputExn (strm, mlOp, IO.ClosedStream)
	  | isClosedOut _ = ()

	fun flushBuffer (strm as OSTRM{buf, pos, writeArr, ...}, mlOp) = (
	      case !pos
	       of 0 => ()
		| n => ((
		    writeArr (AS.slice (buf, 0, SOME n)); pos := 0)
		      handle ex => outputExn (strm, mlOp, ex))
	      (* end case *))

	fun output (strm as OSTRM os, v) = let
	      val _ = isClosedOut (strm, "output")
	      val {buf, pos, bufferMode, ...} = os
	      fun flush () = flushBuffer (strm, "output")
	      fun flushAll () = (#writeArr os (AS.full buf)
		    handle ex => outputExn (strm, "output", ex))
	      fun writeDirect () = (
		    case !pos
		     of 0 => ()
		      | n => (#writeArr os (AS.slice (buf, 0, SOME n));
			      pos := 0)
		    (* end case *);
		    #writeVec os (VS.full v))
		      handle ex => outputExn (strm, "output", ex)
	      fun insert copyVec = let
		    val bufLen = A.length buf
		    val dataLen = V.length v
		    in
		      if (dataLen >= bufLen)
			then writeDirect()
			else let
			  val i = !pos
			  val avail = bufLen - i
			  in
			    if (avail < dataLen)
			      then (
				copyVec(v, 0, avail, buf, i);
				flushAll();
				copyVec(v, avail, dataLen-avail, buf, 0);
			  	pos := dataLen-avail)
			    else (
			      copyVec(v, 0, dataLen, buf, i);
			      pos := i + dataLen;
			      if (avail = dataLen) then flush() else ())
			  end
		    end
	      in
		case !bufferMode
		 of IO.NO_BUF => writeDirect ()
		  | _ => let
		      fun copyVec (src, srcI, srcLen, dst, dstI) =
			  AS.copyVec { src = VS.slice (src, srcI, SOME srcLen),
				       dst = dst, di = dstI }
		      in
			insert copyVec
		      end
		(* end case *)
	      end

	fun output1 (strm as OSTRM{buf, pos, bufferMode, writeArr, ...}, elem) = (
	      isClosedOut (strm, "output1");
	      case !bufferMode
	       of IO.NO_BUF => (
		    arrUpdate (buf, 0, elem);
		    writeArr (AS.slice (buf, 0, SOME 1))
		      handle ex => outputExn (strm, "output1", ex))
		| _ => let
		    val i = !pos
		    val i' = i+1
		    in
		      arrUpdate (buf, i, elem);
		      pos := i';
		      if (i' = A.length buf)
			then flushBuffer (strm, "output1")
			else ()
		    end
	      (* end case *))

	fun flushOut strm = (
	      flushBuffer (strm, "flushOut"))

	fun closeOut (strm as OSTRM{writer=PIO.WR{close, ...}, closed, cleanTag, ...}) =
	      if !closed
		then ()
		else (
		  flushBuffer (strm, "closeOut");
		  closed := true;
		  CleanIO.removeCleaner cleanTag;
		  close())

	fun mkOutstream (wr as PIO.WR{chunkSize, writeArr, writeVec, ...}, mode) = let
	      fun iterate (f, size, subslice) = let
		    fun lp sl = if size sl = 0
			  then ()
			  else let
			    val n = f sl
			    in
			      lp (subslice (sl, n, NONE))
			    end
		    in
		      lp
		    end
	      val writeArr' = (case writeArr
		     of NONE => (fn _ => raise IO.BlockingNotSupported)
		      | (SOME f) => iterate (f, AS.length, AS.subslice)
		    (* end case *))
	      val writeVec' = (case writeVec
		     of NONE => (fn _ => raise IO.BlockingNotSupported)
		      | (SOME f) => iterate (f, VS.length, VS.subslice)
		    (* end case *))
	    (* install a dummy cleaner *)
	      val tag = CleanIO.addCleaner {
		      init = fn () => (),
		      flush = fn () => (),
		      close = fn () => ()
		    }
	      val strm = OSTRM{
		      buf = A.array(chunkSize, someElem),
		      pos = ref 0,
		      closed = ref false,
		      bufferMode = ref mode,
		      writer = wr,
		      writeArr = writeArr',
		      writeVec = writeVec',
		      cleanTag = tag
		    }
	      in
		CleanIO.rebindCleaner (tag, {
		    init = fn () => closeOut strm,
		    flush = fn () => flushOut strm,
		    close = fn () => closeOut strm
		  });
		strm
	      end

	fun getWriter (strm as OSTRM{writer, bufferMode, ...}) = (
	      flushBuffer (strm, "getWriter");
	      (writer, !bufferMode))

      (** Position operations on outstreams **)
	datatype out_pos = OUTP of {
	    pos : PIO.pos,
	    strm : outstream
	  }

	fun getPosOut (strm as OSTRM{writer, ...}) = (
	      flushBuffer (strm, "getPosOut");
	      case writer
	       of PIO.WR{getPos=SOME f, ...} => (
		    OUTP{pos = f(), strm = strm}
		      handle ex => outputExn(strm, "getPosOut", ex))
		| _ => outputExn(strm, "getPosOut", IO.RandomAccessNotSupported)
	      (* end case *))
	fun filePosOut (OUTP{pos, strm}) = (
	      isClosedOut (strm, "filePosOut"); pos)
	fun setPosOut (OUTP{pos, strm as OSTRM{writer, ...}}) = (
	      isClosedOut (strm, "setPosOut");
	      case writer
	       of PIO.WR{setPos=SOME f, ...} => (
		    (f pos)
		      handle ex => outputExn(strm, "setPosOut", ex))
		| _ => outputExn(strm, "getPosOut", IO.RandomAccessNotSupported)
	      (* end case *))

	fun setBufferMode (strm as OSTRM{bufferMode, ...}, IO.NO_BUF) = (
	      flushBuffer (strm, "setBufferMode");
	      bufferMode := IO.NO_BUF)
	  | setBufferMode (strm as OSTRM{bufferMode, ...}, mode) = (
	      isClosedOut (strm, "setBufferMode");
	      bufferMode := mode)
	fun getBufferMode (strm as OSTRM{bufferMode, ...}) = (
	      isClosedOut (strm, "getBufferMode");
	      !bufferMode)

      end (* StreamIO *)

    type vector = V.vector
    type elem = V.elem
    type instream = StreamIO.instream ref
    type outstream = StreamIO.outstream ref

  (** Input operations **)
    fun input strm = let val (v, strm') = StreamIO.input(!strm)
	  in
	    strm := strm'; v
	  end
    fun input1 strm = (case StreamIO.input1(!strm)
	   of NONE => NONE
	    | (SOME(elem, strm')) => (strm := strm'; SOME elem)
	  (* end case *))
    fun inputN (strm, n) = let val (v, strm') = StreamIO.inputN (!strm, n)
	  in
	    strm := strm'; v
	  end
    fun inputAll (strm : instream) = let
	  val (v, s) = StreamIO.inputAll(!strm)
	  in
	    strm := s; v
	  end
    fun canInput (strm, n) = StreamIO.canInput (!strm, n)
    fun lookahead (strm : instream) = (case StreamIO.input1(!strm)
	   of NONE => NONE
	    | (SOME(elem, _)) => SOME elem
	  (* end case *))
    fun closeIn strm = let
	  val (s as StreamIO.ISTRM(buf as StreamIO.IBUF{data, ...}, _)) = !strm
	(* find the end of the stream *)
	  fun findEOS (StreamIO.IBUF{more=ref(StreamIO.MORE buf), ...}) =
		findEOS buf
	    | findEOS (StreamIO.IBUF{more=ref(StreamIO.EOS buf), ...}) =
		findEOS buf
	    | findEOS (buf as StreamIO.IBUF{data, ...}) =
		StreamIO.ISTRM(buf, V.length data)
	  in
	    StreamIO.closeIn s;
	    strm := findEOS buf
	  end
    fun endOfStream strm = StreamIO.endOfStream(! strm)

  (** Output operations **)
    fun output (strm, v) = StreamIO.output(!strm, v)
    fun output1 (strm, c) = StreamIO.output1(!strm, c)
    fun flushOut strm = StreamIO.flushOut(!strm)
    fun closeOut strm = StreamIO.closeOut(!strm)
    fun getPosOut strm = StreamIO.getPosOut(!strm)
    fun setPosOut (strm, p as StreamIO.OUTP{strm=strm', ...}) = (
	  strm := strm'; StreamIO.setPosOut p)

    fun mkInstream (strm : StreamIO.instream) = ref strm
    fun getInstream (strm : instream) = !strm
    fun setInstream (strm : instream, strm') = strm := strm'

    fun mkOutstream (strm : StreamIO.outstream) = ref strm
    fun getOutstream (strm : outstream) = !strm
    fun setOutstream (strm : outstream, strm') = strm := strm'

  (** Open files **)
    fun openIn fname =
	  mkInstream(StreamIO.mkInstream(OSPrimIO.openRd fname, empty))
	    handle ex => raise IO.Io{function="openIn", name=fname, cause=ex}
    fun openOut fname =
	  mkOutstream(StreamIO.mkOutstream(OSPrimIO.openWr fname, IO.BLOCK_BUF))
	    handle ex => raise IO.Io{function="openOut", name=fname, cause=ex}
    fun openAppend fname =
	  mkOutstream(StreamIO.mkOutstream(OSPrimIO.openApp fname, IO.NO_BUF))
	    handle ex => raise IO.Io{function="openAppend", name=fname, cause=ex}

  end (* BinIOFn *)
end

