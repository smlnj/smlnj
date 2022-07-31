(* text-io-fn.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This is the CML version of the TextIO functor.
 *)

functor TextIOFn (

    structure OSPrimIO : sig
        include OS_PRIM_IO
	val stdIn   : unit -> PrimIO.reader
	val stdOut  : unit -> PrimIO.writer
	val stdErr  : unit -> PrimIO.writer
	val strReader : string -> PrimIO.reader
      end
	where type PrimIO.array = TextPrimIO.array
	where type PrimIO.vector = TextPrimIO.vector
        where type PrimIO.array_slice = TextPrimIO.array_slice
        where type PrimIO.vector_slice = TextPrimIO.vector_slice
	where type PrimIO.elem = TextPrimIO.elem
	where type PrimIO.pos = TextPrimIO.pos
	where type PrimIO.reader = TextPrimIO.reader
	where type PrimIO.writer = TextPrimIO.writer

  ) : CML_TEXT_IO = struct

    structure PIO = OSPrimIO.PrimIO
    structure A = CharArray
    structure AS = CharArraySlice
    structure V = CharVector
    structure VS = CharVectorSlice

    structure SV = SyncVar

  (* assign to an MVar *)
    fun mUpdate (mv, x) = ignore(SV.mSwap(mv, x))

  (* an element for initializing buffers *)
    val someElem = #"\000"

    val vecExtract = VS.vector o VS.slice
    val vecSub = V.sub
    val arrUpdate = A.update
    val substringBase = Substring.base
    val empty = ""

    fun dummyCleaner () = ()

    structure StreamIO =
      struct
	type vector = V.vector
	type elem = V.elem
	type reader = PIO.reader
	type writer = PIO.writer
	type pos = PIO.pos

      (* maximum size of an input request *)
        val maxInputSz = Position.fromInt V.maxLen

      (*** Functional input streams ***)
	datatype instream = ISTRM of (in_buffer * int)
	and in_buffer = IBUF of {
	    basePos : pos option,
	    more : more SV.mvar,	(* when this cell is empty, it means that *)
					(* there is an outstanding request to the *)
					(* server to extend the stream. *)
	    data : vector,
	    info : info
	  }
	and more
	  = MORE of in_buffer	(* forward link to additional data *)
	  | NOMORE		(* placeholder for forward link *)
	  | TERMINATED		(* termination of the stream *)

	and info = INFO of {
	    reader : reader,
	    readVec : int -> vector,
	    readVecEvt : int -> vector CML.event,
	    closed : bool ref,
	    getPos : unit -> pos option,
	    tail : more SV.mvar SV.mvar,
		(* points to the more cell of the last buffer *)
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

	datatype more_data = EOF | DATA of in_buffer

      (* extend the stream by a chunk.
       * Invariant: the more m-variable is empty on entry and full on exit.
       *)
	fun extendStream (readFn, mlOp, buf as IBUF{more, info, ...}) = (let
	      val INFO{getPos, tail, ...} = info
	      val basePos = getPos()
	      val chunk = readFn (chunkSzOfIBuf buf)
	      in
		if (V.length chunk = 0)
		  then (SV.mPut (more, NOMORE); EOF)
		  else let
		    val newMore = SV.mVar()
		    val buf' = IBUF{
			    basePos = basePos, data = chunk,
			    more = newMore, info = info
			  }
		    in
		    (* note that we do not fill the more cell until
		     * after the tail has been updated.  This ensures
		     * that someone attempting to access the tail will
		     * not acquire the lock until after we are done.
		     *)
		      mUpdate (tail, newMore);
		      SV.mPut (more, MORE buf');  (* releases lock!! *)
		      SV.mPut (newMore, NOMORE);
		      DATA buf'
		    end
	      end
		handle ex => (
		  SV.mPut (more, NOMORE);
		  inputExn(info, mlOp, ex)))

      (* get the next buffer in the stream, extending it if necessary.  If
       * the stream must be extended, we lock it by taking the value from the
       * more cell; the extendStream function is responsible for filling in
       * the cell.
       *)
	fun getBuffer (readFn, mlOp) (buf as IBUF{more, info, ...}) = let
	      fun get TERMINATED = EOF
		| get (MORE buf') = DATA buf'
		| get NOMORE = (case SV.mTake more
		     of NOMORE => extendStream (readFn, mlOp, buf)
		      | next => (SV.mPut(more, next); get next)
		    (* end case *))
	      in
		get (SV.mGet more)
	      end

      (* read a chunk that is at least the specified size *)
	fun readChunk buf = let
	      val INFO{readVec, reader=PIO.RD{chunkSize, ...}, ...} =
		     infoOfIBuf buf
	      in
		case (chunkSize - 1)
		 of 0 => (fn n => readVec n)
		  | k => (* round up to next multiple of chunkSize *)
		      (fn n => readVec(Int.quot(n+k, chunkSize) * chunkSize))
		(* end case *)
	      end

	fun generalizedInput getBuf = let
	      fun get (ISTRM(buf as IBUF{data, ...}, pos)) = let
		    val len = V.length data
		    in
		      if (pos < len)
			then (vecExtract(data, pos, NONE), ISTRM(buf, len))
			else (case (getBuf buf)
			   of EOF => (empty, ISTRM(buf, len))
			    | (DATA rest) => get (ISTRM(rest, 0))
			  (* end case *))
		    end
	      in
		get
	      end

      (* terminate an input stream *)
	fun terminate (info as INFO{tail, cleanTag, ...}) = let
	      val m = SV.mGet tail
	      in
		case SV.mTake m
		 of (m' as MORE _) => (SV.mPut(m, m'); terminate info)
		  | TERMINATED => SV.mPut(m, TERMINATED)
		  | _ => (
		      CleanIO.removeCleaner cleanTag;
		      SV.mPut(m, TERMINATED))
	        (* end case *)
	      end

      (* find the end of the stream *)
	fun findEOS (buf as IBUF{more, data, ...}) = (case (SV.mGet more)
	       of (MORE buf) => findEOS buf
		| _ => ISTRM(buf, V.length data)
	      (* end case *))

	fun input (strm as ISTRM(buf, _)) =
	      generalizedInput (getBuffer (readVec buf, "input")) strm
	fun input1 (ISTRM(buf, pos)) = let
	      val IBUF{data, more, ...} = buf
	      in
		if (pos < V.length data)
		  then SOME(vecSub(data, pos), ISTRM(buf, pos+1))
		  else let
		    fun get (MORE buf) = input1 (ISTRM(buf, 0))
		      | get TERMINATED = NONE
		      | get NOMORE = (case SV.mTake more
			   of NOMORE => (
				case extendStream (readVec buf, "input1", buf)
				 of EOF => NONE
				  | (DATA rest) => input1 (ISTRM(rest, 0))
				(* end case *))
			    | next => (SV.mPut(more, next); get next)
			  (* end case *))
		    in
		      get (SV.mGet more)
		    end
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
	      and nextBuf (buf as IBUF{more, data, ...}, n) = let
		    fun get (MORE buf) = inputList (buf, 0, n)
		      | get TERMINATED = ([], ISTRM(buf, V.length data))
		      | get NOMORE = (case (SV.mTake more)
			   of NOMORE => (case extendStream (readVec buf, "inputN", buf)
				 of EOF => ([], ISTRM(buf, V.length data))
				  | (DATA rest) => inputList (rest, 0, n)
				(* end case *))
			    | next => (SV.mPut(more, next); get next)
			  (* end case *))
		    in
		      get (SV.mGet more)
		    end
	      val (data, strm) = inputList (buf, pos, n)
	      in
		(V.concat data, strm)
	      end

	fun inputAll (strm as ISTRM(buf, _)) = let
	      val INFO{reader=PIO.RD{avail, ...}, ...} = infoOfIBuf buf
 	    (* read a chunk that is as large as the available input.  Note
	     * that for systems that use CR-LF for #"\n", the size will be
	     * too large, but this should be okay.
	     *)
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
	      fun loop (v, strm) =
		    if (V.length v = 0) then [] else v :: loop(bigInput strm)
	      val data = V.concat (loop (bigInput strm))
	      in
		(data, findEOS buf)
	      end

      (* Return SOME k, if k <= amount characters can be read without blocking. *)
	fun canInput (strm as ISTRM(buf, pos), amount) = let
(******
	      val readVecNB = (case buf
		   of (IBUF{info as INFO{readVecNB=NONE, ...}, ...}) =>
			inputExn(info, "canInput", IO.NonblockingNotSupported)
		    | (IBUF{info=INFO{readVecNB=SOME f, ...}, ...}) => f
		  (* end case *))
******)
	      fun tryInput (buf as IBUF{data, ...}, i, n) = let
		    val len = V.length data
		    val remain = len - i
		    in
		      if (remain >= n)
			then SOME n
			else nextBuf (buf, n - remain)
		    end
	      and nextBuf (IBUF{more, ...}, n) = let
		    fun get (MORE buf) = tryInput (buf, 0, n)
		      | get TERMINATED = SOME(amount - n)
(******
		      | get NOMORE = (case SV.mTake more
			   of NOMORE => ((
				case extendStream (readVecNB, "canInput", buf)
				 of EOF => SOME(amount - n)
				  | (DATA b) => tryInput (b, 0, n)
				(* end case *))
				  handle IO.Io{cause=WouldBlock, ...} => SOME(amount - n))
			    | next => (SV.mPut(more, next); get next)
			  (* end case *))
******)
		      | get NOMORE = SOME(amount - n)
		    in
		      get (SV.mGet more)
		    end
	      in
		if (amount < 0)
		  then raise Size
		  else tryInput (buf, pos, amount)
	      end
      (* close an input stream given its info structure; we need this function
       * for the cleanup hook to avoid a space leak.
       *)
	fun closeInInfo (INFO{closed=ref true, ...}) = ()
	  | closeInInfo (info as INFO{closed, reader=PIO.RD{close, ...}, ...}) = (
(*** We need some kind of lock on the input stream to do this safely!!! ***)
	      terminate info;
	      closed := true;
	      close() handle ex => inputExn(info, "closeIn", ex))
	fun closeIn (ISTRM(buf, _)) = closeInInfo (infoOfIBuf buf)
	fun endOfStream (ISTRM(buf as IBUF{more, ...}, pos)) = (
	      case SV.mTake more
	       of (next as MORE _) => (SV.mPut(more, next); false)
		| next => let
		    val IBUF{data, info=INFO{closed, ...}, ...} = buf
		    in
		      if (pos = V.length data)
			then (case (next, !closed)
			    of (NOMORE, false) => (
			        case extendStream (readVec buf, "endOfStream", buf)
			         of EOF => true
				  | _ => false
				(* end case *))
			     | _ => (SV.mPut(more, next); true)
			  (* end case *))
			else (SV.mPut(more, next); false)
		    end
	      (* end case *))
	fun mkInstream' (reader, data) = let
	      val PIO.RD{readVec, readVecEvt, getPos, setPos, ...} = reader
	      val getPos = (case (getPos, setPos)
		     of (SOME f, SOME _) => (fn () => SOME(f()))
		      | _ => (fn () => NONE)
		    (* end case *))
	      val more = SV.mVarInit NOMORE
	      val closedFlg = ref false
	      val tag = CleanIO.addCleaner dummyCleaner
	      val info = INFO{
		      reader=reader, readVec=readVec, readVecEvt=readVecEvt,
		      closed = closedFlg, getPos = getPos,
		      tail = SV.mVarInit more, cleanTag = tag
		    }
(** What should we do about the position in this case ?? **)
(** Suggestion: When building a stream with supplied initial data,
 ** nothing can be said about the positions inside that initial
 ** data (who knows where that data even came from!).
 **)
	      val basePos = if (V.length data = 0) then getPos () else NONE
	      val buf = IBUF {
		      basePos = basePos, data = data,
		      info = info, more = more
		    }
	      val strm =  ISTRM(buf, 0)
	      in
		(tag, strm)
	      end
        fun mkInstream arg = let
	      val (tag, strm as ISTRM(IBUF{info, ...}, _)) = mkInstream' arg
	      in
		CleanIO.rebindCleaner (tag, fn () => closeInInfo info);
		strm
	      end
	fun getReader (ISTRM(buf, pos)) = let
	      val IBUF{data, info as INFO{reader, ...}, more, ...} = buf
	      fun getData more = (case SV.mGet more
		     of (MORE(IBUF{data, more=more', ...})) => data :: getData more'
		      | _ => []
		    (* end case *))
	      in
		terminate info;
		if (pos < V.length data)
		  then (
		      reader,
		      V.concat(vecExtract(data, pos, NONE) :: getData more)
		    )
		  else (reader, V.concat(getData more))
	      end

(*
      (** Position operations on instreams **)
	datatype in_pos = INP of {
	    base : pos,
	    offset : int,
	    info : info
	  }
*)

(*
	fun getPosIn (ISTRM(buf, pos)) = (case buf
	       of IBUF{basePos=NONE, info, ...} =>
		    inputExn (info, "getPosIn", IO.RandomAccessNotSupported)
		| IBUF{basePos=SOME p, info, ...} => INP{
		      base = p, offset = pos, info = info
		    }
	      (* end case *))
*)
(*
	fun filePosIn (INP{base, offset, ...}) =
	      Position.+(base, Position.fromInt offset)
*)
      (* Get the underlying file position of a stream *)
	fun filePosIn (ISTRM(buf, pos)) = (case buf
	       of IBUF{basePos=NONE, info, ...} =>
		    inputExn (info, "filePosIn", IO.RandomAccessNotSupported)
		| IBUF{basePos=SOME base, info, ...} => let
		    val INFO{reader=PIO.RD rd, readVec, ...} = info
		    in
		      case (#getPos rd, #setPos rd)
		       of (SOME getPos, SOME setPos) => let
			    val tmpPos = getPos()
			    fun readN 0 = ()
			      | readN n = (case V.length(readVec n)
				   of 0 => inputExn (
					  info, "filePosIn", Fail "bogus position")
				    | k => readN(n-k)
				  (* end case *))
			    in
			      setPos base;
			      readN pos;
			      getPos () before setPos tmpPos
			    end
			| _ => raise Fail "filePosIn: impossible"
		      (* end case *)
		    end
	      (* end case *))
(*
	fun setPosIn (pos as INP{info as INFO{reader, ...}, ...}) = let
	      val fpos = filePosIn pos
	      val (PIO.RD rd) = reader
	      in
		terminate info;
		valOf (#setPos rd) fpos;
		mkInstream (PIO.RD rd, NONE)
	      end
*)

      (** Text stream specific operations **)
	fun inputLine (ISTRM(buf as IBUF{data, ...}, pos)) = let
	      fun join (item, (list, strm)) = (item::list, strm)
	      fun nextBuf (isEmpty, buf as IBUF{more, data, ...}) = let
		    fun last () =
			  (if isEmpty then [] else ["\n"], ISTRM(buf, V.length data))
		    fun get (MORE buf) = scanData (buf, 0)
		      | get NOMORE = (case (SV.mTake more)
			       of NOMORE => (
				    case extendStream (readVec buf, "inputLine", buf)
				     of EOF => last ()
				      | (DATA rest) => scanData (rest, 0)
				    (* end case *))
				| next => (SV.mPut(more, next); get next)
			      (* end case *))
		      | get TERMINATED = last()
		    in
		      get (SV.mGet more)
		    end
	      and scanData (buf as IBUF{data, ...}, i) = let
		    val len = V.length data
		    fun scan j = if (j = len)
			    then join(vecExtract(data, i, NONE), nextBuf(false, buf))
			  else if (vecSub(data, j) = #"\n")
			    then ([vecExtract(data, i, SOME(j+1-i))], ISTRM(buf, j+1))
			    else scan (j+1)
		    in
		      scan i
		    end
	      val (data, strm) = if (V.length data = pos)
		    then nextBuf (true, buf)
		    else scanData (buf, pos)
	      val res_v = V.concat data
	      in
	         if V.length res_v = 0 then NONE else SOME (res_v, strm)
	      end

    (* IO event constructors:
     * We exploit the "functional" nature of stream IO to implement the event
     * constructors.  These constructors spawn a thread to do the operation
     * and and write the result in an iVar that serves as the synchronization
     * value.
     * NOTE: this implementation has the weakness that it prevents shutdown when
     * everything else is deadlocked, since the thread that is spawned to actually
     * do the IO could proceed.
     *)
	local
	  datatype 'a result = RES of 'a | EXN of exn
	  fun doInput inputOp = let
		fun input arg = RES(inputOp arg) handle ex => EXN ex
		in
		  fn arg => CML.guard (fn () => let
			val resV = SV.iVar()
			in
			  CML.spawn (fn () => SV.iPut(resV, input arg));
			  CML.wrap(SV.iGetEvt resV,
			    fn (RES x) => x | (EXN ex) => raise ex)
			end)
		end
	in
	val input1Evt = doInput input1
	val inputEvt = doInput input
	val inputNEvt = doInput inputN
	val inputAllEvt = doInput inputAll
	val inputLineEvt = doInput inputLine
	end (* local *)


      (*** Output streams ***)

      (* an output stream is implemented as a monitor using an mvar to
       * hold its data.
       *)
	datatype ostrm_info = OSTRM of {
	    buf : A.array,
	    pos : int ref,
	    closed : bool ref,
	    bufferMode : IO.buffer_mode ref,
	    writer : writer,
	    writeArr : AS.slice -> unit,
	    writeVec : VS.slice -> unit,
	    cleanTag : CleanIO.tag
	  }

	type outstream = ostrm_info SV.mvar

	fun isNL #"\n" = true
	  | isNL _ = false

	fun isLineBreak (OSTRM{bufferMode, ...}) =
	      if (!bufferMode = IO.LINE_BUF) then isNL else (fn _ => false)

	fun outputExn (OSTRM{writer=PIO.WR{name, ...}, ...}, mlOp, exn) =
	      raise IO.Io{function=mlOp, name=name, cause=exn}

      (* lock access to the stream and make sure that it is not closed. *)
	fun lockAndChkClosedOut (strmMV, mlOp) = (case SV.mTake strmMV
	       of (strm as OSTRM({closed=ref true, ...})) => (
		    SV.mPut (strmMV, strm);
		    outputExn (strm, mlOp, IO.ClosedStream))
		| strm => strm
	      (* end case *))

	fun flushBuffer (strmMV, strm as OSTRM{buf, pos, writeArr, ...}, mlOp) = (
	      case !pos
	       of 0 => ()
		| n => ((
		    writeArr (AS.slice (buf, 0, SOME n)); pos := 0)
		      handle ex => (
			SV.mPut(strmMV, strm); outputExn (strm, mlOp, ex)))
	      (* end case *))

      (* A version of copyVec that checks for newlines, while it is copying.
       * This is used for LINE_BUF output of strings and substrings.
       *)
	fun lineBufCopyVec (src, srcI, srcLen, dst, dstI) = let
	      val stop = srcI+srcLen
	      fun cpy (srcI, dstI, lb) =
		    if (srcI < stop)
		      then let val c = vecSub(src, srcI)
			in
			  arrUpdate (dst, dstI, c);
			  cpy (srcI+1, dstI+1, lb orelse isNL c)
			end
		      else lb
	      in
		cpy (srcI, dstI, false)
	      end

      (* a version of copyVec for BLOCK_BUF output of strings and substrings. *)
	fun blockBufCopyVec (src, srcI, srcLen, dst, dstI) = (
	      AS.copyVec {
                  src = VS.slice (src, srcI, SOME srcLen),
		  dst = dst, di = dstI };
	      false)

	fun output (strmMV, v) = let
	      val (strm as OSTRM os) = lockAndChkClosedOut (strmMV, "output")
	      fun release () = SV.mPut (strmMV, strm)
	      val {buf, pos, bufferMode, ...} = os
	      fun flush () = flushBuffer (strmMV, strm, "output")
	      fun flushAll () = (#writeArr os (AS.full buf)
		    handle ex => (release(); outputExn (strm, "output", ex)))
	      fun writeDirect () = (
		    case !pos
		     of 0 => ()
		      | n => (#writeArr os (AS.slice (buf, 0, SOME n)); pos := 0)
		    (* end case *);
		    #writeVec os (VS.full v))
		      handle ex => (release(); outputExn (strm, "output", ex))
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
			      then let
				val _ =
				    AS.copyVec
					{ src = VS.slice (v, 0, SOME avail),
					  dst = buf, di = i }
				val _ = flushAll()
				val needsFlush = copyVec(v, avail, dataLen-avail, buf, 0)
				in
			  	  pos := dataLen-avail;
				  if needsFlush then flush () else ()
				end
			    else let
			      val needsFlush = copyVec(v, 0, dataLen, buf, i)
			      in
			        pos := i + dataLen;
			        if (needsFlush orelse (avail = dataLen))
				  then flush()
				  else ()
			      end
			  end
		    end
	      in
		case !bufferMode
		 of IO.NO_BUF => writeDirect ()
		  | IO.LINE_BUF => insert lineBufCopyVec
		  | IO.BLOCK_BUF => insert blockBufCopyVec
		(* end case *);
		release()
	      end

	fun output1 (strmMV, elem) = let
	      val (strm as OSTRM{buf, pos, bufferMode, writeArr, ...}) =
		    lockAndChkClosedOut (strmMV, "output1")
	      fun release () = SV.mPut (strmMV, strm)
	      in
		case !bufferMode
		 of IO.NO_BUF => (
		      arrUpdate (buf, 0, elem);
		      writeArr (AS.slice (buf, 0, SOME 1))
			handle ex => (release(); outputExn (strm, "output1", ex)))
		  | IO.LINE_BUF => let val i = !pos val i' = i+1
		      in
			arrUpdate (buf, i, elem); pos := i';
			if ((i' = A.length buf) orelse (isNL elem))
			  then flushBuffer (strmMV, strm, "output1")
			  else ()
		      end
		  | IO.BLOCK_BUF => let val i = !pos val i' = i+1
		      in
			arrUpdate (buf, i, elem); pos := i';
			if (i' = A.length buf)
			  then flushBuffer (strmMV, strm, "output1")
			  else ()
		      end
		(* end case *);
		release()
	      end

	fun flushOut strmMV = let
	      val strm = lockAndChkClosedOut (strmMV, "flushOut")
	      in
		flushBuffer (strmMV, strm, "flushOut");
		SV.mPut (strmMV, strm)
	      end

	fun closeOut strmMV = let
	      val (strm as OSTRM{writer=PIO.WR{close, ...}, closed, cleanTag, ...}) =
		    SV.mTake strmMV
	      in
		if !closed
		  then ()
		  else (
		    flushBuffer (strmMV, strm, "closeOut");
		    closed := true;
		    CleanIO.removeCleaner cleanTag;
		    close());
		SV.mPut (strmMV, strm)
	      end

	fun mkOutstream' (wr as PIO.WR{chunkSize, writeArr, writeVec, ...}, mode) =
	      let fun iterate (f, size, subslice) = let
		      fun lp sl =
			  if size sl = 0 then ()
			  else let val n = f sl
			       in
				   lp (subslice (sl, n, NONE))
			       end
		  in
		      lp
		  end
	      val writeArr' = iterate (writeArr, AS.length, AS.subslice)
	      val writeVec' = iterate (writeVec, VS. length, VS.subslice)
	    (* install a dummy cleaner *)
	      val tag = CleanIO.addCleaner dummyCleaner
	      val strm = SV.mVarInit (OSTRM{
		      buf = A.array(chunkSize, someElem),
		      pos = ref 0,
		      closed = ref false,
		      bufferMode = ref mode,
		      writer = wr,
		      writeArr = writeArr',
		      writeVec = writeVec',
		      cleanTag = tag
		    })
	      in
		(tag, strm)
	      end
	fun mkOutstream arg = let
	      val (tag, strm) = mkOutstream' arg
	      in
		CleanIO.rebindCleaner (tag, fn () => closeOut strm);
		strm
	      end

	fun getWriter strmMV = let
	      val (strm as OSTRM{writer, bufferMode, ...}) =
		    lockAndChkClosedOut (strmMV, "getWriter")
	      in
		(writer, !bufferMode) before SV.mPut(strmMV, strm)
	      end

      (** Position operations on outstreams **)
	datatype out_pos = OUTP of {
	    pos : PIO.pos,
	    strm : outstream
	  }

	fun getPosOut strmMV = let
	      val (strm as OSTRM{writer, ...}) =
		    lockAndChkClosedOut (strmMV, "getWriter")
	      fun release () = SV.mPut(strmMV, strm)
	      in
		flushBuffer (strmMV, strm, "getPosOut");
		case writer
	 	 of PIO.WR{getPos=SOME f, ...} => (
		      OUTP{pos = f(), strm = strmMV}
		        handle ex => (release(); outputExn(strm, "getPosOut", ex)))
		  | _ => (
		      release();
		      outputExn(strm, "getPosOut", IO.RandomAccessNotSupported))
		(* end case *)
		before release()
	      end
	fun filePosOut (OUTP{pos, strm=strmMV}) = (
	      SV.mPut (strmMV, lockAndChkClosedOut (strmMV, "filePosOut"));
	      pos)
	fun setPosOut (OUTP{pos, strm=strmMV}) = let
	      val (strm as OSTRM{writer, ...}) =
		    lockAndChkClosedOut (strmMV, "setPosOut")
	      fun release () = SV.mPut(strmMV, strm)
	      in
		case writer
		 of PIO.WR{setPos=SOME f, ...} => (
		      (f pos)
			handle ex => (release(); outputExn(strm, "setPosOut", ex)))
		  | _ => (
		      release();
		      outputExn(strm, "getPosOut", IO.RandomAccessNotSupported))
		(* end case *);
		release()
	      end

	fun setBufferMode (strmMV, mode) = let
	      val (strm as OSTRM{bufferMode, ...}) =
		    lockAndChkClosedOut (strmMV, "setBufferMode")
	      in
		if (mode = IO.NO_BUF)
		  then flushBuffer (strmMV, strm, "setBufferMode")
		  else ();
		bufferMode := mode;
		SV.mPut (strmMV, strm)
	      end
	fun getBufferMode strmMV = let
(** should we be checking for closed streams here??? **)
	      val (strm as OSTRM{bufferMode, ...}) =
		    lockAndChkClosedOut (strmMV, "getBufferMode")
	      in
		!bufferMode before SV.mPut (strmMV, strm)
	      end

      (** Text stream specific operations **)
	fun outputSubstr (strmMV, ss) = let
	      val (strm as OSTRM os) = lockAndChkClosedOut (strmMV, "outputSubstr")
	      fun release () = SV.mPut (strmMV, strm)
	      val (v, dataStart, dataLen) = substringBase ss
	      val {buf, pos, bufferMode, ...} = os
	      val bufLen = A.length buf
	      fun flush () = flushBuffer (strmMV, strm, "outputSubstr")
	      fun flushAll () = (#writeArr os (AS.full buf)
		    handle ex => (release(); outputExn (strm, "outputSubstr", ex)))
	      fun writeDirect () = (
		    case !pos
		     of 0 => ()
		      | n => (#writeArr os (AS.slice (buf, 0, SOME n)); pos := 0)
		    (* end case *);
		    #writeVec os (VS.slice (v, dataStart, SOME dataLen)))
		      handle ex => (release(); outputExn (strm, "outputSubstr", ex))
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
			      then let
				val _ =
				    AS.copyVec { src = VS.slice (v, dataStart,
								 SOME avail),
						 dst = buf, di = i }
				val _ = flushAll()
				val needsFlush = copyVec(v, avail, dataLen-avail, buf, 0)
				in
			  	  pos := dataLen-avail;
				  if needsFlush then flush () else ()
				end
			    else let
			      val needsFlush = copyVec(v, dataStart, dataLen, buf, i)
			      in
			        pos := i + dataLen;
			        if (needsFlush orelse (avail = dataLen))
				  then flush()
				  else ()
			      end
			  end
		    end
	      in
		case !bufferMode
		 of IO.NO_BUF => writeDirect ()
		  | IO.LINE_BUF => insert lineBufCopyVec
		  | IO.BLOCK_BUF => insert blockBufCopyVec
		(* end case *);
		release()
	      end

      end (* StreamIO *)

    type vector = V.vector
    type elem = V.elem
    type instream = StreamIO.instream SV.mvar
    type outstream = StreamIO.outstream SV.mvar

  (** Input operations **)
    fun input strm = let val (v, strm') = StreamIO.input(SV.mTake strm)
	  in
	    SV.mPut (strm, strm'); v
	  end
    fun input1 strm = (case StreamIO.input1(SV.mTake strm)
	   of NONE => NONE
	    | (SOME(elem, strm')) => (SV.mPut (strm, strm'); SOME elem)
	  (* end case *))
    fun inputN (strm, n) = let val (v, strm') = StreamIO.inputN (SV.mTake strm, n)
	  in
	    SV.mPut (strm, strm'); v
	  end
    fun inputAll (strm : instream) = let
	  val (v, strm') = StreamIO.inputAll(SV.mTake strm)
	  in
	    SV.mPut (strm, strm'); v
	  end

  (* event-value constructors *)
    local
      datatype 'a result = RES of 'a | EXN of exn
      fun sendEvt (ch, v) = CML.sendEvt(ch, RES v)
      fun sendExnEvt (ch, exn) = CML.sendEvt(ch, EXN exn)
      fun recvEvt ch =
	    CML.wrap(CML.recvEvt ch, fn (RES v) => v | (EXN exn) => raise exn)
      fun doInput inputEvt (strm : instream) nack = let
	    val replyCh = CML.channel()
	    fun inputThread () = let
		  val strm' = SV.mTake strm
		  val nackEvt = CML.wrap(nack, fn _ => SV.mPut(strm, strm'))
		  fun handleInput (result, strm'') = CML.select [
			  CML.wrap (sendEvt(replyCh, result),
			    fn _ => SV.mPut(strm, strm'')),
			  nackEvt
			]
		  in
		    (CML.select [
			CML.wrap (inputEvt strm', handleInput),
			nackEvt
		      ]) handle exn => CML.select [
			  CML.wrap (sendExnEvt(replyCh, exn),
			    fn _ => SV.mPut(strm, strm')),
			  nackEvt
			]
		  end
	    in
	      ignore (CML.spawn inputThread);
	      recvEvt replyCh
	    end
    in
    fun input1Evt (strm : instream) = let
	  fun inputEvt (strm : StreamIO.instream) = CML.wrap (
		StreamIO.input1Evt strm,
		fn NONE => (NONE, strm) | SOME(s, strm') => (SOME s, strm'))
	  in
	    CML.withNack (doInput inputEvt strm)
	  end
    fun inputEvt strm = CML.withNack (doInput StreamIO.inputEvt strm)
    fun inputNEvt (strm, n) =
	  CML.withNack (doInput (fn strm' => StreamIO.inputNEvt(strm', n)) strm)
    fun inputAllEvt Strm = CML.withNack (doInput StreamIO.inputAllEvt Strm)
    end (* local *)

    fun canInput (strm, n) = StreamIO.canInput (SV.mGet strm, n)
    fun lookahead (strm : instream) = (case StreamIO.input1(SV.mGet strm)
	   of NONE => NONE
	    | (SOME(elem, _)) => SOME elem
	  (* end case *))
    fun closeIn strm = let
	  val (s as StreamIO.ISTRM(buf as StreamIO.IBUF{data, ...}, _)) =
		SV.mTake strm
	  in
	    StreamIO.closeIn s;
	    SV.mPut(strm, StreamIO.findEOS buf)
	  end
    fun endOfStream strm = StreamIO.endOfStream(SV.mGet strm)
(*
    fun getPosIn strm = StreamIO.getPosIn(SV.mGet strm)
    fun setPosIn (strm, p) = mUpdate(strm, StreamIO.setPosIn p)
*)

  (** Output operations **)
    fun output (strm, v) = StreamIO.output(SV.mGet strm, v)
    fun output1 (strm, c) = StreamIO.output1(SV.mGet strm, c)
    fun flushOut strm = StreamIO.flushOut(SV.mGet strm)
    fun closeOut strm = StreamIO.closeOut(SV.mGet strm)
    fun getPosOut strm = StreamIO.getPosOut(SV.mGet strm)
    fun setPosOut (strm, p as StreamIO.OUTP{strm=strm', ...}) = (
	  mUpdate(strm, strm'); StreamIO.setPosOut p)

    fun mkInstream (strm : StreamIO.instream) = SV.mVarInit strm
    fun getInstream (strm : instream) = SV.mGet strm
    fun setInstream (strm : instream, strm') = mUpdate(strm, strm')

    fun mkOutstream (strm : StreamIO.outstream) = SV.mVarInit strm
    fun getOutstream (strm : outstream) = SV.mGet strm
    fun setOutstream (strm : outstream, strm') = mUpdate(strm, strm')

  (* figure out the proper buffering mode for a given writer *)
    fun bufferMode (PIO.WR{ioDesc=NONE, ...}) = IO.BLOCK_BUF
      | bufferMode (PIO.WR{ioDesc=SOME iod, ...}) =
	  if (OS.IO.kind iod = OS.IO.Kind.tty) then IO.LINE_BUF else IO.BLOCK_BUF

  (** Open files **)
    fun openIn fname =
	  mkInstream(StreamIO.mkInstream(OSPrimIO.openRd fname, empty))
	    handle ex => raise IO.Io{function="openIn", name=fname, cause=ex}
    fun openOut fname = let
	  val wr = OSPrimIO.openWr fname
	  in
	    mkOutstream(StreamIO.mkOutstream(wr, bufferMode wr))
	      handle ex => raise IO.Io{function="openOut", name=fname, cause=ex}
	  end
    fun openAppend fname =
	  mkOutstream(StreamIO.mkOutstream(OSPrimIO.openApp fname, IO.NO_BUF))
	    handle ex => raise IO.Io{function="openAppend", name=fname, cause=ex}

  (** Text stream specific operations **)
    fun inputLine strm =
	Option.map (fn (s, strm') => (SV.mPut (strm, strm'); s))
	           (StreamIO.inputLine (SV.mTake strm))
    fun outputSubstr (strm, ss) = StreamIO.outputSubstr (SV.mGet strm, ss)
    fun openString src =
	  mkInstream(StreamIO.mkInstream(OSPrimIO.strReader src, empty))
	    handle ex => raise IO.Io{function="openIn", name="<string>", cause=ex}

    structure ChanIO = ChanIOFn(
      structure PrimIO = PIO
      structure V = CharVector
      structure A = CharArray
      structure VS = CharVectorSlice
      structure AS = CharArraySlice)

  (* open an instream that is connected to the output port of a channel. *)
    fun openChanIn ch =
	  mkInstream(StreamIO.mkInstream(ChanIO.mkReader ch, empty))

  (* open an outstream that is connected to the input port of a channel. *)
    fun openChanOut ch =
	  mkOutstream(StreamIO.mkOutstream(ChanIO.mkWriter ch, IO.NO_BUF))

  (** Standard streams **)
    local
      structure SIO = StreamIO
      fun mkStdIn rebind = let
	    val (tag, strm) = SIO.mkInstream'(OSPrimIO.stdIn(), empty)
	    in
	      if rebind
		then CleanIO.rebindCleaner (tag, dummyCleaner)
		else ();
	      strm
	    end
      fun mkStdOut rebind = let
	    val wr = OSPrimIO.stdOut()
	    val (tag, strm) = SIO.mkOutstream'(wr, bufferMode wr)
	    in
	      if rebind
		then CleanIO.rebindCleaner (tag, fn () => SIO.flushOut strm)
		else ();
	      strm
	    end
      fun mkStdErr rebind = let
	    val (tag, strm) = SIO.mkOutstream'(OSPrimIO.stdErr(), IO.NO_BUF)
	    in
	      if rebind
		then CleanIO.rebindCleaner (tag, fn () => SIO.flushOut strm)
		else ();
	      strm
	    end
    in
  (* build the standard streams.  Since we are not currently running CML, we
   * cannot do the cleaner rebinding here, but that is okay, since these are
   * just place holders.
   *)
    val stdIn = mkInstream(mkStdIn false)
    val stdOut = mkOutstream(mkStdOut false)
    val stdErr = mkOutstream(mkStdErr false)

    fun print s = let val strm' = SV.mTake stdOut
	  in
	    StreamIO.output (strm', s); StreamIO.flushOut strm';
	    SV.mPut(stdOut, strm')
	  end

    fun scanStream scanFn = let
	  val scan = scanFn StreamIO.input1
	  fun doit strm = let
		val instrm = getInstream strm
		in
		  case scan instrm
		   of NONE => NONE
		    | SOME(item, instrm') => (
			setInstream(strm, instrm');
			SOME item)
		  (* end case *)
		end
	  in
	    doit
	  end

  (* Establish a hook function to rebuild the I/O stack *)
    val _ = CleanIO.stdStrmHook := (fn () => (
	  setInstream (stdIn, mkStdIn true);
	  setOutstream (stdOut, mkStdOut true);
	  setOutstream (stdErr, mkStdErr true);
	  SMLofNJ.Internals.prHook := print))
    end (* local *)

  end (* TextIOFn *)
