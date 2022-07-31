(* posix-io.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Structure for POSIX 1003.1 primitive I/O operations
 *)

local
  structure SysWord = SysWordImp
  structure Int = IntImp
  structure Position = PositionImp
in
structure POSIX_IO =
  struct

    structure FS = POSIX_FileSys

    datatype open_mode = datatype FS.open_mode (* O_RDONLY | O_WRONLY | O_RDWR  *)

    type s_word = SysWord.word
    type s_int = SysInt.int	(* == Int.int *)

    val ++ = SysWord.orb
    val & = SysWord.andb
    infix ++ &

    fun cfun x = CInterface.c_function "POSIX-IO" x
    val osval : string -> s_int = cfun "osval"
    val w_osval = SysWord.fromInt o osval
    fun fail (fct, msg) = raise Fail(concat["POSIX_IO.", fct, ": ", msg])

    type file_desc = FS.file_desc
    type pid = POSIX_Process.pid

    val pipe' : unit -> s_int * s_int = cfun "pipe"
    fun pipe () = let
          val (ifd, ofd) = pipe' ()
          in
            {infd = FS.fd ifd, outfd = FS.fd ofd}
          end

    val dup' : s_int -> s_int = cfun "dup"
    val dup2' : s_int * s_int -> unit = cfun "dup2"
    fun dup fd = FS.fd(dup' (FS.intOf fd))
    fun dup2 {old, new} = dup2'(FS.intOf old, FS.intOf new)

    val close' : s_int -> unit = cfun "close"
    fun close fd = close' (FS.intOf fd)

    val read' : int * int -> Word8Vector.vector = cfun "read"
    val readbuf' : int * Word8Array.array * int * int -> int = cfun "readbuf"
    fun readArr (fd, asl) = let
	val (buf, i, len) = Word8ArraySlice.base asl
    in
	readbuf' (FS.intOf fd, buf, len, i)
    end
    fun readVec (fd,cnt) =
          if cnt < 0 then raise Size else read'(FS.intOf fd, cnt)

    val writevec' : (int * Word8Vector.vector * int * int) -> int = cfun "writebuf"
    val writearr' : (int * Word8Array.array * int * int) -> int = cfun "writebuf"
    fun writeArr (fd, asl) = let
	val (buf, i, len) = Word8ArraySlice.base asl
    in
	writearr' (FS.intOf fd, buf, len, i)
    end

    fun writeVec (fd, vsl) = let
	val (buf, i, len) = Word8VectorSlice.base vsl
    in
	writevec' (FS.intOf fd, buf, len, i)
    end

    datatype whence = SEEK_SET | SEEK_CUR | SEEK_END
    val seek_set = osval "SEEK_SET"
    val seek_cur = osval "SEEK_CUR"
    val seek_end = osval "SEEK_END"
    fun whToWord SEEK_SET = seek_set
      | whToWord SEEK_CUR = seek_cur
      | whToWord SEEK_END = seek_end
    fun whFromWord wh =
          if wh = seek_set then SEEK_SET
          else if wh = seek_cur then SEEK_CUR
          else if wh = seek_end then SEEK_END
          else fail ("whFromWord","unknown whence "^(Int.toString wh))

    structure FD =
      struct
        local structure BF = BitFlagsFn ()
	in
	    open BF
	end

        val cloexec = fromWord (w_osval "cloexec")
      end

    structure O =
      struct
        local structure BF = BitFlagsFn ()
	in
	    open BF
	end

        val append   = fromWord (w_osval "append")
        val dsync    = fromWord (w_osval "dsync")
        val nonblock = fromWord (w_osval "nonblock")
        val rsync    = fromWord (w_osval "rsync")
        val sync     = fromWord (w_osval "sync")
      end

    val fcntl_d   : s_int * s_int -> s_int = cfun "fcntl_d"
    val fcntl_gfd : s_int -> s_word = cfun "fcntl_gfd"
    val fcntl_sfd : (s_int * s_word) -> unit = cfun "fcntl_sfd"
    val fcntl_gfl : s_int -> (s_word * s_word) = cfun "fcntl_gfl"
    val fcntl_sfl : (s_int * s_word) -> unit = cfun "fcntl_sfl"
    fun dupfd {old, base} = FS.fd (fcntl_d (FS.intOf old, FS.intOf base))
    fun getfd fd = FD.fromWord (fcntl_gfd (FS.intOf fd))
    fun setfd (fd, fl) = fcntl_sfd(FS.intOf fd, FD.toWord fl)
    fun getfl fd = let
          val (sts, omode) = fcntl_gfl (FS.intOf fd)
          in
            (O.fromWord sts, FS.omodeFromWord omode)
          end
    fun setfl (fd, sts) = fcntl_sfl (FS.intOf fd, O.toWord sts)

    datatype lock_type = F_RDLCK | F_WRLCK | F_UNLCK

    structure FLock =
      struct
        datatype flock = FLOCK of {
             ltype : lock_type,
             whence : whence,
             start : Position.int,
             len : Position.int,
             pid : pid option
           }

        fun flock fv = FLOCK fv
        fun ltype (FLOCK fv) = #ltype fv
        fun whence (FLOCK fv) = #whence fv
        fun start (FLOCK fv) = #start fv
        fun len (FLOCK fv) = #len fv
        fun pid (FLOCK fv) = #pid fv
      end

    type flock_rep = s_int * s_int * Position.int * Position.int * s_int

    val fcntl_l : s_int * s_int * flock_rep -> flock_rep = cfun "fcntl_l"
    val f_getlk = osval "F_GETLK"
    val f_setlk = osval "F_SETLK"
    val f_setlkw = osval "F_SETLKW"
    val f_rdlck = osval "F_RDLCK"
    val f_wrlck = osval "F_WRLCK"
    val f_unlck = osval "F_UNLCK"

    fun flockToRep (FLock.FLOCK{ltype, whence, start, len,...}) = let
          fun ltypeOf F_RDLCK = f_rdlck
            | ltypeOf F_WRLCK = f_wrlck
            | ltypeOf F_UNLCK = f_unlck
          in
            (ltypeOf ltype,whToWord whence, start, len, 0)
          end
    fun flockFromRep (usepid,(ltype,whence,start,len,pid)) = let
          fun ltypeOf ltype =
                if ltype = f_rdlck then F_RDLCK
                else if ltype = f_wrlck then F_WRLCK
                else if ltype = f_unlck then F_UNLCK
                else fail ("flockFromRep","unknown lock type "^(Int.toString ltype))
          in
            FLock.FLOCK {
              ltype = ltypeOf ltype,
              whence = whFromWord whence,
              start = start,
              len = len,
              pid = if usepid then SOME(POSIX_Process.PID pid) else NONE
            }
          end

    fun getlk (fd, flock) =
          flockFromRep(true,fcntl_l(FS.intOf fd,f_getlk,flockToRep flock))
    fun setlk (fd, flock) =
          flockFromRep(false,fcntl_l(FS.intOf fd,f_setlk,flockToRep flock))
    fun setlkw (fd, flock) =
          flockFromRep(false,fcntl_l(FS.intOf fd,f_setlkw,flockToRep flock))

    val lseek' : s_int * Position.int * s_int -> Position.int = cfun "lseek"
    fun lseek (fd, offset, whence) = lseek'(FS.intOf fd, offset, whToWord whence)

    val fsync' : s_int -> unit = cfun "fsync"
    fun fsync fd = fsync' (FS.intOf fd)


    (*
     * Making readers and writers...
     *   (code lifted from posix-bin-prim-io.sml and posix-text-prim-io.sml)
     *)
    fun announce s x y = (
	  (*print "Posix: "; print (s:string); print "\n"; *)
	  x y)

    val bufferSzB = 4096

    fun isRegFile fd = FS.ST.isReg(FS.fstat fd)

    fun posFns (closed, fd) =
	if isRegFile fd then
	    let val pos = ref (Position.fromInt 0)
		fun getPos () = !pos
		fun setPos p =
		    (if !closed then raise IO.ClosedStream else ();
		     pos := announce "lseek" lseek (fd, p, SEEK_SET))
		fun endPos () =
		    (if !closed then raise IO.ClosedStream else ();
		     FS.ST.size(announce "fstat" FS.fstat fd))
		fun verifyPos () =
		    let val curPos = lseek (fd, Position.fromInt 0, SEEK_CUR)
		    in
			pos := curPos; curPos
		    end
	    in
		ignore (verifyPos ());
		{ pos = pos,
		  getPos = SOME getPos,
		  setPos = SOME setPos,
		  endPos = SOME endPos,
		  verifyPos = SOME verifyPos }
	    end
	else { pos = ref (Position.fromInt 0),
	       getPos = NONE, setPos = NONE, endPos = NONE, verifyPos = NONE }

    fun mkReader { mkRD, cvtVec, cvtArrSlice } { fd, name, initBlkMode } = let
	  val closed = ref false
	  val {pos, getPos, setPos, endPos, verifyPos} = posFns (closed, fd)
	  val blocking = ref initBlkMode
	  fun blockingOn () = (setfl(fd, O.flags[]); blocking := true)
	  fun blockingOff () = (setfl(fd, O.nonblock); blocking := false)
	  fun incPos k = pos := Position.+(!pos, Position.fromInt k)
	  fun r_readVec n = let
		val v = announce "read" readVec(fd, n)
		in
		  incPos (Word8Vector.length v);
		  cvtVec v
		end
	  fun r_readArr arg = let
		val k = announce "readBuf" readArr(fd, cvtArrSlice arg)
		in
		  incPos k; k
		end
	  fun blockWrap f x = (
		if !closed then raise IO.ClosedStream else ();
		if !blocking then () else blockingOn();
		f x)
	  fun noBlockWrap f x = (
		if !closed then raise IO.ClosedStream else ();
		if !blocking then blockingOff() else ();
		((* try *) SOME (f x)
			   handle (e as Assembly.SysErr(_, SOME cause)) =>
				  if cause = POSIX_Error.again then NONE
				  else raise e
		 (* end try *)))
	  fun r_close () = if !closed
		then ()
		else (closed:=true; announce "close" close fd)
	  val isReg = isRegFile fd
	  fun avail () = if !closed
		  then SOME 0
		else if isReg
		  then SOME(FS.ST.size(FS.fstat fd) - !pos)
		  else NONE
	  in
	    mkRD {
		name = name,
		chunkSize = bufferSzB,
		readVec = SOME (blockWrap r_readVec),
		readArr = SOME (blockWrap r_readArr),
		readVecNB = SOME (noBlockWrap r_readVec),
		readArrNB = SOME (noBlockWrap r_readArr),
		block = NONE,
		canInput = NONE,
		avail = avail,
		getPos = getPos,
		setPos = setPos,
		endPos = endPos,
		verifyPos = verifyPos,
		close = r_close,
		ioDesc = SOME (FS.fdToIOD fd)
	      }
	  end

    fun mkWriter { mkWR, cvtVecSlice, cvtArrSlice }
		 { fd, name, initBlkMode, appendMode, chunkSize } =
	let val closed = ref false
            val {pos, getPos, setPos, endPos, verifyPos} = posFns (closed, fd)
	    fun incPos k = (pos := Position.+(!pos, Position.fromInt k); k)
	    val blocking = ref initBlkMode
	    val appendFS = O.flags(if appendMode then [O.append] else nil)
	    fun updateStatus() =
		let val flgs = if !blocking then appendFS
			       else O.flags[O.nonblock, appendFS]
		in
		    announce "setfl" setfl(fd, flgs)
		end
	  fun ensureOpen () = if !closed then raise IO.ClosedStream else ()
	  fun ensureBlock (x) =
	      if !blocking = x then () else (blocking := x; updateStatus())
	  fun writeVec' (fd, s) = writeVec (fd, cvtVecSlice s)
	  fun writeArr' (fd, s) = writeArr (fd, cvtArrSlice s)
	  fun putV x = incPos (announce "writeVec" writeVec' x)
	  fun putA x = incPos (announce "writeArr" writeArr' x)
	  fun write (put, block) arg =
	      (ensureOpen();
	       ensureBlock block;
	       put(fd, arg))
	  fun handleBlock writer arg =
	      SOME (writer arg)
	      handle (e as Assembly.SysErr(_, SOME cause)) =>
 		     if cause = POSIX_Error.again then NONE else raise e
	  fun w_close () =
	      if !closed then ()
	      else (closed:=true; announce "close" close fd)
	in
	    mkWR { name = name,
		   chunkSize = chunkSize,
		   writeVec = SOME(write(putV,true)),
		   writeArr = SOME(write(putA,true)),
		   writeVecNB = SOME(handleBlock(write(putV,false))),
		   writeArrNB = SOME(handleBlock(write(putA,false))),
		   block = NONE,
		   canOutput = NONE,
		   getPos = getPos,
		   setPos = setPos,
		   endPos = endPos,
		   verifyPos = verifyPos,
		   ioDesc = SOME (FS.fdToIOD fd),
		   close = w_close }
	end

    local
      fun c2w_vs cvs = let
	    val (cv, s, l) = CharVectorSlice.base cvs
	    val wv = Byte.stringToBytes cv
	    in
	      Word8VectorSlice.slice (wv, s, SOME l)
	    end

      (* hack!!!  This only works because CharArray.array and
       *          Word8Array.array are really the same internally. *)
      val c2w_a : CharArray.array -> Word8Array.array = InlineT.cast

      fun c2w_as cas = let
	    val (ca, s, l) = CharArraySlice.base cas
	    val wa = c2w_a ca
	    in
	      Word8ArraySlice.slice (wa, s, SOME l)
	    end
    in

    val mkBinReader = mkReader { mkRD = BinPrimIO.RD,
				 cvtVec = fn v => v,
				 cvtArrSlice = fn s => s }

    val mkTextReader = mkReader { mkRD = TextPrimIO.RD,
				  cvtVec = Byte.bytesToString,
				  cvtArrSlice =	c2w_as }

    val mkBinWriter = mkWriter { mkWR = BinPrimIO.WR,
				 cvtVecSlice = fn s => s,
				 cvtArrSlice = fn s => s }

    val mkTextWriter = mkWriter { mkWR = TextPrimIO.WR,
				  cvtVecSlice =	c2w_vs,
				  cvtArrSlice = c2w_as }

    end (* local *)

  end (* structure POSIX_IO *)

end (* local *)
