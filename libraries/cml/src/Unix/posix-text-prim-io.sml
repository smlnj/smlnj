(* posix-text-prim-io.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This implements the UNIX version of the OS specific text primitive
 * IO structure.  It is implemented by a trivial translation of the
 * binary operations (see posix-bin-prim-io.sml).
 *)

structure PosixTextPrimIO : sig

    include OS_PRIM_IO

    val stdIn  : unit -> PrimIO.reader
    val stdOut : unit -> PrimIO.writer
    val stdErr : unit -> PrimIO.writer

    val strReader : string -> PrimIO.reader

  end = struct

    structure SV = SyncVar
    structure PF = Posix.FileSys
    structure BinPrimIO = PosixBinPrimIO
    structure PrimIO = TextPrimIO

    type file_desc = PF.file_desc

    val bufferSzB = 4096

 (* If Char.char is really Word8.word, then very efficient versions of
  * translateIn and translateOut are possible:
  *)
    val translateIn : BinPrimIO.PrimIO.reader -> PrimIO.reader = Unsafe.cast
    val translateOut : BinPrimIO.PrimIO.writer -> PrimIO.writer = Unsafe.cast

    fun openRd fname = translateIn(BinPrimIO.openRd fname)
    fun openWr fname = translateOut(BinPrimIO.openWr fname)
    fun openApp fname = translateOut(BinPrimIO.openApp fname)

    fun mkReader args = translateIn(BinPrimIO.mkReader args)
    fun mkWriter args = translateOut(BinPrimIO.mkWriter args)

    fun stdIn () = mkReader{
	    fd		= PF.stdin,
	    name	= "<stdIn>"
	  }

    fun stdOut () = mkWriter{
	    fd		= PF.stdout,
	    name	= "<stdOut>",
	    appendMode	= false, (* Bug!  Should check! *)
	    chunkSize	= bufferSzB
	  }

    fun stdErr () = mkWriter{
	    fd		= PF.stderr,
	    name	= "<stdErr>",
	    appendMode	= false, (* Bug!  Should check! *)
	    chunkSize	= bufferSzB
	  }

    fun strReader src = let
	  val lockMV = SV.mVarInit()
	  fun withLock f x = (
		SV.mTake lockMV;
		f x before SV.mPut(lockMV, ()))
		  handle ex => (SV.mPut(lockMV, ()); raise ex)
	  val pos = ref 0
	  val closed = ref false
	  fun checkClosed () = if !closed then raise IO.ClosedStream else ()
	  val len = String.size src
	  val plen = Position.fromInt len
	  fun avail () = Position.fromInt(len - !pos)
	  fun readV n = let
		val p = !pos
		val m = Int.min(n, len-p)
		in
		  checkClosed ();
		  pos := p+m;
(** NOTE: could use unchecked operations here **)
		  String.substring (src, p, m)
		end
	  fun readA asl = let
		val p = !pos
		val (buf, i, n) = CharArraySlice.base asl
		val m = Int.min (n, len - p)
		in
		  checkClosed ();
		  pos := p+m;
		  CharArraySlice.copyVec
		      { src = CharVectorSlice.slice (src, p, SOME m),
			dst = buf, di = i };
		  m
		end
	  fun getPos () = (checkClosed(); Position.fromInt (!pos))
	  fun setPos p =
	      (checkClosed ();
	       if p < 0 andalso p > plen then raise Subscript
	       else pos := Position.toInt p)
	  in
	    PrimIO.RD{
		name        = "<string>",
		chunkSize   = len,
		readVec     = withLock readV,
        	readArr     = withLock readA,
		readVecEvt  = withLock(CML.alwaysEvt o readV),
		readArrEvt  = withLock(CML.alwaysEvt o readA),
		avail       = SOME o avail,
		getPos      = SOME(withLock getPos),
		setPos	    = SOME(withLock setPos),
        	endPos      = SOME(withLock(fn () => (checkClosed(); plen))),
		verifyPos   = SOME(withLock getPos),
		close       = withLock(fn () => closed := true),
		ioDesc      = NONE
	      }
	  end

  end; (* PosixTextPrimIO *)

