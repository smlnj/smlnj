(* win32-text-prim-io.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This implements the Win32 version of the OS specific text primitive
 * IO structure.  It is implemented by a trivial translation of the
 * binary operations (see win32-bin-prim-io.sml).
 *)

structure Win32TextPrimIO : sig

    include OS_PRIM_IO

    val stdIn  : unit -> PrimIO.reader
    val stdOut : unit -> PrimIO.writer
    val stdErr : unit -> PrimIO.writer

    val strReader : string -> PrimIO.reader

  end = struct

    structure SV = SyncVar
    structure BinPrimIO = Win32BinPrimIO
    structure PrimIO = TextPrimIO

    structure W32FS = Win32.FileSys
    structure W32IO = Win32.IO
    structure W32G = Win32.General

    structure V = Word8Vector

    type file_desc = W32G.hndl

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

    fun stdIn () = let
	  val h = W32IO.getStdHandle(W32IO.STD_INPUT_HANDLE)
	  in
	    if W32G.isValidHandle h
	      then mkReader{fd = h, name = "<stdIn>"}
	      else raise OS.SysErr("Win32TextPrimIO: can't get stdin",NONE)
	  end

    fun stdOut () = let
	  val h = W32IO.getStdHandle(W32IO.STD_OUTPUT_HANDLE)
	  in
	    if W32G.isValidHandle h
	      then mkWriter{
		  fd = h,
		  name = "<stdOut>",
		  appendMode = true,
		  chunkSize = bufferSzB
		}
	      else raise OS.SysErr("Win32TextPrimIO: can't get stdout",NONE)
	  end

    fun stdErr () = let
	  val h = W32IO.getStdHandle(W32IO.STD_ERROR_HANDLE)
	  in
	    if W32G.isValidHandle h
	      then mkWriter{
		  fd = h,
		  name = "<stdErr>",
		  appendMode = true,
		  chunkSize = bufferSzB
		}
	      else raise OS.SysErr("Win32TextPrimIO: can't get stderr",NONE)
	  end

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
	  fun avail () = SOME(Position.fromInt (len - !pos))
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
	      val (buf, i, n) = CharArraySlice.base asl
		val p = !pos
		val m = Int.min(n, len-p)
		in
		  checkClosed ();
		  pos := p+m;
		  CharArraySlice.copyVec { src = CharVectorSlice.slice
						     (src,p,SOME len),
					   dst = buf, di = i };
		  m
		end
	  fun getPos () = (checkClosed(); Position.fromInt (!pos))
	  in
	      PrimIO.RD{
		name        = "<string>",
		chunkSize   = len,
		readVec     = withLock readV,
        	readArr     = withLock readA,
		readVecEvt  = withLock(CML.alwaysEvt o readV),
		readArrEvt  = withLock(CML.alwaysEvt o readA),
		avail       = avail,
		getPos      = SOME(withLock getPos),
		setPos	    = SOME(withLock(fn p => (
				checkClosed();
				if (p < 0) orelse (plen < p)
				  then raise Subscript
				  else ();
				pos := Position.toInt p))),
        	endPos      = SOME(withLock(fn () => (checkClosed(); plen))),
		verifyPos   = SOME(withLock getPos),
		close       = withLock(fn () => closed := true),
		ioDesc      = NONE
	      }
	  end

  end; (* Win32TextPrimIO *)
