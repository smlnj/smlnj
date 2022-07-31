(* win32-text-prim-io.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Implementation of Win32 text primitive IO.
 *)

local
  structure Word32 = Word32Imp
  structure OS = OSImp
  structure String = StringImp
  structure Int = IntImp
  structure Position = PositionImp
in
structure Win32TextPrimIO : sig
	include OS_PRIM_IO

	val stdIn  : unit -> PrimIO.reader
	val stdOut : unit -> PrimIO.writer
	val stdErr : unit -> PrimIO.writer

	val strReader : string -> PrimIO.reader
    end = struct
    structure PrimIO = TextPrimIO

    structure W32FS = Win32.FileSys
    structure W32IO = Win32.IO
    structure W32G = Win32.General

    structure V = CharVector

    type file_desc = W32G.hndl

    fun announce s x y = (
(**	  W32G.sayDebug(concat["Win32TextPrimIO: ", s, "\n"]); **)
	  x y)

    val bufferSzB = 4096

    fun mkReader {initBlkMode=false, ...} =
	  raise IO.NonblockingNotSupported
      | mkReader {fd, name, initBlkMode} = let
	  val closed = ref false
	  fun ensureOpen f x = if !closed then raise IO.ClosedStream else f x
	  val blocking = ref initBlkMode
	  val iod = W32FS.hndlToIOD fd
	  fun readVec n = announce "readVecTxt" W32IO.readVecTxt(W32FS.IODToHndl iod, n)
	  fun readArr arg = announce "readArrTxt" W32IO.readArrTxt(W32FS.IODToHndl iod, arg)
	  fun close () = if !closed
		then ()
	        else (
		  closed:=true;
		  announce "close" W32IO.close (W32FS.IODToHndl iod))
	  in
	    PrimIO.RD{
		name = name,
		chunkSize = bufferSzB,
		readVec = SOME (ensureOpen readVec),
		readArr = SOME (ensureOpen readArr),
		readVecNB = NONE,
		readArrNB = NONE,
		block = NONE,
		canInput = NONE,
		avail = fn () => NONE,
		getPos = NONE,
		setPos = NONE,
		endPos = NONE,
		verifyPos = NONE,
		close = close,
		ioDesc = SOME iod
	      }
	  end

    val shareAll = Word32.orb(W32IO.FILE_SHARE_READ,
			      W32IO.FILE_SHARE_WRITE)

    fun checkHndl name h = if Handle.isValid h
	  then h
	  else raise OS.SysErr(concat["Win32TextPrimIO.", name, ": failed"], NONE)

    fun openRd name = mkReader{
	    fd = checkHndl "openRd"
			   (announce "createFile"
				     W32IO.createFile{
					 name=name,
					 access=W32IO.GENERIC_READ,
					 share=shareAll,
					 mode=W32IO.OPEN_EXISTING,
					 attrs=0wx0
				     }),
	    name = name,
	    initBlkMode = true
	  }

    fun mkWriter {initBlkMode=false,...} =
	  raise IO.NonblockingNotSupported
      | mkWriter {fd,name,initBlkMode,appendMode,chunkSize} = let
	  val closed = ref false
	  val blocking = ref initBlkMode
	  fun ensureOpen () =
	        if !closed then raise IO.ClosedStream else ()
	  val iod = W32FS.hndlToIOD fd
	  fun writeVec v = announce "writeVec"
			     W32IO.writeVecTxt (W32FS.IODToHndl iod,v)
	  fun writeArr v = announce "writeArr"
			     W32IO.writeArrTxt (W32FS.IODToHndl iod,v)
	  fun close () =
	      if !closed then ()
	      else (closed:=true;
		    announce "close"
		      W32IO.close (W32FS.IODToHndl iod))
	  in
	    PrimIO.WR{
		name		= name,
		chunkSize	= chunkSize,
		writeVec	= SOME writeVec,
		writeArr	= SOME writeArr,
		writeVecNB	= NONE,
		writeArrNB	= NONE,
		block		= NONE,
		canOutput	= NONE,
		getPos		= NONE,
		setPos		= NONE,
		endPos		= NONE,
		verifyPos	= NONE,
		close		= close,
		ioDesc		= SOME iod
	      }
	  end

    fun openWr name = mkWriter{
	    fd = checkHndl "openWr"
			   (announce "createFile"
				     W32IO.createFile{
					 name=name,
					 access=W32IO.GENERIC_WRITE,
					 share=shareAll,
					 mode=W32IO.CREATE_ALWAYS,
					 attrs=W32FS.FILE_ATTRIBUTE_NORMAL
				     }),
	    name = name,
	    initBlkMode = true,
	    appendMode = false,
	    chunkSize = bufferSzB
	  }

    fun openApp name = let
	  val h = checkHndl "openApp"
		    (announce "createFile"
		      W32IO.createFile{
			  name=name,
			  access=W32IO.GENERIC_WRITE,
			  share=shareAll,
			  mode=W32IO.OPEN_ALWAYS,
			  attrs=W32FS.FILE_ATTRIBUTE_NORMAL
			})
	  val _ = announce "setFilePointer'" W32IO.setFilePointer' (h, 0, W32IO.FILE_END)
	  in
	    mkWriter{
		fd = h,
		name = name,
		initBlkMode = true,
		appendMode = true,
		chunkSize = bufferSzB
	      }
	  end

    fun stdIn () = let
	  val h = W32IO.getStdHandle W32IO.STD_INPUT_HANDLE
	  in
	    if Handle.isValid h
	      then mkReader{fd = h, name = "<stdIn>", initBlkMode = true}
	      else raise OS.SysErr("Win32TextPrimIO: cannot get stdin", NONE)
	  end

    fun stdOut () = let
	  val h = W32IO.getStdHandle W32IO.STD_OUTPUT_HANDLE
	  in
	    if Handle.isValid h
	      then mkWriter{
		  fd = h,
		  name = "<stdOut>",
		  initBlkMode = true,
		  appendMode = true,
		  chunkSize = bufferSzB
	      }
	      else raise OS.SysErr("Win32TextPrimIO: cannot get stdout", NONE)
	end

    fun stdErr () = let
	  val h = W32IO.getStdHandle W32IO.STD_ERROR_HANDLE
	  in
	    if Handle.isValid h
	      then mkWriter{
		  fd = h,
		  name = "<stdErr>",
		  initBlkMode = true,
		  appendMode = true,
		  chunkSize = bufferSzB
		}
	      else raise OS.SysErr("Win32TextPrimIO: cannot get stderr", NONE)
	  end

    fun strReader src = let (* stolen wholesale from posix-text-prim-io.sml *)
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
		val m = Int.min(n, len-p)
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
	       if p < 0 orelse p > plen then raise Subscript
	       else pos := Position.toInt p)
	  in
	    PrimIO.RD{
		name      = "<string>",
		chunkSize = len,
		readVec   = SOME(readV),
        	readArr   = SOME(readA),
		readVecNB = SOME(SOME o readV),
		readArrNB = SOME(SOME o readA),
		block     = SOME(checkClosed),
		canInput  = SOME(fn () => (checkClosed(); true)),
		avail     = SOME o avail,
		getPos    = SOME getPos,
		setPos    = SOME setPos,
        	endPos    = SOME(fn () => (checkClosed(); plen)),
		verifyPos = SOME getPos,
		close     = fn () => closed := true,
		ioDesc    = NONE
	      }
	  end

    end
end (* local *)
