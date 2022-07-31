(* posix-bin-prim-io.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This implements the UNIX version of the OS specific binary primitive
 * IO structure.  The Text IO version is implemented by a trivial translation
 * of these operations (see posix-text-prim-io.sml).
 *
 *)

local
    structure Position = PositionImp
    structure OS = OSImp
in	
structure PosixBinPrimIO : OS_PRIM_IO = 
  struct

    structure PrimIO = BinPrimIO

    structure Vec = Word8Vector
    structure PF = Posix.FileSys
    structure PIO = Posix.IO

    type file_desc = PF.file_desc

    val toFPI = Position.fromInt

    fun announce s x y = (
	  (*print "Posix: "; print (s:string); print "\n"; *)
	  x y)

    val bufferSzB = 4096

    val mkReader = PIO.mkBinReader
    val mkWriter = PIO.mkBinWriter

    fun openRd name = mkReader{
	    fd = announce "openf" PF.openf(name,PIO.O_RDONLY,PF.O.flags[]),
	    name = name,
	    initBlkMode = true
	  }

    val standardMode = PF.S.flags[	(* mode 0666 *)
	    PF.S.irusr, PF.S.iwusr,
	    PF.S.irgrp, PF.S.iwgrp,
	    PF.S.iroth, PF.S.iwoth
	  ]

    fun createFile (name, mode, flags) =
	  announce "createf" PF.createf(name, mode, flags, standardMode)

    fun openWr name = mkWriter{
	    fd=createFile(name, PIO.O_WRONLY, PF.O.trunc),
	    name=name,
	    initBlkMode=true,
	    appendMode=false,
	    chunkSize=bufferSzB
	  }

    fun openApp name = mkWriter{
	    fd		= createFile(name, PIO.O_WRONLY, PF.O.append),
	    name	= name,
	    initBlkMode	= true,
	    appendMode	= true,
	    chunkSize	= bufferSzB
	  }

  end (* PosixBinPrimIO *)
end

