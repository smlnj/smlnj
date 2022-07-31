(* unix-path.sml
 *
 * COPYRIGHT (c) 2007 The Fellowship of SML/NJ (http://smlnj.org)
 * All rights reserved.
 *
 * Note that this module is largely superseded by the `PathUtil` module
 * in the *Util Library*.
 *)

structure UnixPath : UNIX_PATH =
  struct

    datatype access_mode = datatype OS.FileSys.access_mode

    datatype file_type = F_REGULAR | F_DIR | F_SYMLINK | F_SOCK | F_CHR | F_BLK

  (** Path lists **)

    type path_list = string list

    fun getPath () = let
	  val path = (case (UnixEnv.getEnv "PATH") of (SOME p) => p | _ => "")
	  in
	    String.fields (fn #":" => true | _ => false) path
	  end (* getPath *)

    local

      structure ST = Posix.FileSys.ST
      fun isFileTy (path, ty) = let
	    val st = Posix.FileSys.stat path
	    in
	      case ty
	       of F_REGULAR => ST.isReg st
		| F_DIR => ST.isDir st
		| F_SYMLINK => ST.isLink st
		| F_SOCK => ST.isSock st
		| F_CHR => ST.isChr st
		| F_BLK => ST.isBlk st
	      (* end case *)
	    end
      fun access mode pathname = (OS.FileSys.access(pathname, mode))
      fun accessAndType (mode, ftype) pathname = (
	    OS.FileSys.access(pathname, mode)
	    andalso isFileTy(pathname, ftype))
	      handle _ => false

    in

    fun findFile (pl, mode) = PathUtil.existsFile (access mode) pl
    fun findFiles (pl, mode) = PathUtil.allFiles (access mode) pl
    fun findFileOfType (pl, ftype, mode) =
	  PathUtil.existsFile (accessAndType(mode, ftype)) pl
    fun findFilesOfType (pl, ftype, mode) =
	  PathUtil.allFiles (accessAndType(mode, ftype)) pl

    end (* local *)

  end (* UnixPath *)
