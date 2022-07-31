(* path-util.sml
 *
 * COPYRIGHT (c) 2012 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Various higher-level pathname and searching utilities.
 *)

structure PathUtil : PATH_UTIL =
  struct

    structure P = OS.Path
    structure F = OS.FileSys

    fun existsFile pred pathList fileName = let
	  fun chk s = if (pred s) then SOME s else NONE
	  fun iter [] = NONE
	    | iter (p::r) = (case chk(P.joinDirFile{dir=p, file=fileName})
		 of NONE => iter r
		  | res => res
		(* end case *))
	  in
	    if P.isAbsolute fileName
	      then chk fileName
	      else iter pathList
	  end
    fun allFiles pred pathList fileName = let
	  fun chk s = if (pred s) then SOME s else NONE
	  fun iter ([], l) = rev l
	    | iter (p::r, l) = (case chk(P.joinDirFile{dir=p, file=fileName})
		 of NONE => iter(r, l)
		  | (SOME s) => iter(r, s::l)
		(* end case *))
	  in
	    if not(P.isAbsolute fileName)
	      then iter (pathList, [])
	    else if (pred fileName)
	      then [fileName]
	      else []
	  end

    fun fileExists s = F.access(s, [])

    val findFile  = existsFile fileExists
    val findFiles = allFiles fileExists

    val findExe = existsFile (fn p => OS.FileSys.access(p, [OS.FileSys.A_EXEC]))

  end
