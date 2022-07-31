(* os-filesys.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Win32 implementation of the OS.FileSys structure
 *)

local
  structure String = StringImp
  structure Time = TimeImp
  structure Word = WordImp
in
structure OS_FileSys : OS_FILE_SYS =
  struct
    structure OSPath = OS_Path
    structure W32G = Win32_General
    structure W32FS = Win32_FileSys
    structure S = String
    structure C = Char
    val not = Bool.not

    exception SysErr = Assembly.SysErr

    datatype dirstream = DS of {
	hndlptr : W32G.hndl ref,
	query : string,
	isOpen : bool ref,
	nextFile : string option ref
      }

    fun rse name msg = raise SysErr(String.concat[name, ": ", msg], NONE)

    fun isDir s = (case W32FS.getFileAttributes s
	   of NONE => rse "isDir" "cannot get file attributes"
	    | SOME a => W32G.Word.andb(W32FS.FILE_ATTRIBUTE_DIRECTORY,a) <> 0wx0
	  (* end case *))

    fun openDir s = let
	  fun rse' s = rse "openDir" s
	  val _ = not (isDir s) andalso rse' "invalid directory"
	  fun mkValidDir s = if (S.sub(s,S.size s - 1) <> W32G.arcSepChar)
		then s^(S.str W32G.arcSepChar)
		else s
	  val p = (mkValidDir s)^"*"
	  val (h,firstName) = W32FS.findFirstFile p
	  in
	    if not (Handle.isValid h)
	      then rse' "cannot find first file"
	      else DS{
		  hndlptr=ref h,query=p,
		  isOpen=ref true,nextFile=ref firstName
		}
	  end

    fun readDir (DS{isOpen=ref false,...}) =
	  rse "readDir" "stream not open"
      | readDir (DS{nextFile=ref NONE,...}) = NONE
      | readDir (DS{hndlptr,nextFile=nF as ref (SOME name),...}) = (
	  nF := W32FS.findNextFile (!hndlptr);
	  case name
	   of "" => NONE
	    | _ => SOME name
	  (* end case *))
    val readDir = (* OSPath.mkCanonical o *) readDir

    fun closeDir (DS{isOpen=ref false,...}) = ()
      | closeDir (DS{hndlptr,isOpen,...}) = (
	  isOpen := false;
	  if W32FS.findClose (!hndlptr)
	    then ()
	    else rse "closeDir" "win32: unexpected closeDir failure")

    fun rewindDir (DS{isOpen=ref false,...}) =
	  rse "rewindDir" "rewinddir on closed directory stream"
      | rewindDir (d as DS{hndlptr,query,isOpen,nextFile}) = let
	  val _ = closeDir d
	  val (h, firstName) = W32FS.findFirstFile query
	  in
	    if not (Handle.isValid h)
	      then rse "rewindDir" "cannot rewind to first file"
	      else (
		hndlptr := h;
		nextFile := firstName;
		isOpen := true)
	  end

    fun chDir s = if W32FS.setCurrentDirectory s
	  then ()
	  else rse "chDir" "cannot change directory"

    val getDir = OSPath.mkCanonical o W32FS.getCurrentDirectory'

    fun mkDir s = if W32FS.createDirectory' s
	  then ()
	  else rse "mkDir" "cannot create directory"

    fun rmDir s = if W32FS.removeDirectory s
	  then ()
	  else rse "rmDir" "cannot remove directory"

    fun isLink _ = false
    fun readLink _ = rse "readLink" "OS does not have links"

    fun exists s = W32FS.getFileAttributes s <> NONE

    fun fullPath "" = getDir ()
      | fullPath s = if exists s
	  then OSPath.mkCanonical(W32FS.getFullPathName' s)
	  else rse "fullPath" "file does not exist"

    fun realPath p = if OSPath.isAbsolute p
	  then fullPath p
	  else OSPath.mkRelative {path=fullPath p, relativeTo=fullPath (getDir())}

    fun fileSize s = (case W32FS.getFileSizeByName s
	   of SOME w => w
	    | NONE => rse "fileSize" "cannot get size"
	  (* end case *))

    fun modTime s = (case W32FS.getFileTime s
	   of (SOME t) => t
	    | NONE => rse "modTime" "cannot get file time"
	  (* end case *))

    fun setTime (s, t) = let
	  val t = (case t of NONE => Time.now() | SOME t' => t')
	  in
	    if W32FS.setFileTime (s, t)
	      then ()
	      else rse "setTime" "cannot set time"
	  end

    fun remove s = if W32FS.deleteFile s
	  then ()
	  else rse "remove" "cannot remove file"

    fun rename {old: string, new: string} = let
	  fun rse' s = rse "rename" s
	  val _ = if not (exists old)
		then rse' (concat["cannot find old='", old, "'"])
		else ()
	  val same = (exists new) andalso
		     (fullPath old = fullPath new)
	  in
	    if not same
	      then (
		if (exists new)
		  then remove new handle _ => rse' "cannot remove 'new'"
		  else ();
		if W32FS.moveFile (old, new)
		  then ()
		  else rse' "moveFile failed")
	      else ()
	  end

    datatype access_mode = A_READ | A_WRITE | A_EXEC

    val strUpper = CharVector.map C.toUpper

    fun access (s, []) = exists s
      | access (s, al) = (case W32FS.getFileAttributes s
	   of NONE => rse "access" "cannot get file attributes"
	    | SOME aw => let
		fun aux A_READ = true
		  | aux A_WRITE = (W32G.Word.andb(W32FS.FILE_ATTRIBUTE_READONLY,aw) = 0w0)
		  | aux A_EXEC = (case #ext(OS_Path.splitBaseExt s)
		       of SOME ext => (case (strUpper ext)
			     of ("EXE" | "COM" | "CMD" | "BAT" ) => true
			      | _ => false
			    (* end case *))
			| NONE => false
		      (* end case *))
		in
		  List.all aux al
		end
	  (* end case *))

    fun tmpName () = (case W32FS.getTempFileName' ()
	   of NONE => rse "tmpName" "cannot obtain tmp filename"
	    | SOME s => s
	  (* end case *))

  (* Windows does not have an equivalent to inode-numbers, so we use the canonical
   * full path for the file as its unique ID.
   *)
    type file_id = string

    fun fileId s = if exists s
	  then OSPath.mkCanonical(W32FS.getFullPathName' s)
	  else rse "fileId" "No such file or directory"

    fun hash (fid : file_id) = Word.fromInt (
	  CharVector.foldl (fn (a, b) => (Char.ord a + b) handle _ => 0) 0 fid)

    val compare = String.compare

  end (* OS_FileSys *)

end (* local *)
