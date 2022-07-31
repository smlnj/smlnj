(* win32-filesys.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Hooks to Win32 file system.
 *)

local
  structure Time = TimeImp
  structure Word64 = Word64Imp
in
structure Win32_FileSys : WIN32_FILESYS =
  struct

    structure W32G = Win32_General
    type hndl = W32G.hndl

    type word = W32G.word

    fun hndlToIOD h = OS.IO.IODesc(ref h)
    fun IODToHndl (OS.IO.IODesc(ref h)) = h
      | IODToHndl (OS.IO.SockDesc _) = raise Fail "IODToHndl: socket"

    fun rebindIOD (OS.IO.IODesc hr,h) = hr := h

    fun cf name = W32G.cfun "WIN32-FILESYS" name

    val findFirstFile : string -> (hndl * string option) = cf "find_first_file"
    val findNextFile : hndl -> string option = cf "find_next_file"
    val findClose : hndl -> bool = cf "find_close"

    val setCurrentDirectory : string -> bool = cf "set_current_directory"
    val getCurrentDirectory' : unit -> string = cf "get_current_directory"
    val createDirectory' : string -> bool = cf "create_directory"
    val removeDirectory : string -> bool = cf "remove_directory"

    val cc = W32G.getConst "FILE_ATTRIBUTE"

    val FILE_ATTRIBUTE_ARCHIVE : word = cc "ARCHIVE"
    val FILE_ATTRIBUTE_DIRECTORY : word = cc "DIRECTORY"
    val FILE_ATTRIBUTE_HIDDEN : word = cc "HIDDEN"
    val FILE_ATTRIBUTE_NORMAL : word = cc "NORMAL"
    val FILE_ATTRIBUTE_READONLY : word = cc "READONLY"
    val FILE_ATTRIBUTE_SYSTEM : word = cc "SYSTEM"
    val FILE_ATTRIBUTE_TEMPORARY : word = cc "TEMPORARY"
(** future win32 use
    val FILE_ATTRIBUTE_ATOMIC_WRITE : word = cc "ATOMIC_WRITE"
    val FILE_ATTRIBUTE_XACTION_WRITE : word = cc "XACTION_WRITE"
**)

    val getFileAttributes : string -> word option = cf "get_file_attributes"
    val getFileAttributes' : hndl -> word option = cf "get_file_attributes_by_handle"

    fun isRegularFile h = let (* assumes attributes accessible *)
	  val SOME a = getFileAttributes' h
	  in
	    W32G.Word.andb(FILE_ATTRIBUTE_DIRECTORY,a) = 0wx0
	  end

    val getFullPathName' : string -> string = cf "get_full_path_name"

    val getFileSize : hndl -> Position.int = cf "get_file_size"
    val getFileSizeByName : string -> Position.int option = cf "get_file_size_by_name"

    local
    (* returns time in nanoseconds *)
      val getFileTime' : string -> Word64.word option = cf "get_file_time"
    (* set file time in nanoseconds *)
      val setFileTime' : (string * Word64.word) -> bool =  cf "set_file_time"
    in
    fun getFileTime f = (case getFileTime' f
	   of SOME ns => SOME(Time.fromNanoseconds(Word64.toLargeInt ns))
	    | NONE => NONE
	  (* end case *))
    fun setFileTime (f, t) = setFileTime'(f, Word64.fromLargeInt(Time.toNanoseconds t))
    end (* local *)

    val deleteFile : string -> bool = cf "delete_file"
    val moveFile : (string * string) -> bool = cf "move_file"

    val getTempFileName' : unit -> string option = cf "get_temp_file_name"

  end
end (* local *)
