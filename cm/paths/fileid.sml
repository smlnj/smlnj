(*
 * Abstract file IDs.
 *   - IDs for files regardless whether they exist or not.
 *   - For existing files equivalent to OS.FileSys.file_id.
 *
 * Copyright (c) 2000 by Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature FILEID = sig

    type id
    type ord_key = id			(* to be able to match ORD_KEY *)

    val compare : id * id -> order

    val fileId : string -> id

    val canonical : string -> string
end

structure FileId :> FILEID = struct

    structure F = OS.FileSys
    structure P = OS.Path

    datatype id =
	PRESENT of F.file_id
      | ABSENT of string

    type ord_key = id

    fun compare (PRESENT fid, PRESENT fid') = F.compare (fid, fid')
      | compare (ABSENT _, PRESENT _) = LESS
      | compare (PRESENT _, ABSENT _) = GREATER
      | compare (ABSENT s, ABSENT s') = String.compare (s, s')

    fun fileId f = let
	(* To maximize our chances of recognizing equivalent path names to
	 * non-existing files, we use F.fullPath to expand the largest
	 * possible prefix of the path. *)
	fun expandPath f = let
	    fun loop { dir, file } = P.concat (F.fullPath dir, file)
		handle _ => let
		    val { dir = dir', file = file' } = P.splitDirFile dir
		in
		    loop { dir = dir', file = P.concat (file', file) }
		end
	in
	    (* An initial call to splitDirFile is ok because we already know
	     * that the complete path does not refer to an existing file. *)
	    loop (P.splitDirFile f)
	end
    in
	PRESENT (F.fileId f) handle _ => ABSENT (expandPath f)
    end

    fun canonical "" = ""
      | canonical f =
	if (F.access (f, []) handle _ => false) then
	    let val f' = P.mkCanonical f
	    in
		if F.compare (F.fileId f, F.fileId f') = EQUAL then f'
		else f
	    end
	else
	    let val { dir, file } = P.splitDirFile f
	    in
		P.joinDirFile { dir = canonical dir, file = file }
	    end
end
