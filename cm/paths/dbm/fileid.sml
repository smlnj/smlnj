(*
 * Abstract file IDs.
 *   - IDs for files regardless whether they exist or not.
 *   - For existing files equivalent to OS.FileSys.file_id.
 *
 * Copyright (c) 2023 The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (matthias.blume@gmail.com)
 *)

signature FILEID =
sig

  type id

  val compare : id * id -> order
  val fileId : string -> id
  val canonical : string -> string

end (* signature FILEID *)


structure FileId :> FILEID =
struct

local
    
  structure FS = OS.FileSys
  structure OSP = OS.Path
  structure P = Path    

in

  datatype id
    = PRESENT of FS.file_id
    | ABSENT of string

  fun compare (PRESENT fid, PRESENT fid') = FS.compare (fid, fid')
    | compare (ABSENT _, PRESENT _) = LESS
    | compare (PRESENT _, ABSENT _) = GREATER
    | compare (ABSENT s, ABSENT s') = String.compare (s, s')

  (* To maximize our chances of recognizing the equivalence of path names to non-existing
   * files, we use FS.fullPath to expand the largest possible prefix of the path. *)
  (* fileId : P.fpath -> id *)
  fun fileId (f: P.fpath) =
      let fun expandPath f =
	      let fun loop { dir, file } =
		      OSP.concat (FS.fullPath dir, file)
		      handle _ =>
			let val { dir = dir', file = file' } = OSP.splitDirFile dir
			 in loop { dir = dir', file = OSP.concat (file', file) }
			end
	       in (* An initial call to splitDirFile is ok because we already know
		   * that the complete path does not refer to an existing file. *)
		  loop (OSP.splitDirFile f)
	      end

       in PRESENT (FS.fileId f) handle _ => ABSENT (expandPath f)
      end

  (* canonical: P.fpath -> P.fpath *)
  fun canonical ("": P.fpath) = ""
    | canonical f =
      if (FS.access (f, []) handle _ => false)
      then let val f' = OSP.mkCanonical f
	    in case FS.compare (FS.fileId f, FS.fileId f')
		 of EQUAL => f'
		  | _ =>  f
	   end
      else let val { dir, file } = OSP.splitDirFile f
	    in OSP.joinDirFile { dir = canonical dir, file = file }
	   end

end (* top local *)
end (* structure FileId *)
