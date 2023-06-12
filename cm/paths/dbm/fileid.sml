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
  val fileId : Path.path -> id
  val canonical : P.fpath -> P.fpath

end (* signature FILEID *)


structure FileId :> FILEID =
struct

local
    
  structure FS = OS.FileSys
  structure OSP = OS.Path

  structure P = Path    
  structure SP = SrcPath  (* or, just Cwd? *)

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
  fun fileId (path: P.path) =
      let val fpath = P.pathToFpath (SP.fullPath path)
       in PRESENT (FS.fileId fpath) handle _ => ABSENT fpath
      end

  (* This "canoncial" function belongs somewhere else where string representations of 
   * file paths are managed. Maybe a structure named "FilePaths"? Such a structure would
   * provide a CM-specific interface to the generic functionality provided by OS.Path.*)

  (* canonical: P.fpath -> P.fpath *)
  fun canonical ("": P.fpath) = ""
    | canonical f =
      if (FS.access (f, []) handle _ => false)
      then let val f' = OSP.mkCanonical f
	    in case FS.compare (FS.fileId f, FS.fileId f')  (* how can these be different? *)
		 of EQUAL => f'
		  | _ =>  f
	   end
      else let val { dir, file } = OSP.splitDirFile f
	    in OSP.joinDirFile { dir = canonical dir, file = file }
	   end

end (* top local *)
end (* structure FileId *)
