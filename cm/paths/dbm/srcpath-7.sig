(* cm/paths/dbm-srcpath.sig *)

(* Revision 7 -- minimize internal use of file path strings (fpaths) *)

signature SRCPATH =
sig

    exception Format

    type anchor = string
    type fpath = string   (* file path *)

    type path  (* "abstract" file paths *)
    type file

    type pathEnv  (* a "functional" anchor environment: anchor --> path *)


  (* paths and files *)

    val compareFile : file * file -> order

    (* return the path component of a file *)
    val fileToPath : file -> path

    (* create a partially defined file;
     * check that there is at least one arc in the path? *)
    val pathToFile : path -> file

    (* get a time stamp *)
    val tstamp : file -> TStamp.t

    (* is the argument path absolute? *)
    val absolutePath : path -> bool

    (* path1 relative to path2, if path1 is a proper extension of path2 
     * ASSERT: path2 is absolute *)
    val relativePath : path * path -> path

    (* expand an anchor root using a given anchor expansion function *)
    val reanchorPath : (anchor -> path) -> path -> path

    (* an absolute path ==> the path relative to CWD, otherwise returns arg *)
    val cwdRelativePath : path -> path  (* replaces osstring' *)

    (* "pickle" and "unpickle" paths as string list lists
     * but these are trivial -- probably not needed *)
    val picklePath : path -> string list list
    val unpicklePath : string list list -> path


  (* Anchor environments *)
  (* Two-level anchor environments:
   *   (1) global, stateful environment, implemented as a structure;
   *   (2) functional anchor environment (local or "dynamic").
   * Both map anchors to paths. *)

    (* accessing and destructively updating the global anchor-path environment (PathEnv) *)
    val get_anchor : anchor -> path option
    val set_anchor : anchor * path option -> unit
    val reset_anchors : unit -> unit

    (* non-destructive bindings for anchors (for "scoped" anchor bindings) *)
    val bindAnchors: pathEnv -> (anchor * path) list -> pathEnv

    (* looks for anchor first in pathEnv, then in global anchor env *)
    (* val lookAnchor : pathEnv -> anchor -> path option -- don't need to export this *)
							 

  (* parsing "spec" files (?) *)

    (* process a specification file; must sync afterwards! *)
    val processSpecFile : fpath -> TextIO.instream -> unit


  (* interned files, "sync", CWD path, CWD change notifications *)

    (* "re-establish stability of ordering", DBM: What does this mean? *)
    (* reconstructs the internal set of interned files,
     * recomputing their file_ids (FileId.id). *)
    val sync : unit -> unit

    (* forget all known path names *)
    (* clears (resets to empty) the internal set of known, interned, files. *)
    val clear : unit -> unit

    (* register a "client module" with a function that notifies it when CWD changes.
     * Such notifications will be initiated by calls of the cwdPath function. *)
    val addClientToBeNotified : (unit -> unit) -> unit

    (* make sure all such clients get notified about the a CWD during "next validation". *)
    val scheduleNotification : unit -> unit

    (* fetch/revalidate the path of the current working directory;
     * notify registerd "clients" if the CWD has changed since last call. *)
    val cwdPath : unit -> path


  (* parsing fpaths and unparsing paths to fpaths *)

    (* parse file path strings into paths *)
    val parseFpath : fpath -> path

    (* native and standard file path parsers; with anchor expansion (once) *)
    val native : pathEnv -> fpath -> path    (* redundant? *)
    val standard : pathEnv -> fpath -> path  (* redundant? *)

    (* parsing segmented fpaths, with anchor expansion (once) *)
    val parseSegmentedFpath : pathEnv -> fpath -> path

    (* translate path to fpath (unparsing paths to strings) *)
    val pathToFpath : path -> fpath


(* osstring functions (obsolete?)

  ???? RETHINK! Do these need to be replaced, or can they be deleted?
  Since dpaths and dirs no longer exist!

    val osstring : file -> fpath  -- redundant ~~ fileToPath + pathToFpath
    val osstring' : file -> fpath  -- redundant -- use relativePath + pathToFpath

    (* get path relative to the file's context; this will produce an
     * absolute path if the original spec was not relative (i.e., if
     * it was anchored or absolute) *)
    val osstring_relative : file -> fpath

    (* get name of dpath -- replaced by pathToFpath? *)
    val osstring_dpath : dpath -> fpath

    (* same for dpath *)
    val osstring_dpath_relative : dpath -> fpath

    (* get name of dir *)
    val osstring_dir : dir -> string
*)


end (* signature SRCPATH *)
