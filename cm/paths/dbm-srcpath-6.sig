(* cm/paths/dbm-srcpath.sig *)

signature SRCPATH =
sig

    exception Format

    type anchor = string
    type fpath = string   (* file path *)

    type path  (* "abstract" file paths *)
    type file

    type pathEnv  (* a "functional" environment: anchor --> path, a StringMap.map *)

    val compareFile : file * file -> order

    (*  "re-establish stability of ordering", DBM: ??? *)
    (* reconstructs the internal mapping from files to stable ids, while
     * recomputing their associated file_ids (FileId.id). *)
    val sync : unit -> unit

    (* forget all known path names *)
    (* clears (resets to empth) the internal mapping from files to their stable ids. *)
    val clear : unit -> unit

    (* register a "client module" that wishes to be notified when CWD changes.
     * Such notifications will be initiated by calls of the cwd function.
     * DBM: what sort of things are clients? *)
    val addClientToBeNotified : (unit -> unit) -> unit

    (* make sure all such clients get notified about the a CWD during
     * "next validation" ? *)
    val scheduleNotification : unit -> unit

    (* destructive updates to the global anchor prepath environment (for configuration) *)
    val get_anchor : anchor -> fpath option
    val set_anchor : anchor * fpath option -> unit (* native syntax! *)
    val reset_anchors : unit -> unit

    (* process a specification file; must sync afterwards! *)
    val processSpecFile : fpath -> TextIO.instream -> unit

    (* non-destructive bindings for anchors (for anchor scoping) *)
    val bindAnchors: pathEnv -> (anchor * path) list -> pathEnv

    (* parse fpaths into paths, while "expanding" anchors to their associated poths *)
    val native : pathEnv -> fpath -> path
    val standard : pathEnv -> fpath -> path

    (* translate path to fpath (unparse path) *)
    val pathToFpath : path -> fpath

    (* should check that there is at least one arc in the path? *)
    val pathToFile : path -> file

    (* return the path component of a file *)
    val fileToPath : file -> path

    (* current working directory *)
    val cwd : unit -> fpath

    (* get info out of abstract paths *)
    val osstring : file -> fpath
    val osstring' : file -> fpath	(* use relative path if shorter *)

  (* ???? RETHINK! Do these need to be replaced, or can they be deleted?
     Since dpaths and dirs no longer exist!

    (* get path relative to the file's context; this will produce an
     * absolute path if the original spec was not relative (i.e., if
     * it was anchored or absolute) *)
    val osstring_relative : file -> fpath


    (* get name of dpath *)
    val osstring_dpath : dpath -> fpath

    (* same for dpath *)
    val osstring_dpath_relative : dpath -> fpath

    (* get name of dir *)
    val osstring_dir : dir -> string
  *)

    (* expand root anchors using a given anchor expansion function *)
    val pathReanchored : (anchor -> path) -> path -> path option

    (* get a time stamp *)
    val tstamp : file -> TStamp.t

    (* portable encodings that avoid whitespace *)
    val encodeFile : file -> fpath
    val parseFpath : pathEnv -> fpath -> path

    (* check whether encoding (result of "encode") is absolute
     * (i.e., neither anchored, nor relative) *)
    val absoluteFpath : fpath -> bool

    (* converted to work with paths, but don't do much *)
    val picklePath : path -> string list list

    val unpicklePath : string list list -> path

end (* signature SRCPATH *)
