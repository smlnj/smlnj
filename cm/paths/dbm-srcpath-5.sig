(* cm/paths/dbm-srcpath.sig *)

signature SRCPATH =
sig

    exception Format

    type anchor = string
    type fpath = string   (* file path *)

    type apath  (* "abstract" file paths *)
    type file

    type apathEnv  (* a "functional" environment: anchor --> apath, a StringMap.map *)

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
    val get_anchor : anchor -> string option
    val set_anchor : anchor * string option -> unit (* native syntax! *)
    val reset_anchors : unit -> unit

    (* process a specification file; must sync afterwards! *)
    val processSpecFile : fpath -> TextIO.instream -> unit

    (* non-destructive bindings for anchors (for anchor scoping) *)
    val bindAnchors: dpathEnv -> (anchor * apath) list -> apathEnv

    (* make abstract paths (apaths) ???? *)
    val native : apathEnv -> fpath -> apath
    val standard : apathEnv -> fpath -> apath

    (* extend a dpath's arcs (naming relative to a directory) with a list of new arcs *)
    val extendApath : apath -> string list -> apath

    (* check that there is at least one arc in the path of the dpath *)
    val apathToFile : apath -> file

    (* To be able to pickle a file, turn it into a apath first... ???? *)
    val fileToApath : file -> apath

    (* current working directory *)
    val cwd : unit -> fpath
    val fileToApath : file -> apath

    (* get info out of abstract paths *)
    val osstring : file -> fpath
    val osstring' : file -> fpath	(* use relative path if shorter *)

    (* get path relative to the file's context; this will produce an
     * absolute path if the original spec was not relative (i.e., if
     * it was anchored or absolute) *)
    val osstring_relative : file -> fpath

  (* ???? needed? replaceable?

    (* get name of dpath *)
    val osstring_dpath : dpath -> fpath

    (* same for dpath *)
    val osstring_dpath_relative : dpath -> fpath

    (* get name of dir *)
    val osstring_dir : dir -> string
  *)

    (* expand root anchors using given function *)
    val osstring_reanchored : (anchor -> string) -> file -> fpath option

    (* get a human-readable (well, sort of) description *)
    val fileToFpath : file -> fpath

    (* get a time stamp *)
    val tstamp : file -> TStamp.t

    (* portable encodings that avoid whitespace *)
    val encodeFile : file -> fpath
    val decodeFile : apathEnv -> fpath -> file

    (* check whether encoding (result of "encode") is absolute
     * (i.e., neither anchored, nor relative) *)
    val absoluteFpath : fpath -> bool

    (* ???? need to be converted to work with apaths *)
    val pickle : (bool * string -> unit) ->
		 { dpath: dpath, relativeTo: file } -> string list list

    val unpickle : dpathEnv ->
		   { pickled: string list list, relativeTo: file } -> dpath

end (* signature SRCPATH *)
