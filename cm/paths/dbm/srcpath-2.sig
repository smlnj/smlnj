(* cm/paths/dbm-srcpath.sig *)

signature SRCPATH =
sig

    exception Format

    type anchor = string
    type filepath = string

    type prepath
    type prefile
    type file
    type dir

    type prepathEnv  (* anchor --> prepath, stateful *)
    type prefileEnv  (* anchor --> prefile, functional *)

    (*  "re-establish stability of ordering", DBM: ??? *)
    val sync : unit -> unit

    (* forget all known path names *)
    val clear : unit -> unit

    (* register a "client" module that wishes to be notified when
     * the CWD changes.  DBM: what sort of things are clients? *)
    val addClientToBeNotified : (string -> unit) -> unit

    (* make sure all such clients get notified about the a CWD during
     * "next validation" ? *)
    val scheduleNotification : unit -> unit

    (* new "empty" prepathEnv *)
    val newEnv : unit -> prepathEnv

    (* destructive updates to anchor settings (for configuration) *)
    val set_anchor : prepathEnv * anchor * string option -> unit (* native syntax! *)
    val get_anchor : prepathEnv * anchor -> string option
    val reset_anchors : prepathEnv -> unit

    (* process a specification file; must sync afterwards! *)
    val processSpecFile : env * filepath -> TextIO.instream -> unit

    (* non-destructive bindings for anchors (for anchor scoping) *)
    val bind: prefileEnv -> (anchor * prefile) list -> prefileEnv

    (* make abstract paths (prefiles) *)
    val mkPrefile : dir * filepath -> prefile
    val native : env -> dir * filepath -> prefile
    val standard : env -> dir * filepath -> prefile

    (* extend a prefile's arcs (naming relative to a directory) with a list of new arcs *)
    val extendPrefile : prefile -> string list -> prefile

    (* check that there is at least one arc in after the path's context *)
    val file : prefile -> file

    (* To be able to pickle a file, turn it into a prefile first... *)
    val fileToPrefile : file -> prefile

    (* directory paths (contexts) *)
    val cwd : unit -> dir
    val fileToDir : file -> dir

    (* get info out of abstract paths *)
    val osstring : file -> filepath
    val osstring' : file -> filepath	(* use relative path if shorter *)

    (* get path relative to the file's context; this will produce an
     * absolute path if the original spec was not relative (i.e., if
     * it was anchored or absolute) *)
    val osstring_relative : file -> filepath

    (* get name of prefile *)
    val osstring_prefile : prefile -> string

    (* same for prefile *)
    val osstring_prefile_relative : prefile -> filepath

    (* get name of dir *)
    val osstring_dir : dir -> string

    (* expand root anchors using given function *)
    val osstring_reanchored : (anchor -> string) -> file -> filepath option

    (* get a human-readable (well, sort of) description *)
    val fileToFilepath : file -> string

    (* get a time stamp *)
    val tstamp : file -> TStamp.t

    (* portable encodings that avoid whitespace *)
    val encodeFile : file -> filepath
    val decodeFilepath : env -> filepath -> file

    (* check whether encoding (result of "encode") is absolute
     * (i.e., not anchored, nor relative) *)
    val absoluteFilepath : filepath -> bool

    val pickle : (bool * string -> unit) ->
		 { prefile: prefile, relativeTo: file } -> string list list

    val unpickle : env ->
		   { pickled: string list list, relativeTo: file } -> prefile

end (* signature SRCPATH *)