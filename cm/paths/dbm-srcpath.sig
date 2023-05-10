(* cm/paths/dbm-srcpath.sig *)

signature SRCPATH =
sig

    exception Format

    (* When faced with an undefined anchor, pressing on does not
     * make much sense. Therefore, we raise an exception in this
     * case after reporting the error. *)
    exception BadAnchor

    type file
    type dir
    type env
    type anchor = string
    type prefile

    type rebindings = { anchor: anchor, value: prefile } list

    type ord_key = file

    (* path comparison *)
    val compare : file * file -> order

    (* re-establish stability of ordering *)
    val sync : unit -> unit

    (* forget all known path names *)
    val clear : unit -> unit

    (* re-validate current working directory *)
    val revalidateCwd : unit -> unit

    (* register a "client" module that wishes to be notified when
     * the CWD changes *)
    val addClientToBeNotified : (string -> unit) -> unit

    (* make sure all such clients get notified about the a CWD during
     * next validation *)
    val scheduleNotification : unit -> unit

    (* new "empty" env *)
    val newEnv : unit -> env

    (* destructive updates to anchor settings (for configuration) *)
    val set_anchor : env * anchor * string option -> unit (* native syntax! *)
    val get_anchor : env * anchor -> string option
    val reset_anchors : env -> unit

    (* process a specification file; must sync afterwards!
     * The function argument is used for issuing warnings. *)
    val processSpecFile :
	{ env : env, specfile : string, say : string list -> unit }
	-> TextIO.instream -> unit

    (* non-destructive bindings for anchors (for anchor scoping) *)
    val bind: env -> rebindings -> env

    (* make abstract paths *)
    val raw : { err: string -> unit } ->
              { context: dir, spec: string } -> prefile
    val native : { err: string -> unit, env: env } ->
		 { context: dir, spec: string } -> prefile
    val standard : { err: string -> unit, env: env } ->
		   { context: dir, spec: string } -> prefile

    (* augment a prefile (naming a directory) with a list of arcs... *)
    val extend : prefile -> string list -> prefile

    (* check that there is at least one arc in after the path's context *)
    val file : prefile -> file

    (* To be able to pickle a file, turn it into a prefile first... *)
    val pre : file -> prefile

    (* directory paths (contexts) *)
    val cwd : unit -> dir
    val dir : file -> dir

    (* get info out of abstract paths *)
    val osstring : file -> string
    val osstring' : file -> string	(* use relative path if shorter *)

    (* expand root anchors using given function *)
    val osstring_reanchored : (anchor -> string) -> file -> string option

    (* get path relative to the file's context; this will produce an
     * absolute path if the original spec was not relative (i.e., if
     * it was anchored or absolute) *)
    val osstring_relative : file -> string

    (* same for prefile *)
    val osstring_prefile_relative : prefile -> string

    (* get name of dir *)
    val osstring_dir : dir -> string

    (* get name of prefile *)
    val osstring_prefile : prefile -> string

    (* get a human-readable (well, sort of) description *)
    val descr : file -> string

    (* get a time stamp *)
    val tstamp : file -> TStamp.t

    (* portable encodings that avoid whitespace *)
    val encode : file -> string
    val decode : env -> string -> file

    (* check whether encoding (result of "encode") is absolute
     * (i.e., not anchored and not relative) *)
    val encodingIsAbsolute : string -> bool

    val pickle : { warn: bool * string -> unit } ->
		 { file: prefile, relativeTo: file } -> string list list

    val unpickle : env ->
		   { pickled: string list list, relativeTo: file } -> prefile

end (* signature SRCPATH *)
