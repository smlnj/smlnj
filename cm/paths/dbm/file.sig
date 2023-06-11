(* cm/paths/dbm/file.sig *)

(* interned files, "sync", CWD path, CWD change notifications *)

signature FILE =
sig

  type file

  val compare : file * file -> order

  (* return the path component of a file *)
  val fileToPath : file -> path

  (* create an interned file, with specified path.
   * Does not (yet) check that there is at least one arc in the path. *)
  val pathToFile : path -> file

  (* get a time stamp *)
  val tstamp : file -> TStamp.t

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
   * notify registerd "clients" if the CWD has changed since last call (if notifications enabled) *)
  val cwdPath : unit -> path

end (* signature FILE *)
