(* cm/paths/dbm/srcpath.sig *)

(* Revision 8 -- adding and reorganizing Path functions + ? *)

signature SRCPATH =
sig

(* CWD management *)

  (* set a flag that enables "notification" of "clients" when a change of 
   * CWD is detected *)
  val scheduleNotification : unit -> unit

  (* add a "client" with its notification function *)
  val addClientToBeNotified : (unit -> unit) -> unit

  (* fetch/revalidate the path of the current working directory;
   * Notifies registerd "clients" if the CWD has changed since last call.
   * Unsets the notification flag! *)
  val cwdPath : unit -> Path.path

  (* maps an absolute path that extends CWD to a path relative to CWD, otherwise returns arg unchanged *)
  val cwdRelativePath : Path.path -> Path.path  (* replaces osstring' *)


(* parsing pathconfig ("spec") files  *)

  (* process a pathconfig specification file (the instream) in the context of the given fpath;
   * does a sync afterwards! *)
  val processSpecFile : Path.fpath -> TextIO.instream -> unit

end (* signature SRCPATH *)
