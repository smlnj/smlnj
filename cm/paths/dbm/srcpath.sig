(* cm/paths/dbm/srcpath.sig *)

(* Revision 9 -- breaking out separate structures PATH, FILE, AnchorEnv, PathConfigProcessing
     All that is left in SRCPATH is CWD management functions.
 *)

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

  (* fullPath path : if path is relative, it is concatenated onto the CWD path,
   * producing an absolute path;
   *   otherwise (path is absolute or anchored), path is returned unchanged *)
  val fullPath : path -> path

end (* signature SRCPATH *)
