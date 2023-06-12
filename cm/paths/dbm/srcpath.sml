(* cm/paths/dbm/srcpath.sml
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * (1) Abstract paths used to designate CM description files and SML source files.
 * (2) Translation to and from file path string formats.
 * (3) Files, and interning files; stable ids.
 * (4) Anchor environments: global and local/dynamic.
 *
 * Author: Matthias Blume
 * Edited by: DBM
 *
 * Revision, phase 8:
 *   Adding some path interface functions (splitPath, addArc, addArcs) used in dbm-filename-policy.sml
 *   This phase prepares for splitting the core path functionality into a separate Path structure.
 *   The core file functionality might also be split off into a File structure that would include
 *   intern, sync, and clear (maintaining the set of interned files).
 *
 *   The Path structure would not depend of FileId, but the File structure would depend on Path
 *   and FileId.
 *
 *   The Path structure would probably include the path parsing and unparsing functionality.
 *
 *   The SrcPath structure would remain responsible for anchor environments, CWD management, and
 *   processing pathconfig files (processSpecFile). The global anchor environment would not need
 *   to be packaged as an internal structure (i.e. PathEnv is not exported and could easily
 *   be eliminated).
 *)

structure SrcPath :> SRCPATH =
struct

local

(*  structure EM = ErrorMsg *)

  structure FS  = OS.FileSys  (* Basis *)

  structure P  = Path
  structure SM = StringMap  (* ../util/sources.cm: cm/util/stringmap.sml *)

(*  fun impossible (msgs: string list) = EM.impossible (concat ("SrcPath: " :: msgs)) *)
  fun impossible (msgs: string list) = raise Fail (concat ("SrcPath: " :: msgs))

  (* non-fatal CM errors? *)
  fun error (msgs: string list) = (Say.say msgs; Say.say ["\n"])

in

   (* *********************************************************************************** *)
   (* current working directory (CDW) *)

    (* getCwdPath : unit -> path *)
    (* returns an absolute path for CWD *)
    fun getCwdPath () = parseFpath (FS.getDir ())

    (* cwd_path : path ref *)
    (* a ref initialized from the absolute path of the current working directory (CWD);
     * If the CWD changes, this record will be updated if and when the cwd function is called
     * subsequently.
     * Why store this value anyway?  Why not just call FS.getDir whenever one needs to know
     * the current working directory (as a fpath or path). The reason, apparently, is that
     * if the cwd function is called after a change in CWD, "clients" can be notified. *)
    val cwd_path = ref (getCwdPath ())

    (* cwd_notify : bool ref *)
    (* a control flag that, if true, will cause "clients" to be notified if CWD changes
     * between calls of the "cwd" function *)
    val cwd_notify = ref true

    (* scheduleNotification : unit -> unit *)
    fun scheduleNotification () = cwd_notify := true

    (* clients : (unit -> unit) list ref *)
    (* ref of a list of "registered" notification functions, which, when called, should notify
     * their associated "client" processes that a change in the CWD has been detected by a call
     * of the cwd function. *)
    val clients = ref ([] : (unit -> unit) list)

    (* addClientToBeNotified : (unit -> unit) -> unit *)
    (* used to add client notification functions to clients list *)
    fun addClientToBeNotified c = clients := c :: !clients

    (* refreshCWD : unit -> fpath *)
    fun refreshCWD () =
	let val cwdPath0 = getCwdPath ()  (* get the current CWD fpath *)
	 in (* check whether CWD has changed since last time cwd_fpath was set *)
	    if equalPaths (cwdPath0, !cwd_path) (* check for change of CWD since last call *)
	    then () (* no change; cwd_path is validated, return newCwdFpath *)
	    else (* if not, CWD must have changed since the previous call of cwd *)
	      (cwd_path := cwdPath0;
	       if !cwd_notify
	       then app (fn c => c ()) (!clients)  (* notify registered "clients" *)
	       else ();
	       cwd_notify := false); (* why turn notifications off? *)
	    cwdPath0
	end

    (* cwd : unit -> path *)
    (* returns the fpath of the current working directory, making sure that cwd_info is
     * up-to-date. If the CWD has changed since the last call of cwd, and !cwd_notify is true,
     * then "clients" are notified of the change. But what if the CWD is changes but we don't
     * call cwd afterwards. Then the clients won't be notified of the change. There seems to
     * be a problem because of the "asynchronous" way of notifying clients! If there was a change
     * of CWD this time, cwd_notify will become false, so next time cwd is called clients won't
     * be notified if there is a following CWD change, unless someone set cwd_notify to true *)
    fun cwdPath (): path = refreshCWD ()


    (* *********************************************************************************** *)
    (* paths relative to CWD
     * (Formerly handled through osstring functions?) *)

    (* cwdRelativePath [osstring']: path -> path *)
    (* try shortening a path to a CWD-relative one, if the path is absolute and extends CWD *)
    fun cwdRelativePath (path : path) : path = P.relativePath path (!cwd_path)

    (* fullPath : path -> path *)
    (* fullPath path : if path is relative, it is concatenated onto the CWD path;
     *   otherwise (path is absolute or anchored), path is returned unchanged *)
    fun fullPath (path : path) = P.mkAbsolute (!cwd_path, path)

end (* top local *)
end (* structure StrPath *)
