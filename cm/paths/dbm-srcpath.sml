(* dbm-srcpath.sml
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Operations over abstract names for CM source files.
 *
 * Author: Matthias Blume
 * Edited by: DBM
 * 
 * Revision, phase 4:  rationalization of prepath, prefile, fileInfo, and file types
 *)

structure SrcPath :> SRCPATH =
struct

local

  structure EM = ErrorMsg

  structure P = OS.Path     (* Basis *)
  structure F = OS.FileSys  (* Basis *)
  structure FI = FileId     (* srcpath-lib.cm: ./fileid.sml *)
  structure SM = StringMap  (* ../util/sources.cm: cm/util/stringmap.sml *)

  fun impossible msg = EM.impossible ("SrcPath: " ^ msg)
  fun error (msgs: string list) = Say.say (concat msgs ^ "\n")

in

    exception Format
     (* This is raised only in the exported function unpickle.
        Why not use the existing UnpickleUtil.Format exception, as is done in other source files
	like stable/stabilize.sml dnd smlfile/skel-io.sml?
        [This would require importing "$pickle-lib.cm" in srcpath-lib.cm, as is done in
        cm-lib.cm.]
	There is one specific "handle SrcPath.Format, occuring in the file stable/stabilize.sml
        where the handler just raises UnpickleUtil.Format in place of this Format exn. *)

    (* anchor: the name of an anchor, e.g. "smlnj" for the anchor $smlnj *)
    type anchor = string

    (* filepath: a file path in some format
     * by default, we assume the format that OS.Path uses
     * but sometimes the format will be OS native (Unix and Windows OSs only) *)
    type filepath = string 

    (* stableid: integers used as "stable" ids of files(?) *)
    type stableid = int

    (* A apath is similar to the result of OS.Path.fromString except that
     * we keep the list of arcs in reversed order.  This makes adding
     * and removing arcs at the end easier.
     * A apath is essentially an "abstract syntax" for ordinary filepaths. *)
    type apath = { revarcs: string list, vol: string, isAbs: bool }

    (* reanchor: relative location spec that can be used reconstruct a path relative to
     * an "anchor point" (specified by a filepath) for the anchor that occurs at the end
     * of the reanchor chain (Anchor a).
     * A reanchor is a sort of "delta path" from some given filepath/apath and the anchor
     * that that path is relative to.
     * If a apath is not relative to an anchor, then there is no reanchor for that apath;
     * i.e., apathToReanchor will return NONE. *)
    datatype reanchor
      = Anchor of anchor
      | Parent of reanchor  (* this is only used in one place in computing reanchors for/with dirs ???? *)
      | Extend of string list * reanchor   (* string list is list of arcs *)

    (* DBM: The elab type, which is not exported, and so is not mentioned in the signature, was:

          type elab = { pp: apath,
	                valid: unit -> bool,
			reanchor: (anchor -> string) -> apath option }

     * elab was another internal representation of a file, or perhaps more properly, of an
     * anchor. It has been simplified out of existence, replaced by two attributes:
     *
     *   (1) a apath, associated with an attribute via an apathEnv environment (the implicit
     *       anchor --> apath mapping). The apath associated with an anchor through
     *       the env defines the "anchor-point" of the anchor (absolute or relative to ?),
     *       that is, the location in the file system that the anchor "points to".
     *
     *   (2) a "reanchor", which is a description of the "path" from an anchor to a file
     *       (filepath, apath) that is defined relative to that anchor, and only exists
     *       for files whose apath is relative to an anchor point (the location in the
     *       file system that an anchor "names").  So a reanchor option is an attribute of
     *       file, not directly of an anchor (or anchor point) directly. It represents the
     *       relationship, or path-delta between an anchor's anchor point and the location of
     *       a file. The reanchor of a file (apath, prefile) is computed from the file
     *       (respectively apath, prefile) based on its being defined by a path (list of
     *       arcs) relative to an anchor (it could probably be represented simply by a path
     *       (of type string list).
     *
     * The "valid: unit -> bool" attribute of an "elab" seems to be related to the question
     * of whether the "pp: apath" attribute is still accurate. The way the valid attribute
     * is defined and used seems is seems to indicate that "validity" is equivalent to whether
     * the env binding of the anchor is still in place. If an anchor is deleted from the env
     * mapping, then the elab that it was mapped to is no longer valid. The current hypothesis
     * is that the "valid" attribute of an elab, and hence indirectly of an anchor. is not really
     * necessary.  The "is_set" (now defined) env function can tell us whether an anchor is still
     * "current".
     *)

    (* dir: supposed to denote file system directories? *)
    datatype dir
      = CWD                   (* the current working directory, the filepath for which can
				 be obtained either from the cwd_filepath ref, or as returned
				 by a call of F.getDir (), normally through the cwd function *)
      | ANCHOR of anchorInfo  (* anchors always "denote" directories, not ordinary files *)
      | ROOT of string        (* the string is: for Unix: ""; for Windows: a volume name *)
      | DIR of dpath          (* betting we don't need the fileInfo id for this *)

    (* dpath: represents a file in terms of a directory and a list of arcs relative to that
     * directory. As usual, a corresponding file may not exist in the file system.
     * The arcs are in normal (file system) order, outermost first (unlike for apaths).
     * The arc list can be empty (?), in which case the dpath designates the "directory". *)
    withtype dpath = { dir: dir, arcs: string list }

    (* anchorInfo: information associated with an ANCHOR dir (directory).
     * Encode (if present) produces a corresponding filepath, with "anchor annotations" if
     * its boolean argument is true. *)
    and anchorInfo =
      {name  : anchor,    (* i.e. the name of the anchor, a string *)
       apath : apath,     (* original "anchor point"?
			   * should it always be absolute?
			   * should it be stored in the ANCHOR dir, or should be look it up? *)
       dpath : dpath option}  (* SOME if dpathEnv is defined for name -- stored when ANCHOR is created
			       * in the function mk_anchor *)

(*
     fileInfo: representation of a (semantic?) file?
     * How are (dir, arcs) related to the apath component of the elab component?
     * Should they be consistent? If so, is this enforced?
     * When will the elab component be changed?  (elabFileInfo, ...)
     * When will the id component be changed? -- set by intern, and reset by sync
     * Here we have used "withtype" instead of introducing the not usefule PATH constructor.
     * !!! elab component deleted. An elab can be computed when necessary by fileInfoToElab.
     * Now a fileInfo is just a dpath with an added id field, which will be set to a FileId.id
     * by intern (or reset by sync)
    and fileInfo = (* PATH constructor unnecessary (using "withthpe"), so deleted *)
      { dir: dir,               (* formerly "context" *)
	arcs: string list,      (* filepath relative to dir: INVARIANT: length arcs >= 1 *)
	id: FI.id option ref }  (* updatable, optional file id *)
 *)

    (* or, alternatively: a fileInfo could be a dpath plus an id field:
    and fileInfo = (* PATH constructor unnecessary (using "withthpe"), so deleted *)
      { dpath: dpath,
	id: FI.id option ref }  (* updatable, optional file id *)
    *)

    (* file: a file is a fileInfo record with a stableid (int) attached.
     * The stableid numbers are generated by the intern function, which also fills in the
     * id field of the fileInfo part. The interned files are "recorded" in a finite map of
     * type FileIdSidMap.map.
     * Possible INVARIANT: id field of fileInfo is defined (is SOME) *)
    type file = {dpath : dpath, fid: FI.id option ref, sid: stableid option}

    (* compareFile : file * file -> order *)
    (* This is used in paths/srcpathmap.sml to define maps over files, with ord_key = file *)
    fun compareFile ({_,sid=i1, ...}: file, {sid=i2, ...} : file) = Int.compare (id1, id2)

    (* filepathToApath : filepath -> apath *)
    (* use OS.Path.fromString to parse the filepath, then convert result to a apath
     * by reversing the arcs *)
    fun filepathToApath (filepath: string) =
	let val { arcs, vol, isAbs } = P.fromString filepath
         in { revarcs = rev arcs, vol = vol, isAbs = isAbs }
	end

    (* apathToFilepath : apath -> string *)
    fun apathToFilepath ({ revarcs, vol, isAbs }: apath) =
	P.toString { arcs = rev revarcs, vol = vol, isAbs = isAbs }


   (* *********************************************************************************** *)
   (* current working directory (CDW) *)

    (* cwd_filepath : filepath ref *)
    (* a ref initialized with the raw filepath of the current working directory (CWD);
     * If the CWD changes, this record will be updated if and when the cwd function is called
     * subsequently.
     * Why store this value anyway?  Why not just call F.getDir whenever one needs to know
     * the current working directory (as a filepath or apath). The reason, apparently, is that
     * if the cwd function is called after a change in CWD, "clients" can be notified. *)
    val cwd_filepath = ref (F.getDir ())

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
    val clients = ref ([] : (string -> unit) list)

    (* addClientToBeNotified : (unit -> unit) -> unit *)
    (* used to add client notification functions to clients list *)
    fun addClientToBeNotified c = clients := c :: !clients

    (* cwd : unit -> filepath *)
    (* returns the filepath of the current working directory, making sure that cwd_info is
     * up-to-date. If the CWD has changed since the last call of cwd, and !cwd_notify is true,
     * then "clients" are notified of the change. But what if the CWD is changes but we don't
     * call cwd afterwards. Then the clients won't be notified of the change. There seems to
     * be a problem because of the "asynchronous" way of notifying clients! If there was a change
     * of CWD this time, cwd_notify will become false, so next time cwd is called clients won't
     * be notified if there is a following CWD change, unless someone set cwd_notify to true *)
    fun cwd () =
	let val newCwdFilepath = F.getDir ()  (* get the current CWD filepath *)
	 in (* check whether CWD has changed since last time cwd_filepath was set *)
	    if newCwdFilepath = !cwd_filepath (* check for change of CWD since last call *)
	    then newCwdFilepath (* no change; cwd_filepath is validated, return newCwdFilepath *)
	    else (* if not, CWD must have changed since the previous call of cwd *)
	      (cwd_filepath := newCwdFilepath;
	       if !cwd_notify then app (fn c => c ()) (!clients) else ();
	       cwd_notify := false; (* why turn notifications off? *)
	       newCwdFilepath)
	end


    (* translating to dpath *)
(*
    (* fileInfoToDpath [pre0] : fileInfo -> dpath *)
    (* this is just a projection of the dir and arcs fields to obtain a dpath
     * not exported
     * local: fileToDpath*, pickle* *)
    fun fileInfoToDpath ({ dir, arcs, ... }: fileInfo) : dpath =
	{ dir = dir, arcs = arcs }
*)
    (* fileToDpath [pre] : file -> dpath
     * exported
     * local: encodeFile*, fileToFilepath, osstring_relative*
     * external: stable/stabilize.sml *)
    fun fileToDpath ({dpath,...}: file) : dpath = dpath


   (* *********************************************************************************** *)
   (* encoding dpaths as (possibly anchor annotated) filepath strings *)

    (* isChar : char -> char -> bool *)
    (* curried character equality; a utility function used in decodeFilepath *)
    fun isChar (c1: char) c2 = c1 = c2

    (* translating "special characters" to \ddd codes *)
    (* specialChar : char -> bool *)
    (* identifies "special" characters (nonprinting characters or members of the string
     * "/:\\$%()") that should be translated to "\ddd" codes *)
    fun specialChar c = not (Char.isPrint c) orelse Char.contains "/:\\$%()" c

    (* charCode : char -> string *)
    (* maps any char to its "\ddd" escape code representation *)
    fun charToCode c = "\\" ^ StringCvt.padLeft #"0" 3 (Int.toString (Char.ord c))


    (* specialToCode : char -> string *)
    fun specialToCode c = if specialChar c then charToCode c else String.str c

    (* transSpecial : string -> string [was ta] *)
    (* translate only special characters in the string to their \ddd codes.
     * Thus the result string does not contain any of the special characters. *)
    val transSpecial = String.translate specialToCode

    (* transAll : string -> string [was ta'] *)
    (* replace all characters in the string with their \ddd escape codes *)
    val transAll = String.translate charToCode

    (* transCode: string -> string *)
    (* converting \ddd codes in a string back into "special" characters,
     * i.e. roughly the inverse of transSpecial *)
    fun transCode (s: string) : string =
	let val dc = Char.chr o valOf o Int.fromString o implode
	    fun scan ([], r) = String.implode (rev r)
	      | scan (#"\\" :: d0 :: d1 :: d2 :: l, r) =
		  (scan (l, dc [d0, d1, d2] :: r)
		  handle _ => scan (l, d2 :: d1 :: d0 :: #"\\" :: r))
	      | scan (c :: l, r) = scan (l, c :: r)
	 in scan (String.explode s, [])
	end

    (* transArc : string -> string *)
    (* translate an arc string to "encoded" form? (special characters translated to
     *   \ddd codes). Special case translations for "." and ".." in the obscure and
     *   very cases where they are not the same as P.currentArc and P.parentArc
     *   (i.e. some OS that is not macOS, Windows, or Linux/Unix). This ensures that
     *   the actual currentArc and the actual parentArc are internally represented
     *   by the strings "." and "..", regardless of the OS.
     * Being compatible with Unix and Windows seems like enough. No need to accomodate
     * some unknown OS with potentially different conventions.
     *)
    fun transArc (arc: string) =
	if arc = P.currentArc then "."        (* i.e. arc, since P.currentArc = "." *)
	else if arc = P.parentArc then ".."   (* i.e. arc, since P.parentArc = ".." *)
	else if arc = "." then "\\046"        (* unlikely case where "." <> P.currentArc *)
	else if arc = ".." then "\\046\\046"  (* unlikely case where ".." <> P.parentArc *)
	else transSpecial arc  (* the general, or "ordinary" case *)

    (* dpathToString : dpath -> [show_anchors:]bool -> string *)
    (* dpathToString translates a dpath into a string that uses filepath notation
     * conditionally (on show_anchors) enhanced with some special anchor notations.
     * If show_anchors is true, sometimes an "expansion" of an anchor is included? e.g. "foo(=bar)" can
     * be produced when the encode component of an ANCHOR dir returns SOME "bar".
     * Also, if show_anchors is true, the "$/" abbreviation is used when the first arc matches the
     * anchor name.
     * Arc strings are "canonicalized" by applying transArc, which replaces "special" characters
     * in the arc string with their escape codes (of the form "\ddd").
     * The internal function names, e_ac and e_c, are not informative and could be improved.
     * The parameter name "show_anchors" could be changed to something more descriptive. *)
    fun dpathToString ({dir, arcs}: dpath) (show_anchors: bool) : string =
	let
 	    (* arcs_path : dir * [arcs:]string list * [ctxt:]bool * [path:]string list -> string *)
	    (* the dir argument is just passed through without change to calls of dir_path
	     * the ctxt argument enables the show_anchors anchor notation option *)
	    fun arcs_path (dir: dir, []: string list, _: bool, path: string list) =
		  dir_path (dir, path, NONE)
	      | arcs_path (dir, arcs, ctxt, path) =
		  let val arcs' as (arc0 :: _) =  (* arc0 is bound to the outermost arc? *)
			  map transArc arcs       (* get rid of any special characters in arc strings *)
		      val arcs'' as (first :: rest) = rev arcs' (* don't need to map transArc again! *)
		      val first' :: rest' =
			  (* arcs'', rest are reversed arc lists, first is "innermost" arc *)
			    if ctxt andalso show_anchors
			    then concat ["(", first, ")"] :: rest  (* first' = (first) *)
			    else arcs''
		      val path : string list =  (* reversed again by foldl to restore order *)
			    foldl (fn (arc, path) => arc :: "/" :: path) (first' :: path) rest'
		   in dir_path (dir, path, SOME arc0)
		  end

	    (* dir_path : dir * [path:]string list * string option -> string *)
	    and dir_path (ROOT "", path, _) = concat ("/" :: path)
                   (* make path absolute by adding "/" at front *)
	      | dir_path (ROOT vol, path, _) = concat ("%" :: transSpecial vol :: "/" :: path)
		   (* make path "volume-rooted" by adding "%<volume>" at front *)
	      | dir_path (CWD, path, _) = concat path  (* make filepath relative to CWD *)
	      | dir_path (ANCHOR {name, dpathOp, ...}, path, firstArcOp) =
		  let val name' = transSpecial name (* get rid of special chars in anchor name *)
		      val path : string list =
			    (* path is a list of path component strings consisting of arcs,
			       "$", "/", ":" and other anchor-related notations? *)
			    (case dpathOp
			      of SOME dpath => (* try "expanding" the anchor to a filepath *)
				   let val fp = dpathToString dpath
				    in if show_anchors
				       then "$" :: name' :: "(=" :: fp :: ")/" :: path
				       else fp :: "/" :: path
				   end
				| NONE =>
				    (case firstArcOp
				       of SOME firstArc =>
					    if show_anchors andalso firstArc = name'
					    then "$/" :: path
					    else "$" :: firstArc :: "/" :: path
					| NONE => "$" :: name' :: "/" :: path))
		   in concat path
		  end
	      | dir_path (DIR { dir, arcs}, path, _) =
		  arcs_path (dir, arcs, true, ":" :: path)  (* introduce the "segment divider", ":" *)

	 in arcs_path (dir, arcs, false, [])
	end (* fun dpathToString *)

    (* encodeFile : file -> string *)
    val encodeFile = (dpathToString false) o fileToDpath


   (* *********************************************************************************** *)
   (* translating between representations *)

    (* unintern : file -> dpath *)
    (* alternative name: fileToDpath *)
    (* returns the fileInfo component (#1) of a file *)
    fun fileToDpath ({dpath, ...}: file) : dpath = dpath

(*
    (* fileToFilepath [desc] : file -> string
     * exported
     * local: none
     * external: 10 files *)
    val fileToFilepath = dpathToString true o fileToDpath
*)
    (* fileToDir [dir]: file -> dir
     * exported
     * local: unpickle
     * external: parse/parse.sml, bootstrap/build-initdg.sml *)
    fun fileToDir (f: file) = DIR (fileToDpath f)

(* -- unnecessary 
    (* dpathToFileInfo : dpath -> fileInfo *)
    fun dpathToFileInfo ({ dir, arcs }: dpath) : fileInfo =
	{ dir = dir,
	  arcs = (case arcs
		    of nil => (error ["dpath needs at least one arc relative to `",
				      apathToFilepath (#pp (dirToElab dir)), "'"];
			       ["<bogus>"])   (* DBM: Is a fileInfo with no arcs bogus? *)
		     | _ => arcs),
	  id = ref NONE }  (* why not generate a new fileid here based on the dpath? *)
*)

    (* dpathToFile : dpath -> file *)
    val dpathToFile = intern


   (* *********************************************************************************** *)
   (* apath and reanchor functions (formerly involving elab) *)

  (* apath *)

    (* rootApath : apath *)
    fun rootApath (vol : string) : apath =
	{ revarcs = nil, vol = vol, isAbs = true }

    (* parentApath [dirPP] : apath -> apath *)
    (* returns the apath of the parent directory
     * not exported.
     * local uses: parentToDir, do_reanchor *)
    fun parentApath ({ revarcs = _ :: revarcs, vol, isAbs }: apath) : apath =
	  { revarcs = revarcs, vol = vol, isAbs = isAbs }
      | parentApath _ = impossible "parentApath"

    (* extendApath : path -> apath -> apath
     * not exported
     * local uses: fileInfoToApath, do_reanchor *)
    fun extendApath (arcs: string list) ({ revarcs, vol, isAbs }: apath) : apath  =
	{ revarcs = List.revAppend (arcs, revarcs), vol = vol, isAbs = isAbs }

    (* extendDpath [extend]: dpath -> string list -> dpath
     * exported
     * local: none
     * external: none [tools/main/private-tools.sml (augment)] *)
    fun extendDpath ({ dir, arcs }: dpath) (morearcs: string list) =
	{ dir = dir, arcs = arcs @ morearcs }

    (* dirToApath : dir -> apath
     * not exported
     * local: fileInfoToApath *)
    fun dirToApath CWD = filepathToApath (cwd ())  (* fetch CWD and parse it to derive a apath *)
      | dirToApath (ANCHOR { apath, ... }) = apath (* original anchor point *)
      | dirToApath (ROOT vol) = rootApath vol
      | dirToApath (DIR dpath) = parentApath (dpathToApath dpath) (* why parentApath? *)

    (* dpathToApath : fileInfo -> apath
     * not exported
     * local: dirToApath, getFileId *)
    and dpathToApath ({ dir, arcs }: dpath) =
	extendApath arcs (dirToApath dir)
(*
    (* fileInfoToApath : fileInfo -> apath
     * not exported
     * local: dirToApath, getFileId *)
    and fileInfoToApath ({ dir, arcs, id }: fileInfo) =
	extendApath arcs (dirToApath dir)
*)

    (* fileToApath : file -> apath
     * not exported
     * local: none *)
    val fileToApath = dpathToApath o fileToDpath


  (* reanchor *)

    (* dirToReanchor : dir -> reanchor option
     * not exported
     * local: fileInfoToReanchor, dpathToReanchor *)
    fun dirToReanchor CWD = NONE
      | dirToReanchor (ROOT vol) = NONE
      | dirToReanchor (ANCHOR { name, ... }) = SOME (Anchor name)
      | dirToReanchor (DIR dpath) =
	 Option.map Parent (dpathToReanchor dpath) (* why do we need Parent? *)

    (* dpathToReanchor : dpath -> reanchor option
     * not exported
     * local: dirToReanchor, fileToReanchor *)
    and dpathToReanchor ({ dir, arcs }: dpath) =
	Option.map (fn reanchor => Extend (arcs, reanchor)) (dirToReanchor dir)

    (* fileToReanchor : file -> reanchor option
     * not exported
     * local: osstring_reanchored* *)
    val fileToReanchor = dpathToReanchor o fileToDpath


    (* mk_anchor : dpathEnv * anchor -> dir *)
    (* make an anchor directory: not exported.
     * Other sorts of directories are made using the directory constructors CWD, ROOR, DIR
     * not exported
     * local: native, standard, unipickle, decodeFilepath *)
    fun mk_anchor (dpathEnv, anchor: anchor) : dir =
	case SM.find (dpathEnv, anchor)
	  of SOME dpath =>  (* anchor is in the domain of the dpathEnv *)
	      ANCHOR { name = anchor,
		       apath = dpathToApath dpath, (* dpathToApath ? *)
		       dpath = SOME dpath }
	   | NONE => (* anchor is not in the domain of dpathEnv;
			as an alternative, see if it is defined in ApathEnv *)
	      ANCHOR { name = anchor,
		       apath = ApathEnv.get anchor,  (* get will fail if not (defined anchor) *)
		       dpath = NONE }


   (* *********************************************************************************** *)
   (* "interning files": assigning file_ids and generating stableids for fileInfos *)

    local

       (* known: an internal reference to a finite map from fileInfos to stableids
	    (known: int FileInfoMap.map ref).
	* New bindings are created by the intern function.
	* intern uses uses FileInfoMap.insert, which uses getFileId, which uses FileId.fileId
	*   to update the id field of the fileInfo record to a FileId.id computed by
	*   OS.FileSys.fileId. *)

	(* getFileId [idOf] : fileInfo -> FI.id *)
	(* returns the fileId associated with the file, updating the id field if it was NONE *)
	fun getFileId (file as { dpath, id, ... }: file) =
	    let val apath = dpathToApath dpath (* compute the apath of the fileInfo *)
	     in case !id
		  of SOME i => i
		   | NONE =>
		       let val i = FI.fileId (apathToFilepath apath)
			in id := SOME i; i
		       end
	    end

	(* compareFile : file * file -> order *)
	(* compare files by comparing their id fields as accessed/computed by getFileId *)
	fun compareFile (f1: file, f2: file) =
	    FI.compare (getFileId f1, getFileId f2)

	structure FileSet =
	  RedBlackSetFn (type ord_key = file
			 val compare = compareFile)

	(* known: a finite set of files *)
	val known : FileSet.set ref = ref FileSet.empty
	val next = ref 0

    in

        (* clear : unit -> unit *)
        fun clear () = known := FileMap.empty

	(* intern : fileInfo -> file *)
	(* generate a stableid (sequentially) to add to a fileInfo to make a file. If the fileInfo
	 * has already been interned before, just return the associated stableid. If it is new
	 * generate the next stableid and also (through the getFileId function called by insert)
	 * define the id field of the fileInfo. *)
	fun intern (file as {sid, ...}: file) : file =
	    case FileMap.find (!known, file)
	      of SOME stableid => (fi, stableid)  (* fi was previously interned *)
	       | NONE =>
		   let val stableid = !next  (* generate a new stableid *)
		    in next := stableid + 1;
		       known := FileMap.insert (!known, fi, stableid);
		         (* record the (fi, stableid) in known *)
		       (fi, stableid)  (* return the interned file *)
		   end

	(* sync : unit -> unit *)
	(* sync causes the file ids of all the interned files to be reset to the actual
	   file system file ids (obtained from OS.FileSys.fileId) *)
	fun sync () =
	    let val fset = !known
		fun invalidateId ({ id, ... }: file) = id := NONE
		fun reinsert (k, i, m) = FileMap.insert (m, k, i)
	     in FileSet.app invalidateId fset;
		(* This will cause the id fields to be reassigned using the file systems file ids
		 * when they are accessed via getFileId for the insert operation. *)
		known := FileSet.foldl reinsert FileSet.empty fset  (* DBM: why? *)
	    end

    end (* local -- interning files *)


    (* *********************************************************************************** *)
    (* environments: anchor-apath env, anchor-dpath functional env (formerly "bound") *)

    (* The apath environment (mapping anchors to apaths) is global, and is now implemented as
     * a structure ApathEnv.  It maintains a mapping from anchors to apaths (apath SM.map)
     * as its state, and opertions get, set, defined, and reset operate on this internal state.
     * The apath that is associated with an anchor by this environment is assumed to designate
     * the "anchor point" of the anchor (presumably a directory), i.e. the directory "named" by
     * the anchor.
     *
     * structure : ApathEnv
     * This stateful module embodies an anchor to apath environment as an "object" structure.
     * The mapping from anchors to apaths is stored in the local reference variable anchorMapRef.
     * 
     * get a: lookup the anchor a and return the associated apath, if a is bound in the current
     *   map (!anchorMapRef).  Causes a fatal error (Compiler Bug) if a is not in the domain
     *   of the map.
     * 
     * set (a, SOME pp) : bind (or rebind) the anchor a to apath pp, modifying the map
     * set (a, NONE) : remove a from the domain of the map if it is currently bound, otherwise
     *   do nothing.
     *
     * defined a : is the anchor a in the domain of the current map?
     * 
     * reset () : replace the current map with the empty SM.map, thus reseting the apath
     *   anchor environment to be the empty environment.
     *
     * An anchor that is not in the domain of the current apath map (!anchorMapRef) can be
     * considered to be "invalid".
     * The operations get and set and defined are not called outside this file.
     *   while (some other version of?) reset is called in many files in cm.
     * get is only called in get_anchor and mk_anchor.
     * set is only called in setRelative.
     * ApathEnv is not exported (is not in the signature of SrcPath)
     * local: get_anchor, setRelative, set_anchor, reset_anchors, mk_anchor, processSpecFile
     * external: main/cm-boot.sml, bootstrap/btcompile.sml
     *
     * Previously, the anchor to apath environment(s) were created by a function newEnv
     * that was called just once in the files main/cm-boot.sml and bootstrap/btcompile.sml.
     * I conjecture that there was never more than one (global) instance of this environment,
     * and hence it is safe to replace any reference to the environments created by newEnv
     * with references to the global ApathEnv structure defined inwith SrcPath.
     *
     * There is still a separate "functional" environment, of type dpathEnv, that maps 
     * anchors to dpaths.  These dpath environment are passed as parameters to the
     * exported SrcPath functions bind, decodeFilepath, and unpickle.
     *
     * The global apath environment is represented by the structure ApathEnv and apath
     * anchor environments are no longer passed as parameters.
     *
     * QUESTION: Why do we need two anchor environments, the global, stateful one in
     *   ApathEnv and the functional version (type dpathEnv)?
     *
     * QUESTION: does ApathEnv embody the "root anchor environment" mentioned in Sec 3.4
     *   of the CM manual?  Or does "root anchor environment" refer to the dual apath
     *   and dpath anchor environments?
     * 
     * QUESTION: how (where) do the pathconfig files system/pathconfig, config/extrapathconfig,
     *   and $SMLNJ/lib/pathconfig contribute to initializing the anchor environments (and which
     *   environments are initialized)?
     *)

    structure ApathEnv
      : sig	      
	  get: anchor -> apath,
	  set: anchor * apath option -> unit,
	  defined: anchor -> bool,
	  reset: unit -> unit
        end =

    struct

      val anchorMapRef : apath SM.map ref = ref SM.empty

      (* find : anchor -> apath option *)
      (* locally used "look up" function for accessing the current state of the apath
       * environment *)
      fun find anchor = SM.find (!anchorMapRef, anchor)

      (* defined : anchor -> bool *)
      (* Is anchor bound in !anchorMapRef?, i.e. in the apath anchor environment? *)
      fun defined anchor = SM.inDomain (!anchorMapRef, anchor)

      (* get : anchor -> elab *)
      (* look up anchor in !anchorMapRef. If found, return a new elab for the anchor
       * containing the same apath and validity as the existing binding. If not
       * found (anchor is not in the domain of !anchorMapRef), produces an undefined
       * anchor fatal error (impossible, compiler bug. So get should only be called with
       * an anchor that is known to be defined. *)
      fun get anchor =
	  case find anchor
	    of SOME pp => pp
	     | NONE => impossible ["get -- undefined anchor: $", anchor]

      (* set : anchor * apath option -> unit *)
      (* If apathOp is SOME apath, binds or rebinds anchor to apath.
       * If apathOp is NONE, unbinds anchor in !anchorMapRef. *)
      fun set (anchor, apathOp) : unit =
	  case find anchor
	    of SOME _ =>
		 anchorMapRef :=
		   (case apathOp
		      of SOME pp => SM.insert (!anchorMapRef, anchor, pp) (* rebind *)
		       | NONE => #1 (SM.remove (!anchorMapRef, anchor)))) (* remove *)
			 (* this can't raise NotFound because find returned SOME *)
	     | NONE =>
		 (case apathOp
		    of SOME pp => anchorMapRef := SM.insert (!anchorMapRef, anchor, pp)) (* bind *)
		     | NONE => ()) (* do nothing *)

      (* reset : unit -> unit *)
      (* wipe out the contents of the apath environment by setting anchorMapRef to SM.empty *)
      fun reset () = anchorMapRef := SM.empty  (* reset anchorMapRef to the empty map *)

    end (* structure ApathEnv *)

    (* get_anchor : anchor -> filepath option
     * maps an anchor to the filepath of its "anchor point" *)
    fun get_anchor (anchor: anchor) =
	Option.map apathToFilepath (ApathEnv.get anchor)

    (* setRelative [set0]: anchor * filepath option * filepath-> unit *)
    fun setRelative (anchor: anchor, filepathOp: filepath option,
		     relativeTo: filepath) =
	let fun fp_pp (filepath: filepath) : apath =
		let val fp1 = P.mkAbsolute {path = filepath, relativeTo = relativeTo}
		    val fp2 = if P.isAbsolute filepath then filepath else fp1
		 in filepathToApath fp2
		end
	 in ApathEnv.set (anchor, Option.map fp_pp filepathOp)
	end

    (* set_anchor : anchor * filepath option -> unit *)
    (* When filepathOp is SOME, binds a corresponding apath to the anchor in env;
     * when filepathOp is NONE, deletes the anchor and its binding from env.
     * Exported and called externally (3 times) in main/cm-boot.sml *)
    fun set_anchor (anchor, filepathOp) =
	setRelative (anchor, filepathOp, F.getDir ()) before sync ()

    (* reset_anchors : unit -> unit *)
    (* exported
     * external: main/cm-boot.sml (in resetPathConfig) *)
    fun reset_anchors () = (ApathEnv.reset (); sync ())


    (* dpath environments *)    

    type dpathEnv = dpath SM.map

    val emptyDpathEnv : dpathEnv = SM.empty			      

    (* bindDpaths : dpathEnv -> (anchor * dpath) list -> dpathEnv *)
    (* produces a new env record with only the "bound" field altered.
     * Anchors are bound to corresponding dpaths, with these bindings being
     * added to the existing "bound" mapping.
     * exported
     * external: main/general-params.sml, elsewhere? *)
    fun bindDpaths (pfenv: dpathEnv) (alist: (anchor * dpath) list) : dpathEnv =
	let fun folder ((anchor, dpath), env) = SM.insert (env, anchor, dpath)
	 in foldl folder pfenv alist
	end


    (* ******************************************************************************** *)
    (* processing "spec files" ? *)
    (* What are "spec files"? -- .cm files (CDFs)? Aren't those handled by parser?
     * What does this produce? How? *)

    (* processSpecFile : filepath -> TextIO.instream -> unit *)
    fun processSpecFile (filepath: filepath) =
	let val local_dir = P.dir (F.fullPath filepath)
	    fun set (anchor, filepathOp) =
		setRelative (anchor, filepathOp, local_dir)

	    (* mknative : bool -> string -> string *)
	    fun mknative true fp = fp
	      | mknative false fp =
		  let fun return (abs, arcs) =
			  P.toString { vol = "", isAbs = abs, arcs = arcs }
		   in case String.fields (fn c => c = #"/") fp
		        of "" :: arcs => return (true, arcs)
		         | arcs => return (false, arcs)
		  end

	    (* work: TextIO.instream -> ? *)
	    fun work (s: TextIO.instream) =
		let fun loop isnative =
			case TextIO.inputLine s
			  of NONE => ()
			   | SOME line =>
			       if String.sub (line, 0) = #"#" then loop isnative
			       else case String.tokens Char.isSpace line of
					["!standard"] => loop false
				      | ["!native"] => loop true
				      | [a, d] =>
					  (set (a, SOME (mknative isnative d));
					   loop isnative)
				      | ["-"] => (ApathEnv.reset (); loop isnative)
				      | [a] => (set (a, NONE); loop isnative)
				      | [] => loop isnative
				      | _ => (error [f, ": malformed line (ignored)"];
					      loop isnative)
		 in loop true
		end

	 in work
	end (* fun processSpecFile *)


    (* ******************************************************************************** *)
    (* parsing filepaths *)

    datatype stdspec
      = RELATIVE of string list
      | ABSOLUTE of string list
      | ANCHORED of anchor * string list

    (* parseFilepathNative [parseNativeSpec]: filepath -> stdspec *)
    fun parseFilepathNative (filepath: filepath) =
	let val {isAbs, arcs, vol} = OS.Path.fromString filepath
         in case arcs
	      of [""] => impossible "parseFilepathNative -- zero-length arc name"
	       | [] => impossible "parseFilepathNative -- no fields"
	       | (["$"] | "$"::""::_) =>
		   (error ["invalid zero-length anchor name in: ", filepath]);
		    RELATIVE arcs)
	       | "$" :: (arcs' as (arc1 :: _)) => (* "$/arc1/..." *)
		   if isAbs
		   then RELATIVE arcs          (* e.g. "/$/a/b" ? *)
		   else ANCHORED(arc1, arcs')  (* e.g. "$/a/b" ? *)
	       | arc1 :: arcn =>
		   if String.sub(arc1, 0) = #"$"
		   then ANCHORED(String.extract(arc1, 1, NONE), arcn)
		   else if isAbs
		        then ABSOLUTE arcs
		        else RELATIVE arcs
	     (* end case *)
        end

    (* parseFilepathStandard [parseStdspec]: (string -> unit ) -> filepath -> stdspec *)
    fun parseFilepathStandard (filepath: filepath) =
	let fun delim #"/" = true
	      | delim #"\\" = true
	      | delim _ = false
	    fun transl ".." = P.parentArc
	      | transl "." = P.currentArc
	      | transl arc = arc
	 in case map transl (String.fields delim s)
	      of [""] => impossible ("parseFilepathStandard -- zero-length name: " ^ filepath)
	       | [] => impossible ("parseFilePathStandard -- no fields" ^ filepath)
	       | "" :: arcs => ABSOLUTE arcs
	       | arcs as (["$"] | "$" :: "" :: _) =>
		   (error ["invalid zero-length anchor name in: ", filepath];
		    RELATIVE arcs)
	       | "$" :: (arcs as (arc1 :: _)) => ANCHORED (arc1, arcs)
	       | arcs as (arc1 :: arcn) =>
		   if String.sub (arc1, 0) <> #"$" then RELATIVE arcs
		   else ANCHORED (String.extract (arc1, 1, NONE), arcn)
	end (* fun parseFilepathStandard *)

    (* mkDpath [raw] : dir * filepath -> dpath *)
    (* it is assumed that the filepath is either absolute, and hence independent of dir, in
     * which case we ignore the dir argument and replace it with ROOT vol),
     * or, if the filepath is relative, we assume it is relative to dir. In that case vol is
     * irrelevant.
     * exported
     * local: none
     * external: semant/semant.sml, tools/main/private-tools.sml *)
    fun mkDpath (dir: dir, filepath: filepath) : dpath =
        let val {arcs, vol, isAbs} = P.fromString filepath
	    val dir = if isAbs then ROOT vol else dir
	 in {dir = dir, arcs = arcs}
	end

    (* native : dpathEnv -> dir * filepath -> dpath *)
    fun native pfenv (dir, filepath) =
          (case parseFilepathNative filepath
             of RELATIVE arcs => {dir = dir, arcs = arcs}
              | ABSOLUTE arcs => {dir = ROOT "", arcs = arcs}
              | ANCHORED (anchor, arcs) => {dir = mk_anchor (pfenv, anchor), arcs = arcs}
          (* end case *))

    (* standard : dpathEnv -> dir * filepath -> dpath *)
    fun standard pfenv (dir, filepath) =
	(case parseFilepathStandard filepath
	   of RELATIVE arcs => {dir = dir, arcs = arcs}
	    | ABSOLUTE arcs => {dir = ROOT "", arcs = arcs}
	    | ANCHORED (anchor, arcs) => {dir = mk_anchor (pfenv, anchor), arcs = arcs}
        (* end case *))


    (* *********************************************************************************** *)
    (* the "osstring" family of functions
     *  these translate files, dpaths, dirs to strings and allow for the "reanchoring"
     *  of "anchored" paths.
     *)

    (* osstring : file -> string *)
    val osstring = FI.canonical o apathToFilepath o dpathToApath o fileToDpath

    (* osstring_dpath : dpath -> string *)
    fun osstring_dpath ({ dir, arcs }: dpath) : filepath =
	FI.canonical (apathToFilepath (extendApath arcs (dirToApath dir))))

    (* osstring_dir : dir -> string *)
    fun osstring_dir dir =
	case apathToFilepath (dirToApath dir)
	  of "" => P.currentArc  (* = "." *)
	   | s => FI.canonical s

    (* osstring' : file -> string *)
    fun osstring' f =
	let val oss = osstring f
	 in if P.isAbsolute oss
	    then let val ross = P.mkRelative { path = oss, relativeTo = (!cwd_filepath) }
		  in if size ross < size oss then ross else oss
		 end
	    else oss
	end

    (* do_reanchor : (string -> filepath) -> reanchor -> apath? *)
    (* recurses down to the anchor root, maps that to a filepath using cvt, converts
       that filepath to a apath (filepathToApath) then recursively applies the transforms
       of the reanchor layers to yield a modified apath. *)
    fun do_reanchor (cvt: string -> filepath) reanchor =
	case reanchor
	  of Anchor of anchor => filepathToApath (cvt anchor)
	   | Parent of reanchor => parentApath (do_reanchor cvt reanchor)
	   | Extend of (arcs, reanchor) => extendApath arcs (do_reanchor cvt reanchor)

    (* osstring_reanchored : (string -> string) -> file -> filepath option *)
    (* used once in main/filename-policy.sml (FilenamePolicyFn) *)
    fun osstring_reanchored cvt file =
	let val reanchorOp = fileToReanchor file
	 in case reanchorOp
	    of NONE => NONE
	     | SOME reanchor => 
		 SOME (FI.canonical (apathToFilepath (do_reanchor cvt reanchor)))
	end

    (* osstring_dpath_relative : dpath -> filepath *)
    fun osstring_dpath_relative (pf as { dir, arcs }: dpath) =
	case dir
	  of DIR _ => FI.canonical (P.toString { arcs = arcs, vol = "", isAbs = false })
	   | _ => osstring_dpath pf

    (* osstring_relative : file -> filepath *)
    val osstring_relative = osstring_dpath_relative o fileToDpath

    (* tstamp : file -> TSamp.t *)
    fun tstamp f = TStamp.fmodTime (osstring f)


    (* *********************************************************************************** *)
    (* pickling and unpickling dpaths *)				   

    (* pickle : (bool * string -> unit) -> {dpath: dpath, relativeTo : file}
                -> string list list *)
    (* nontrivial warn is used in function apath2list in stabilize.sml *)
    fun pickle warn { dpath as {dir,arcs}: dpath, relativeTo = (fi, _) } =
	let fun warn' (abs_or_rel: bool) =
		warn (abs_or_rel, dpathToString false { dir = dir, arcs = arcs })
		     (* HACK! We are cheating here, turning the dpath into a file
		      * even if there are no arcs.  This is ok because of the false
		      * arg for dpathToString. *)

	    fun p_p p = p_pf (fileInfoToDpath p)

	    and p_pf ({ dir, arcs } : dpath) = arcs :: p_d dir

	    and p_d (ROOT vol) = (warn' true; [[vol, "r"]])
	      | p_d CWD = impossible "pickle: CWD"
	      | p_d (ANCHOR { name, ... }) = [[name, "a"]]
	      | p_d (DIR dir_fi) =
		(case compareFileInfo (dir_fi, fi)
		   of EQUAL => (warn' false; [["c"]])
		    | _ => p_p f0)

	 in p_pf dpath
	end

    (* unpickle : dpathEnv -> {pickled: string list list, relativeTo: file} -> dpath *)
    fun unpickle (pfenv: dpathEnv) { pickled: string list list, relativeTo: file } =
	let fun u_pf (arcs :: l) = {dir = u_d l, arcs = arcs}
	      | u_pf _ = raise Format

	    and u_p l = dpathToFileInfo (u_pf l)

	    and u_d [[vol, "r"]] = ROOT vol
	      | u_d [["c"]] = fileToDir relativeTo
	      | u_d [[n, "a"]] = mk_anchor (pfenv, n)
	      | u_d l = DIR (u_p l)

	 in u_pf pickled
	end


    (* *********************************************************************************** *)
    (* decoding "filepaths" to files (relative to a given dpathEnv) *)

    (* decodeFile [decode] : dpathEnv -> filepath -> file *)
    (* What are "segments"? (Where are they documented?)
       What is the purpose of segments?
       Where are they introduced? *)
    fun decodeFilepath (pfenv: dpathEnv) (filepath: filepath) =
	let
	    (* transArc: string -> string *)
	    (* what does transArc protect against?
	     * Is it reversing the effect of transSpecial performed earlier? *)
	    fun transArc "." = P.currentArc  (* = "." *)
	      | transArc ".." = P.parentArc  (* = ".." *)
	      | transArc a = transCode a

	    (* firstSet : string -> fileInfo *)
	    fun firstSeg s =
		(case map transArc (String.fields (isChar #"/") s)
		   of nil => impossible "decodeFilepath: no fields in segment 0"
		    | arc0 :: arcs =>
		      if arc0 = ""
		      then dpathToFileInfo (ROOT "", arcs)
		      else let val arc0' = String.extract (arc0, 1, NONE)
			       val char0 = String.sub (arc0, 0)
			    in case char0 
				 of #"%" => (* arc0' is a volume name *)
				      dpathToFileInfo (ROOT arc0', arcs)
				  | #"$" => (* arc0' is an anchor *)
				      dpathToFileInfo {dir = mk_anchor (pfenv, arc0')), arcs = arcs}
	                          | _ => dpathToFileInfo (CWD, transArc arc0 :: arcs)
	                   end

	    (* addseg : string * fileInfo -> fileInfo *)
	    fun addseg (seg, fi) =
		dpathToFileInfo {dir = DIR fi,
				   arcs = map transArc (String.fields (isChar #"/") seg)}

	 in case String.fields (isChar #":") filepath
	      of nil => impossible "decodeFilepath: no segments"
	       | seg0 :: segs => intern (foldl addseg (firstSeg seg0) segs)
	end (* fun decodeFile *)

    (* absoluteFilepath : filepath -> bool *)
    (* does filepath start with $"/" or #"%"? *)
    fun absoluteFilepath s =
	(case String.sub (s, 0) of (#"/" | #"%") => true | _ => false)
	handle _ => false

end (* top local *)
end (* structure StrPath *)


(* ================================================================================ *)

(* NOTES:

1. The design seems to be meant to accomodate possible changes in the underlying file system
   (e.g. maybe renaming and deleting files, directories, changing the current working
    directory) while maintaining "validity" of the internal CM representations (of files).

2. Lots of values, datastructure components are "suspended" or represented by thunks.
   Was this for "efficiency", or to try to insulate from changes occuring in the file system?

3. How much of the "tolerance" of background change is actually being used or is really justified?
   I can see how avoiding dependence on the current working directory would be good, i.e. refering
   to files by paths relative to the current working directory.  But it seems exessive to try to
   accomodate things like someone in the background renaming or deleting files, etc.

4. Is there a reason why srcpath-lib.cm should not import from util-lib.cm?  (e.g. to use Say.say)
   Nope! It is already importing StringMap from util.

5. There are six "representations" for files:

   1. filepath strings (using the "OS-indepent" notation used in OS.FileSys and OS.Path),

   2. apath: a structure similar to the record that OS.Path.fromString produces, but with arcs
      in reverse order (deepest arc first),

   3. dpath: a base directory (type dir), originally called the "context", plus a relative path
      (list of arcs) from that directory in outer-to-inner order,

   4. fileInfo: the information in a dpath (dir, arcs) plus
        (1) an elab ref and
        (2) an "id :FileId.id option ref", initially NONE, but reset to SOME FileId.id by intern
	    and sync, where the FileId.id is derived from the actual OS-FS file_id if it exists,

   5. file: fileInfo plus a "stableid" integer (an "interned" fileInfo),

   6. "elab": a apath plus a validity flag and a "reanchor" function (whatever that does).
      elabs can be generated from other representations (e.g. dpath) with initial valid value true.
      They can be "invalidated" if their associated env binding is

There is a lot of redundancy among these six ways of representing files, and several of them
are translatable into each other. The fileInfo, file, and elab types could be considered a bit more
"semantic" (i.e. deeper) because they involve a bit more than a purely syntactic description.
The purpose of the valid and reanchor components of an elab is not clear.

There is also a the DIR variety of the dir datatype that has an associated fileInfo, so dir values
can indirectly involve a file.

Could this multiplicity of file representations be simplified?  How many different represenations do
we really need? Note that dpath, file, and dir are the only ones exported by the SRCPATH signature,
and the only representations present in the exported val types are string, dpath, file, and dir.
Thus fileInfo, elab, and apath are only used internally in SrcPath.

There is a couple progressions:

     filepath (string) --[P.fromString]--> apath --[apathToFilepath]--> filepath

     dpath --[intern?]--> fileInfo (adding undefined file-id)
             --[intern?]--> file (adding stableid and defining file-id)

Where do dpaths come from?  Where is the link between apaths and dpaths?

Dpaths are created (directly) by the function decodeFilepath, which (with the help of the
env) translates filepaths directly to (interned) files.  There is no direct translation from
the syntactic filepath/apath representations to the dpath representation, or in particular,
to the dir type.  It is also not clear whether the id field of a fileInfo record is relevant
for working with DIR values.


6. There are two mappings over anchors, both incorporated into the env type:

   1. The implicit string map (anchorMapRef) of an env is operated on by the get, set,
      defined, and reset components of the env record. This maps anchors (strings) to
      pairs of dpaths and "validity" flags (bool ref). The validity flag is by default
      true, but can be set false either by the set function, when rebinding an anchor that
      is already mapped, or wholesale by the reset function. These validity flags are
      (assumed to be) shared with elab records (associated with the anchor).

      The set function (formerly set_free) can add new bindings of anchors to the map, or
      "rebind" an anchor that was already in the domain of the map, while "invalidating"
      the old binding by setting its validity flag to false.

      The get function, if applied to a mapped anchor (i.e. an anchor in the domain of !anchorMapRef)
      will return an elab generated from the (apath, validity) that the anchor maps to.
      When get is applied to an anchor that is not mapped, it causes a fatal error.

      So the "validity" flag associated with an anchor (in the !anchorMapRef map) will be set
      false when a binding of the anchor is removed (or overwritten) in the map by set or reset.

   2. The bound field of an env record is a mapping from anchors to a dpath (formerly an
      "anchorval"). Bindings are added to this mapping by the bind function, which
      operats "functionally" on an env record, producing a modified env. Bindings in the
      "bound" mapping are looked up (via StringMap.find) only in the mk_anchor function.

   Thus env is both an statefull environment "object" whose state consistes of the mapping stored
   in its anchorMapRef ref cell, and it is a functional environment (where binding new anchors produces
   a "new" environment with respect to the "bound" component. But the new env record returned by the
   bind function shares its state with the bind argument env.  Both aspects of the env are used in
   the mk_anchor function, which consults first the bound environment and if that fails, it consults
   the anchorMapRef mapping (i.e. the env state mapping).  This dual nature seems quite odd.


7. There is also an implicit map (now called FileInfoMap, formerly F0M) mapping
   fileInfo records to stableid int values based on the id field (FileId.id) of the
   fileInfo.

   The implicit fileInfo to stableid mapping is stored in a local ref ("known").

   Since the initial default id values in the fileInfo records are NONE, the intern
   operation will (through calling getFileId) redefine the info field in terms of an actual
   file system file_id (mediated through FileId.id).

   The fileInfo map is built and operated on by three functions: clear, intern, and sync.

      clear: resets the "known" ref to the empty map

      intern: generates a new (sequenctial) stableid integer value and binds it in the map
        and combines it with its fileInfo argument to return an "interned" file. If it is a
	new fileInfo key (presumably with id = ref NONE), its binding will be inserted into the map
	using fileInfoSidMap.insert, which will call getFileId, which will redefine the id field using
	the actual OS.FileSys file_id (actually PRESENT (file_id) or ABSENT filepath depending on
        whether the the file actually exists in the file system (?)).

      sync: resets the id field of all currently mapped fileInfo's to NONE (thus invalidating them?)
        and rebuilds a new fileInfoSidMap with the same (now invalidated!) fileInfos as keys.
	This "works" because the map insert function uses getFileId on the fileInfo keys, and this
	will generate new id values for the invalidated keys. Thus syn replaces the original
	file id (fileInfo.id) with actual file system file ids produced by OS.FileSys.fileId.

	The file_ids used in the reconstructed mapping will normally be the same as the file_ids
	used when a fileInfo record is interned. [When might they differ?]
	[So, is sync normally a no-op having no discernable effect?]


8. reanchor -- one of the compononets of an "elab"

      val reanchor: (anchor -> string) -> apath option

   Takes a conversion function mapping an anchor to a string (normally a filepath?) and returns a
   apath option.  In most cases this is initialized to a trivial (fn _ => NONE): for instance:

     bogus_elab,
     absoluteElab

   Where is the reanchor component of an elab actually applied?

   osstring_reanchored (only application)

    fun osstring_reanchored cvt file =
	let val {reanchor, ...} = fileToElab file
	 in Option.map (FI.canonical o apathToFilepath) (reanchor cvt)
	end

   osstring_reanchored is exported from SrcPath and used only once, in main/filename-policy:

    (* separate_generic : {bindir: string, bootdir: string} -> policy *)
    (* SP = SrcPath *)
    fun separate_generic { bindir: string, bootdir: string } =
	let fun shiftname (root: string) (file: SP.file) =
		let fun anchor_cvt (anchor: string) = OS.Path.concat (root, anchor)
		 in case SP.osstring_reanchored anchor_cvt file
		      of SOME s => s
		       | NONE => (Say.say ["Failure: ", SP.descr p,
					   " is not an anchored path!\n"];
				  EM.impossible "separate-generic -- bad path")
		end
	 in mkPolicy (shiftname bindir, shiftname bootdir, true)
	end

     Initial reanchor values:

         (fn () => NONE)   -- trivial default

       env.get initialization:

         (fn (cvt: string -> filepath) => SOME (filepathToApath (cvt anchor))

     Anchor "tranforms":

       osstring_dpath_reanchored:

	 fun osstring_reanchored cvt file =
	     let val {reanchor, ...} = fileToElab file
	      in Option.map (FI.canonical o apathToFilepath) (reanchor cvt)
	     end

       parentElab:

         fun parentElab ({ pp, reanchor }: elab) : elab  =
	     { pp = parentApath pp,
	       reanchor = Option.map parentApath o reanchor }

      extendElab:

	 fun extendElab arcs { pp, reanchor } =
	     { pp = extendApath arcs pp,
	       reanchor = Option.map (extendApath arcs) o reanchor }


   A datatype to encode the reanchor construction

      datatype reanchor  (* relative to an anchor *)
        = Anchor of anchor
        | Parent of reanchor
        | Extend of string list (*arcs*) * reanchor

      (* do_reanchor : (string -> filepath) -> reanchor -> apath
      fun do_reanchor (cvt: string -> filepath) reanchor =
        case reanchor
	  of Anchor of anchor => filepathToApath (cvt anchor)
           | Parent of reanchor => parentApath (do_reanchor cvt reanchor)
           | Extent of (arcs, reanchor) => extendApath arcs (do_reanchor cvt reanchor)

      Actually, parentApath and extendApath operate only on the revarcs component of a
      apath, leaving the vol and isAbs fields alone, so possibly the Parent case could
      just do a tl and the Excend case could just do a revappend on the revarcs of the 
      relevant apaths ("on the way out" after having recursed down to the anchor in
      the final Anchor node (Anchor a) to which the cvt : string -> filepath is applied
      to give the new "anchor point".

      The reanchor encodes a kind of "file system delta" down the path from an anchor
      point to a file that is located relative to that anchor (i.e. relative to an ANCHOR
      dir). We "apply" the reanchor to translate an anchor-relative path to a new (full)
      path relative to a shifted location of the anchor point given by the cvt function.
      
      The Parent constructor for reanchor is used in only one place, to trim the arcs for
      a DIR directory given by its fileInfo.  This may because of the way the DIR is introduced
      in the decodeFilepath [decode] function, where DIRs seem to serve to link "segments".
      [Have to understand what is going on in decodeFilepath better. Don't yet understand
       segments and the need to adjust arcs using a Parent reanchor in dirToReanchor.

   A couple things I still don't understand about reanchors and DIRs:

      * Why do we use the Parent reanchor to trim the last (innermost) arc when computing
        the (potential) reanchor for a DIR?

      * What are segments and why do they exist?  [in fun processFilepath]
        How are DIRs used to "link" segments together?
	Examples?


9. Phase 3 simplification

  The main change is to separate the two anchor environments embodied in the env type,
  namely the anchor --> apath environment (a "stateful" mapping), and the
  anchor --> dpath environment (the "bound" component of an env).

  Since newEnv is called in only two places to create new "env" environments
  (once in main/cm-boot.sml and once in bootstrap/btcompile.sml), and since these
  environments do not "co-exist" in a given CM process, it seems likely that the
  anchor --> apath environment can be implemented as a "global" stateful resource
  embodied in a structure ApathEnv inside SrcPath and used only locally.
  Most functions in the SrcPath signature that took an env parameter can instead
  internally access the apath environment through the ApathEnv structure and 
  no longer need an env parameter. [The exceptions are mk_anchor and bind, which
  still need to access the dpath environment.]

  The anchor --> dpath environment (the "bound" component of an env record),
  on the other hand, is treated functionally and can be given its own type with
  operations "bind" to add bindings and "mk_anchor" to access the environment to
  produce ANCHOR directories for a given anchor.

  There remains the question of whether we actually need two different anchor 
  environments.

  Note that apaths and dpaths could both be considered representations of locations
  in the file system (like filepaths, which are the "raw" form of file system locations).
  But dpaths contain dir components as the root of a arc list path, and the dir
  type expresses some of the "semantics" relevant to CM (i.e. paths relative to
  the root (ROOT), or the current working directory (CWD), or an anchor (ANCHOR) or
  relative to a directory (DIR) (?).

  A remaining mystery to be cleared up is the notion of "segments" in a filepath
  (separated by the #":" character) and how these are process to create dir values
  and dpaths in the decodeFilepath (formerly "decode") function. This gives rise
  to the need for a Parent constructor for the reanchor type, whose role is unclear
  to me. It would simplify reanchoring if this constructor could be eliminated.

  Conjecture: the fileInfos created in decodeFilepath designate CDFs. Then the Parent
  delta in the associated reanchors gives the relative path to the directory containing
  the CDF, and then the further reanchoring will be applied relative to that directory
  containing the CDF, not to the relative path of the CDF.

  An example: one place where decode[Filepath] is called is in cm-boot.sml, where it
  is applied to "filepaths" that are found at the beginning of lines in the PIDMAP
  file.  Here is an example of a "segmented" filepath found in PIDMAP:

    $SMLNJ-LIB/PrettyPrint/prettyprint-lib.cm:src/prettyprint.cm

  Note that the path for prettyprint.cm should be something like

    $SMLNJ-LIB/PrettyPrint/src/prettyprint.cm

  So if in decodeFilepath, the first segment is a DIR with arcs

    ["PrettyPrint", "prettyprint-lib.cm"]

  then this can be "Parent'ed" to ["PrettyPrint"] before being Extend'ed to

    ["PrettyPrint", "src", "prettyprint.cm"]

  So it looks like whatever is generating the PIDMAP file can produce such "segmented"
  file paths.  Is this is the only source of segmented file paths?


--------------------------------------------------------------------------------
Name changes and new names:

Renamed:

encode0		dpathToString (type changed)
bracket		show_anchors  -- parameter of encodeDpath (was encode0)
encode*		encodeFile
encodingIsAbsolute*  absoluteFilepath
file0 [type]	fileInfo  -- now a simple record type, not a datatype (defined using "withtype")
file0 [fun]	dpathToFileInfo
file*  		dpathToFile
context 	dir   -- fileInfo[file0] field and various argument names
look		elab  -- anchorInfo field (argument type for ANCHOR dir constructor); changed type
get_free	ApathEnv.get
set_free	ApathEnv.set
set0		setRelative
pp2name		apathToFilepath
F0M		FileInfoMap [structure, used in intern, clear, sync]
dir*		fileToDir
desc*		fileToFilepath
extend*		extendDpath
augPP		extendApath
dirPP		parentApath
pre*		fileToDpath
idOf		getFileId
raw*		mkDpath
decode*		decodeFilepath (segmented filepaths?)
pre0		fileInfoToDpath
pre*		fileToDpath
prepath	[type]	apath
prefile	[type]	dpath

Removed:

null_pp		.  -- removed, only used once and replaced with its defn
revalidateCwd*	.  -- removed, functionality merged into fun cwd
absElab		.
ord_key*	.  -- file (not needed)
compare*	.  -- compare for files based on stableid (not needed)
elab_file	.  -- no more elabs
elab_dir	.  -- "
augElab		.  -- "
dirElab		.  -- "
bosug_elab	.  -- "

Added:

.               anchorInfo     -- new type; argument type of ANCHOR dir constructor
.		dpathToFileInfo
.		dpathToFile
.		dirToReanchor
.		fileInfoToReanchor
.		dpathToReanchor
.		fileToReanchor

* Some of the former "elab" functions are replaced by "reanchor" functions.

* In some cases, an env parameter that provided access to the anchor -> apath
  environment is removed, and instead the global apath environment in ApathEnv
  is accessed directly. In other cases (like bind and mk_anchor) where the env 
  parameter provided access to the "bound" dpath environment, the env parameter
  is replaced by a dpathEnv parameter.

--------------------------------------------------------------------------------



