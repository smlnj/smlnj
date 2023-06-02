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
 * Revision, phase 5: Can we delete dpath and use only apath (and fpath) for locations?
 *   The key function seems to be decodeFpath, but there remains the question of whether the
 *   "segmented paths" represented using DIR in dpaths are essential for clients of SrcPath.
 *   See Note 11 at the end of this file.
 *)

structure SrcPath :> SRCPATH =
struct

local

(*  structure EM = ErrorMsg *)

  structure P = OS.Path     (* Basis *)
  structure F = OS.FileSys  (* Basis *)
  structure FI = FileId     (* srcpath-lib.cm: ./fileid.sml *)
  structure SM = StringMap  (* ../util/sources.cm: cm/util/stringmap.sml *)

  fun impossible (msgs: string list) = EM.impossible (concat ("SrcPath: " :: msgs))
  fun error (msgs: string list) = (Say.say msgs; Say.say ["\n"])

in

    exception Format
     (* This is raised only in the exported function unpickle.
        Why not use the existing UnpickleUtil.Format exception, as is done in other source files
	like stable/stabilize.sml and smlfile/skel-io.sml? [This would require importing
	"$pickle-lib.cm" in srcpath-lib.cm, as is done in cm-lib.cm.]
	There is one specific "handle SrcPath.Format", occuring in the file stable/stabilize.sml
        where the handler just raises UnpickleUtil.Format in place of this Format exception. *)

    (* anchor: the name of an anchor, e.g. "smlnj" for the anchor $smlnj *)
    type anchor = string

    (* stableid: integers used as "stable" ids of files(?) *)
    type stableid = int

    (* fpath: a file path in some format
     * by default, we assume the format that OS.Path uses
     * but sometimes the format will be OS native (Unix and Windows OSs only), and
     * sometimes it may contain (start with) an achor (starting with "$"). 
     * This ambiguity about the format of an fpath should be cleared up! *)
    type fpath = string 

    type path = string list  (* list of arc names *)
    type rpath = string list (* reversed list of arc names *)

    (* A apath is similar to the result of OS.Path.fromString except that
     * we keep the list of arcs in reversed order.  This makes adding
     * and removing arcs at the end easier.
     * A apath is essentially an "abstract syntax" for ordinary fpaths. *)
    type apath = { revarcs: rpath, vol: string, isAbs: bool }

    (* reanchor: relative location spec that can be used reconstruct a path relative to
     * an "anchor point" (specified by a fpath) for the anchor that occurs at the end
     * of the reanchor chain (Anchor a).
     * A reanchor is a sort of "delta path" from some given fpath/apath and the anchor
     * that that path is relative to.
     * If a apath is not relative to an anchor, then there is no reanchor for that apath;
     * i.e., apathToReanchor will return NONE. *)
    datatype reanchor
      = Anchor of anchor
      | Extend of path * reanchor

    (* dir datatype eliminated, not relevant if dpath is eliminated? *)
				    
    (* file: a file is a record with an apath, the locaction of the file, an fid, which
     * is a ref to a FI.fileId option, and an sid, which is a stableid (an int).
     * The stableid numbers are generated in the intern function, which also defines the
     * id field. The interned files are "recorded" in a finite set of type FileSet.set *)
    type file = {apath : apath, fid: FI.id option ref, sid: stableid}

    (* compareFile : file * file -> order *)
    (* This is used in paths/srcpathmap.sml to define maps over files, with ord_key = file *)
    fun compareFile ({sid=i1, ...}: file, {sid=i2, ...} : file) = Int.compare (i1, i2)


    (* converting: fpath <--> apath *)

    (* fpathToApath : fpath -> apath *)
    (* use OS.Path.fromString to parse the fpath, then convert result to a apath
     * by reversing the arcs *)
    fun fpathToApath (fpath: string) =
	let val { arcs, vol, isAbs } = P.fromString fpath
         in { revarcs = rev arcs, vol = vol, isAbs = isAbs }
	end

    (* apathToFpath : apath -> string *)
    fun apathToFpath ({ revarcs, vol, isAbs }: apath) =
	P.toString { arcs = rev revarcs, vol = vol, isAbs = isAbs }


   (* *********************************************************************************** *)
   (* current working directory (CDW) *)

    (* cwd_fpath : fpath ref *)
    (* a ref initialized with the raw fpath of the current working directory (CWD);
     * If the CWD changes, this record will be updated if and when the cwd function is called
     * subsequently.
     * Why store this value anyway?  Why not just call F.getDir whenever one needs to know
     * the current working directory (as a fpath or apath). The reason, apparently, is that
     * if the cwd function is called after a change in CWD, "clients" can be notified. *)
    val cwd_fpath = ref (F.getDir ())

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

    (* getCWD : unit -> fpath *)
    fun getCWD () = 
	let val newCwdFpath = F.getDir ()  (* get the current CWD fpath *)
	 in (* check whether CWD has changed since last time cwd_fpath was set *)
	    if newCwdFpath = !cwd_fpath (* check for change of CWD since last call *)
	    then () (* no change; cwd_fpath is validated, return newCwdFpath *)
	    else (* if not, CWD must have changed since the previous call of cwd *)
	      (cwd_fpath := newCwdFpath;
	       if !cwd_notify then app (fn c => c ()) (!clients) else ();
	       cwd_notify := false); (* why turn notifications off? *)
	    newCwdFpath
	end

    (* cwd : unit -> fpath ??? *)
    (* returns the fpath of the current working directory, making sure that cwd_info is
     * up-to-date. If the CWD has changed since the last call of cwd, and !cwd_notify is true,
     * then "clients" are notified of the change. But what if the CWD is changes but we don't
     * call cwd afterwards. Then the clients won't be notified of the change. There seems to
     * be a problem because of the "asynchronous" way of notifying clients! If there was a change
     * of CWD this time, cwd_notify will become false, so next time cwd is called clients won't
     * be notified if there is a following CWD change, unless someone set cwd_notify to true *)
    fun cwd () = getCWD ()


    (* translating to apath *)

    (* fileToApath [pre] : file -> apath
     * (equivalent to #apath applied to a file record)
     * exported
     * local: encodeFile*, fileToFpath, osstring_relative*
     * external: stable/stabilize.sml *)
    fun fileToApath ({apath,...}: file) : apath = apath


   (* *********************************************************************************** *)
   (* encoding dpaths as (possibly anchor annotated) fpath strings *)

    (* isChar : char -> char -> bool *)
    (* curried character equality; a utility function used in decodeFpath *)
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

    (* dpathToString [encode0] no longer needed ????, or do we need a fancier version of
     * apathToFpath that could conditionally add the anchor annotations/abbreviations of
     * (dpathToString true)? Note*)

    (* encodeFile : file -> string
     *   This function does _not_ include anchor annotations/abbreviations. *)
    fun encodeFile file = apathToFpath (fileToApath file)


   (* *********************************************************************************** *)
   (* translating between representations *)

    (* fileToFpath [desc] : file -> string
     * This is where anchor notation/abbreviation could be added (like dpathToString x true).
     * There is also the question of whether the apath component of the file should be a segmented
     * path [e.g. a list of apaths?]?
     * exported
     * local: none
     * external: 10 files *)
    fun fileToFpath file = apathToString (fileToApath file)

    (* dpathToFile : dpath -> file *)
    (* This actually produces what could be called a "prefile", an incomplete file record.
     *   sid = 0 is not a valid, allocated stable id because it is less than 1.
     * Could have sid be an int option instead. *)
    fun apathToFile (apath: apath) = {apath = apath, fid = ref NONE, sid = 0}


   (* *********************************************************************************** *)
   (* apath and reanchor functions (formerly involving elab) *)

  (* apath *)

    (* rootApath : apath *)
    fun rootApath (vol : string) : apath =
	{ revarcs = nil, vol = vol, isAbs = true }

    (* parentApath [dirPP] : apath -> apath *)
    (* returns the apath of the parent directory
     * not exported.
     * local uses: dirToApath, applyReanchor *)
    fun parentApath ({ revarcs = _ :: revarcs, vol, isAbs }: apath) : apath =
	  { revarcs = revarcs, vol = vol, isAbs = isAbs }
      | parentApath _ = impossible ["parentApath"]

    (* extendApath : rpath -> apath -> apath
     * not exported
     * local uses: dpathToApath, applyReanchor *)
    fun extendApath (rarcs: string list) ({ revarcs, vol, isAbs }: apath) : apath  =
	{ revarcs = append (rarcs, revarcs), vol = vol, isAbs = isAbs }

  (* reanchor *)

    fun apathToReanchor ({revarcs, ...}: apath) = extendApath revarcs

    (* fileToReanchor : file -> reanchor option
     * not exported
     * local: osstring_reanchored* *)
    val fileToReanchor = apathToReanchor o fileToApath


   (* *********************************************************************************** *)
   (* "interning files": assigning file_ids and generating stableids for files *)

    local (* interning files *)

       (* known: an internal reference to a finite set of files
	    (known: FileSet.set ref).
	* New bindings are created by the intern function.
	* intern uses uses FileInfoMap.insert, which uses getFileId, which uses FileId.fileId
	*   to update the id field of the fileInfo record to a FileId.id computed by
	*   OS.FileSys.fileId. *)

	(* getFileId [idOf] : fileInfo -> FI.id *)
	(* returns the fileId associated with the file, updating the id field if it was NONE *)
	fun getFileId (file as { apath, fid, ... }: file) : FI.id =
	      (case !fid
		 of SOME id => id
		  | NONE =>
		      let val id = FI.fileId (apathToFpath apath)
		       in fid := SOME id; id
		      end)

	(* compareFile : file * file -> order *)
	(* compare files by comparing their id fields as accessed/computed by getFileId *)
	fun compareFile (f1: file, f2: file) =
	    FI.compare (getFileId f1, getFileId f2)

	structure FileSet =
	  RedBlackSetFn (type ord_key = file
			 val compare = compareFile)

	(* known: (a reference to) a finite set of files that have been interned *)
	val known : FileSet.set ref = ref FileSet.empty
	val nextStableId = ref 1  (* "allocated" stable ids are >= 1 *)

    in

        (* clear : unit -> unit *)
        fun clear () = known := FileSet.empty

	(* intern : file -> file *)
	(* generate a stableid (sequentially) to add to a fileInfo to make a file. If the fileInfo
	 * has already been interned before, just return the associated stableid. If it is new
	 * generate the next stableid and also (through the getFileId function called by insert)
	 * define the id field of the fileInfo. *)
	fun intern (file as {apath, sid, fid}: file) : file =
	    if FileSet.member (!known, file)
	    then file
	    else let val stableid = !nextStableId  (* generate a new stableid *)
		     val file' = {apath = apath, sid = stableid, fid = fid}
		  in nextStableId := stableid + 1;
		     known := FileSet.add (!known, file');
		       (* add the new file with stableid to known *)
		     file'  (* return the interned file *)
		 end

	(* sync : unit -> unit *)
	(* sync causes the file ids of all the interned files to be reset to the actual
	 *   file system file ids (obtained from OS.FileSys.fileId)
	 * What is sync expected to do to the stable ids of the files in !known?
         * Should new stable ids be generated by sync? *)
	fun sync () =
	    let val files = !known
		fun killId ({ fid, ... }: file) = fid := NONE
		fun reinsert (f, s) = FileSet.add (s, f)
	     in FileSet.app killId files;
		(* This will cause the id fields to be reassigned using the file systems file ids
		 * when they are accessed via getFileId for the insert operation. *)
		known := FileSet.foldl reinsert FileSet.empty files  (* DBM: why? *)
	    end

    end (* interning files *)


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
     * exported SrcPath functions bind, decodeFpath, and unpickle.
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
	  val get : anchor -> apath option
	  val set : anchor * apath option -> unit
	  val defined : anchor -> bool
	  val reset : unit -> unit
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

      (* get : anchor -> apath option *)
      (* look up anchor in !anchorMapRef. If found, return a new elab for the anchor
       * containing the same apath and validity as the existing binding. If not
       * found (anchor is not in the domain of !anchorMapRef), produces an undefined
       * anchor fatal error (impossible, compiler bug. So get should only be called with
       * an anchor that is known to be defined. *)
      fun get anchor = find anchor

      (* set : anchor * apath option -> unit *)
      (* If apathOp is SOME apath, binds or rebinds anchor to apath.
       * If apathOp is NONE, unbinds anchor in !anchorMapRef. *)
      fun set (anchor, apathOp) : unit =
	  case find anchor
	    of SOME _ =>
		 anchorMapRef :=
		   (case apathOp
		      of SOME pp => SM.insert (!anchorMapRef, anchor, pp) (* rebind *)
		       | NONE => #1 (SM.remove (!anchorMapRef, anchor))) (* remove *)
			 (* this can't raise NotFound because find returned SOME *)
	     | NONE =>
		 (case apathOp
		    of SOME apath => anchorMapRef := SM.insert (!anchorMapRef, anchor, apath) (* bind *)
		     | NONE => ()) (* do nothing *)

      (* reset : unit -> unit *)
      (* wipe out the contents of the apath environment by setting anchorMapRef to SM.empty *)
      fun reset () = anchorMapRef := SM.empty  (* reset anchorMapRef to the empty map *)

    end (* structure ApathEnv *)

    (* get_anchor : anchor -> fpath option
     * maps an anchor to the fpath of its "anchor point", if it is defined *)
    fun get_anchor (anchor: anchor) =
	Option.map apathToFpath (ApathEnv.get anchor)

    (* setRelative [set0]: anchor * fpath option * fpath-> unit *)
    fun setRelative (anchor: anchor, fpathOp: fpath option,
		     relativeTo: fpath) =
	let fun fp_pp (fpath: fpath) : apath =
		let val fp1 = P.mkAbsolute {path = fpath, relativeTo = relativeTo}
		    val fp2 = if P.isAbsolute fpath then fpath else fp1
		 in fpathToApath fp2
		end
	 in ApathEnv.set (anchor, Option.map fp_pp fpathOp)
	end

    (* set_anchor : anchor * fpath option -> unit *)
    (* When fpathOp is SOME, binds a corresponding apath to the anchor in env;
     * when fpathOp is NONE, deletes the anchor and its binding from env.
     * Exported and called externally (3 times) in main/cm-boot.sml *)
    fun set_anchor (anchor, fpathOp) =
	setRelative (anchor, fpathOp, F.getDir ()) before sync ()

    (* reset_anchors : unit -> unit *)
    (* exported
     * external: main/cm-boot.sml (in resetPathConfig) *)
    fun reset_anchors () = (ApathEnv.reset (); sync ())


    (* dpath environments *)    

    type apathEnv = apath SM.map

    val emptyApathEnv : apathEnv = SM.empty 
    (* bindAnchors : dpathEnv -> (anchor * dpath) list -> dpathEnv *)
    (* produces a new env record with only the "bound" field altered.
     * Anchors are bound to corresponding dpaths, with these bindings being
     * added to the existing "bound" mapping.
     * exported
     * external: main/general-params.sml, elsewhere? *)
    fun bindAnchors (pfenv: dpathEnv) (alist: (anchor * dpath) list) : dpathEnv =
	let fun folder ((anchor, dpath), env) = SM.insert (env, anchor, dpath)
	 in foldl folder pfenv alist
	end


    (* lookAnchor : apathEnv * anchor -> apath option *)
    (* make an anchor directory: not exported.
     * Other sorts of directories are made using the directory constructors CWD, ROOR, DIR
     * not exported
     * local: native, standard, unipickle, decodeFpath *)
    fun lookAnchor (apathEnv, anchor: anchor) : apath option =
	case SM.find (apathEnv, anchor)
	  of NONE => ApathEnv.get anchor (* anchor is not in apathEnv, try ApathEnv *)
	   | apathOp => apathOp



    (* ******************************************************************************** *)
    (* processSpecFile : fpath -> TextIO.instream -> unit *)
    (* Processing a "spec file" (as an instream) relative to a given base fpath.
     * What are "spec files" (or instreams) and what is their format?
     * This processes an instream ("relative" to the given baseFpath) line by line,
     * interpretting each line as a comment, a blank line, or a "command".
     * What are the commands and what do they do?
     *   "#" ... -- a comment line, ignored
     *   !standard -- switch to "standard" treatment of fpaths (isnative becomes false)
     *   !native -- switch to "native" treatment of fpaths (no parsing) (isnative becomes true)
     *   anchor fpath  -- add a --> fpathToApath fpath to ApathEnv (relative to the directory
     *     part of baseFpath)
     *   anchor -- delete anchor from the ApathEnv environment
     *   -  -- reset ApathEnv to the empty environment *)
    fun processSpecFile (baseFpath: fpath) =
	let val local_dir : fpath = P.dir (F.fullPath baseFpath)

	    fun set (anchor, fpathOp) =
		setRelative (anchor, fpathOp, local_dir)

	    (* mknative : bool -> string -> string *)
	    fun mknative true fpath = fpath
	      | mknative false fpath =
		  let fun mkFpath (abs, arcs) =
			  P.toString { vol = "", isAbs = abs, arcs = arcs }
		   in case String.fields (fn c => c = #"/") fpath
		        of "" :: arcs => mkFpath (true, arcs)
		         | arcs => mkFpath (false, arcs)
		  end

	    (* work: TextIO.instream -> ? *)
	    fun work (s: TextIO.instream) =
		(* loop processes each and every line of the instream *)
		let fun loop isnative =
			case TextIO.inputLine s
			  of NONE => ()
			   | SOME line =>
			       if String.sub (line, 0) = #"#" then loop isnative
			       else case String.tokens Char.isSpace line
				      of ["!standard"] => loop false
				       | ["!native"] => loop true
				       | [a, d] =>
					   (set (a, SOME (mknative isnative d));
					    loop isnative)
				       | ["-"] => (ApathEnv.reset (); loop isnative)
				       | [a] => (set (a, NONE); loop isnative)
				       | [] => loop isnative
				       | _ => (error [fpath, ": malformed line (ignored)"];
					       loop isnative)
		 in loop true
		end

	 in work
	end (* fun processSpecFile *)


    (* ******************************************************************************** *)
    (* parsing fpaths *)

    (* What are the specifications that define "standard" and "native" file paths? *)

    datatype stdspec
      = RELATIVE of string list
      | ABSOLUTE of string list
      | ANCHORED of anchor * string list

    (* parseFpathNative [parseNativeSpec]: fpath -> stdspec *)
    (* ASSERT: fpath is not the empty string *)
    fun parseFpathNative (fpath: fpath) =
	let val {isAbs, arcs, vol} = P.fromString fpath
         in case arcs
	      of [""] => impossible ["parseFpathNative -- zero-length arc name"]
	       | [] => impossible ["parseFpathNative -- no fields"]
	       | (["$"] | "$"::""::_) =>
		   (error ["invalid zero-length anchor name in: ", fpath];
		    RELATIVE arcs)
	       | "$" :: (arcs' as (arc1 :: _)) => (* "$/arc1/..." *)
		   if isAbs
		   then RELATIVE arcs          (* e.g. "/$/a/b" ? *)
		   else ANCHORED (arc1, arcs')  (* e.g. "$/a/b" ? *)
	       | arc1 :: arcn =>
		   if String.sub (arc1, 0) = #"$"
		   then ANCHORED (String.extract(arc1, 1, NONE), arcn)
		   else if isAbs
		        then ABSOLUTE arcs
		        else RELATIVE arcs
	     (* end case *)
        end

    (* parseFpathStandard [parseStdspec]: (string -> unit ) -> fpath -> stdspec *)
    (* ASSERT: fpath is not the empty string *)
    fun parseFpathStandard (fpath: fpath) =
	let fun delim #"/" = true
	      | delim #"\\" = true
	      | delim _ = false
	    fun transl ".." = P.parentArc
	      | transl "." = P.currentArc
	      | transl arc = arc
	 in case map transl (String.fields delim fpath)
	      of [""] => impossible ["parseFpathStandard -- zero-length name: ", fpath]
	       | [] => impossible ["parseFpathStandard -- no fields", fpath]
	       | "" :: arcs => ABSOLUTE arcs
	       | arcs as (["$"] | "$" :: "" :: _) =>
		   (error ["invalid zero-length anchor name in: ", fpath];
		    RELATIVE arcs)
	       | "$" :: (arcs as (arc1 :: _)) => ANCHORED (arc1, arcs)
	       | arcs as (arc1 :: arcn) =>
		   if String.sub (arc1, 0) <> #"$" then RELATIVE arcs
		   else ANCHORED (String.extract (arc1, 1, NONE), arcn)
	end (* fun parseFpathStandard *)

    (* ???? *)
    (* native : apathEnv -> fpath -> apath *)
    fun native apathEnv fpath =
          (case parseFpathNative fpath
             of RELATIVE arcs => {isAbs = false, vol = "", revarcs = rev arcs}  (* dir? *)
              | ABSOLUTE arcs => {isAbs = true, vol = "", revarcs = rev arcs}
              | ANCHORED (anchor, arcs) =>
		  (case lookAnchor apathEnv
		     of NONE => impossible ["native"]
		      | SOME apath => extendApath arcs)
          (* end case *))

    (* ???? *)
    (* standard : apathEnv -> fpath -> apath *)
    fun standard apathEnv fpath =
	  (case parseFpathStandard fpath
	     of RELATIVE arcs => {isAbs = false, vol = "", revarcs = rev arcs} (* dir? *)
	      | ABSOLUTE arcs => {isAbs = true, vol = "", revarcs = rev arcs}
	      | ANCHORED (anchor, arcs) => {dir = lookAnchor (pfenv, anchor), arcs = arcs}
		  (case lookAnchor apathEnv
		     of NONE => impossible ["standard"]
		      | SOME apath => extendApath arcs)
	  (* end case *))


    (* *********************************************************************************** *)
    (* the "osstring" family of functions
     *  these translate files, dpaths, dirs to strings and allow for the "reanchoring"
     *  of "anchored" paths.
     *)

    (* osstring : file -> string *)
    val osstring = FI.canonical o apathToFpath o fileToApath

    (* osstring_apath : apath -> string *)
    fun osstring_apath (apath) : fpath =
	FI.canonical (apathToFpath (extendApath arcs apath)

    (* osstring' : file -> string *)
    fun osstring' f =
	let val oss = osstring f
	 in if P.isAbsolute oss
	    then let val ross = P.mkRelative { path = oss, relativeTo = (!cwd_fpath) }
		  in if size ross < size oss then ross else oss
		 end
	    else oss
	end

    (* applyReanchor : (string -> fpath) -> reanchor -> apath? *)
    (* recurses down to the anchor root, maps that to a fpath using cvt, converts
       that fpath to a apath (fpathToApath) then recursively applies the transforms
       of the reanchor layers to yield a modified apath. *)
    fun applyReanchor (cvt: string -> fpath) reanchor =
	case reanchor
	  of Anchor anchor => fpathToApath (cvt anchor)
	   | Extend (arcs, reanchor) => extendApath arcs (applyReanchor cvt reanchor)

    (* osstring_reanchored : (string -> string) -> file -> fpath option *)
    (* used once in main/filename-policy.sml (FilenamePolicyFn) *)
    fun osstring_reanchored cvt file =
	let val reanchorOp = fileToReanchor file
	 in case reanchorOp
	    of NONE => NONE
	     | SOME reanchor => 
		 SOME (FI.canonical (apathToFpath (applyReanchor cvt reanchor)))
	end

  (* ???? check what is required from these functions by clients
    (* osstring_dpath_relative : dpath -> fpath *)
    fun osstring_dpath_relative (pf as { dir, arcs }: dpath) =
	case dir
	  of DIR _ => FI.canonical (P.toString { arcs = arcs, vol = "", isAbs = false })
	   | _ => osstring_dpath pf

    (* osstring_relative : file -> fpath *)
    val osstring_relative = osstring_dpath_relative o fileToApath
  *)

    (* tstamp : file -> TSamp.t *)
    fun tstamp f = TStamp.fmodTime (osstring f)


    (* *********************************************************************************** *)
    (* "pickling" and "unpickling" apaths
	These operations are now trivial.
        QUESTIONS:
          What do we really need to pickle/unpickle?
	  Do we need to pickle/unpickle relative to a file as the original version did?
        SrcPath.pickle is called twice in stable/stabilize.sml.
     *)

    (* apathToFileId : apath -> FI.fileId *)
    fun apathToFileId (apath: apath) = FI.fileId (apathToFpath apath)

    (* fileToFileIdOp : file -> FI.fileId option *)
    fun fileToFileId ({fid,...}: file) = !fid

    (* pickle : apath -> string list *)
    fun pickleApath ({ isAbs, vol, revarcs } : apath) = Bool.toString isAbs :: vol :: revarcs

    (* unpickle :  string list -> apath *)
    fun unpickle (isAbs :: vol :: revarcs) =
  	  (case Bool.fromString isAbs
	     of SOME b => {revarcs = revarcs, isAbs = b, vol = vol}
  	      | NONE => raise Format)
      | unpickle _ = raise Format


    (* *********************************************************************************** *)
    (* decoding "fpaths" to files (relative to a given dpathEnv) *)

    (* transArc: string -> string *)
    (* What does transArc protect against?
     * Is it roughly reversing the effect of a transSpecial performed earlier? *)
    fun transArc "." = P.currentArc  (* = "." *)
      | transArc ".." = P.parentArc  (* = ".." *)
      | transArc a = transCode a  (* replace numberic escape codes *)

    (* The fpath argument is a "segmented fpath" (sequence of fpaths separated by ":").
     * What are "segments"? (Where are they documented? Where are they introduced?)
         One place where "segmented" paths are introduced is in the PIDMAP file.
	 Any others?
       What is the purpose of segments?
         Chaining where one CDF serves as a proxy for another local CDF?
	 Known example is "$SMLNJ-LIB/PrettyPrint/prettyprint-lib.cm:src/prettyprint.cm".
       The apathEnv argument is a "local" overlay over the global ApathEnv anchor environment. *)

    (* decodeFile [decode] : apathEnv -> fpath -> file *)
    fun decodeFpath (apathenv: apathEnv) (fpath: fpath) : apath list =
	let (* firstseg : string -> apath *)
	    fun firstseg (seg : string) =
		(case map transArc (String.fields (isChar #"/") seg)
		   of nil => impossible ["decodeFpath: no fields in segment 0"]
		    | arc0 :: arcs =>
		      if arc0 = ""  (* fpath starts with #"/" *)
		      then {isAbs = true, vol = "", revarcs = rev arcs}
		      else let val char0 = String.sub (arc0, 0) (* 1st char of arc0 *)
			       val arc0' = String.extract (arc0, 1, NONE)
			    in case char0 
				 of #"%" => (* arc0' is a volume name *)
				      {isAbs = true, vol = arc0', revarcs = rev arcs}
				  | #"$" => (* arc0' is an anchor *)
				      let val {isAbs, vol, revarcs} = lookAnchor (apathenv, arc0')
				       in {isAbs = isAbs, vol = vol,
					   revarcs = revappend (arcs, revarcs)}
				      end
	                          | _ => {isAbs = false, vol = "", revarcs = rev (arc0 :: arcs)}
	                   end)

	    (* segToApath: string -> apath *)
            fun segToApath (seg: string) =
		{isAbs = false, vol = "",
		 revarcs = rev (map transArc (String.fields (isChar #"/") seg))}

	    fun combineApaths nil = impossible ["combineApaths"]
	      | combineApaths [apath] = apath
	      | combineApaths (apaths as {isAbs, vol, revarcs} :: rest) = 
		  let val rarcss = map #revarcs apaths
		      fun combine [rarcs] = rarcs
			| combine (rarcs::rest) = revappend (tl rarcs, combind rest)
		  in {isAbs = isAbs, vol = vol, revarcs = combine rarcss}
		  end

	 in case String.fields (isChar #":") fpath
	      of nil => impossible ["decodeFpath: no segments"]
	       | seg0 :: segs =>
		   let val segApaths = fistset seg0 :: map segToApath segs
		       val fullApath = combineApaths segApaths
		   in intern (apathToFile fullApath)
		   end

	end (* fun decodeFile *)

    (* absoluteFpath : fpath -> bool *)
    (* does fpath start with $"/" or #"%"? Then it is absolute. *)
    fun absoluteFpath s =
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

   1. fpath strings (using the "OS-indepent" notation used in OS.FileSys and OS.Path),

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

     fpath (string) --[P.fromString]--> apath --[apathToFpath]--> fpath

     dpath --[intern?]--> fileInfo (adding undefined file-id)
             --[intern?]--> file (adding stableid and defining file-id)

Where do dpaths come from?  Where is the link between apaths and dpaths?

Dpaths are created (directly) by the function decodeFpath, which (with the help of the
env) translates fpaths directly to (interned) files.  There is no direct translation from
the syntactic fpath/apath representations to the dpath representation, or in particular,
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
      "bound" mapping are looked up (via StringMap.find) only in the lookAnchor function.

   Thus env is both an statefull environment "object" whose state consistes of the mapping stored
   in its anchorMapRef ref cell, and it is a functional environment (where binding new anchors produces
   a "new" environment with respect to the "bound" component. But the new env record returned by the
   bind function shares its state with the bind argument env.  Both aspects of the env are used in
   the lookAnchor function, which consults first the bound environment and if that fails, it consults
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
	the actual OS.FileSys file_id (actually PRESENT (file_id) or ABSENT fpath depending on
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

   Takes a conversion function mapping an anchor to a string (normally a fpath?) and returns a
   apath option.  In most cases this is initialized to a trivial (fn _ => NONE): for instance:

     bogus_elab,
     absoluteElab

   Where is the reanchor component of an elab actually applied?

   osstring_reanchored (only application)

    fun osstring_reanchored cvt file =
	let val {reanchor, ...} = fileToElab file
	 in Option.map (FI.canonical o apathToFpath) (reanchor cvt)
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

         (fn (cvt: string -> fpath) => SOME (fpathToApath (cvt anchor))

     Anchor "tranforms":

       osstring_dpath_reanchored:

	 fun osstring_reanchored cvt file =
	     let val {reanchor, ...} = fileToElab file
	      in Option.map (FI.canonical o apathToFpath) (reanchor cvt)
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

      (* applyReanchor : (string -> fpath) -> reanchor -> apath *)
      fun applyReanchor (cvt: string -> fpath) reanchor =
        case reanchor
	  of Anchor of anchor => fpathToApath (cvt anchor)
           | Parent of reanchor => parentApath (applyReanchor cvt reanchor)
           | Extent of (arcs, reanchor) => extendApath arcs (applyReanchor cvt reanchor)

      Actually, parentApath and extendApath operate only on the revarcs component of a
      apath, leaving the vol and isAbs fields alone, so possibly the Parent case could
      just do a tl and the Excend case could just do a revappend on the revarcs of the 
      relevant apaths ("on the way out" after having recursed down to the anchor in
      the final Anchor node (Anchor a) to which the cvt : string -> fpath is applied
      to give the new "anchor point".

      The reanchor encodes a kind of "file system delta" down the path from an anchor
      point to a file that is located relative to that anchor (i.e. relative to an ANCHOR
      dir). We "apply" the reanchor to translate an anchor-relative path to a new (full)
      path relative to a shifted location of the anchor point given by the cvt function.
      
      The Parent constructor for reanchor is used in only one place, to trim the arcs for
      a DIR directory given by its fileInfo.  This may because of the way the DIR is introduced
      in the decodeFpath [decode] function, where DIRs seem to serve to link "segments".
      [Have to understand what is going on in decodeFpath better. Don't yet understand
       segments and the need to adjust arcs using a Parent reanchor in dirToReanchor.

   A couple things I still don't understand about reanchors and DIRs:

      * Why do we use the Parent reanchor to trim the last (innermost) arc when computing
        the (potential) reanchor for a DIR?

      * What are segments and why do they exist?  [in fun processFpath]
        How are DIRs used to "link" segments together?
	Examples?


------------------------------------------------------------------------------------------
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
  no longer need an env parameter. [The exceptions are lookAnchor and bind, which
  still need to access the dpath environment.]

  The anchor --> dpath environment (the "bound" component of an env record),
  on the other hand, is treated functionally and can be given its own type with
  operations "bind" to add bindings and "lookAnchor" to access the environment to
  produce ANCHOR directories for a given anchor.

  There remains the question of whether we actually need two different anchor 
  environments.

  Note that apaths and dpaths could both be considered representations of locations
  in the file system (like fpaths, which are the "raw" form of file system locations).
  But dpaths contain dir components as the root of a arc list path, and the dir
  type expresses some of the "semantics" relevant to CM (i.e. paths relative to
  the root (ROOT), or the current working directory (CWD), or an anchor (ANCHOR) or
  relative to a directory (DIR) (?).

  A remaining mystery to be cleared up is the notion of "segments" in a fpath
  (separated by the #":" character) and how these are process to create dir values
  and dpaths in the decodeFpath (formerly "decode") function. This gives rise
  to the need for a Parent constructor for the reanchor type, whose role is unclear
  to me. It would simplify reanchoring if this constructor could be eliminated.

  Conjecture: the fileInfos created in decodeFpath designate CDFs. Then the Parent
  delta in the associated reanchors gives the relative path to the directory containing
  the CDF, and then the further reanchoring will be applied relative to that directory
  containing the CDF, not to the relative path of the CDF.

  An example: one place where decode[Fpath] is called is in cm-boot.sml, where it
  is applied to "fpaths" that are found at the beginning of lines in the PIDMAP
  file.  Here is an example of a "segmented" fpath found in PIDMAP:

    $SMLNJ-LIB/PrettyPrint/prettyprint-lib.cm:src/prettyprint.cm

  Note that the path for prettyprint.cm should be something like

    $SMLNJ-LIB/PrettyPrint/src/prettyprint.cm

  So if in decodeFpath, the first segment is a DIR with arcs

    ["PrettyPrint", "prettyprint-lib.cm"]

  then this can be "Parent'ed" to ["PrettyPrint"] before being Extend'ed to

    ["PrettyPrint", "src", "prettyprint.cm"]

  So it looks like whatever is generating the PIDMAP file can produce such "segmented"
  file paths.  Is this is the only source of segmented file paths?


------------------------------------------------------------------------------------------
10. Phase 4 simplification

* Rename "file location" types:

    path = string list (arc list, with arcs ordered from outer to inner)
    rpath = string list (reverse arc list, with arcs ordered from inner to outer)
    fpath = string (file paths in some "standard" format)

    prepath -> apath (for "abstract path")
    prefile -> dpath (for "directory-based" path)

** fpath (assume OS = unix for discussion, FS = OS file system)

  The fpath format is a bit ambiguous. I think it would be correct to say that
  this includes "normal" absolute and relative (canonical?) paths, as described in
  the OS.Path Basis documentation, plus file paths starting with an anchor (a non-empty
  arc string preceded by the #"$" character). Internally we should assume that we are
  working with cannonical paths, but user-specified paths may not be cannonical.

  A relative fpath must be interpreted relative to some implicit file system "directory",
  by default the current working directory (CWD).

  Non-relative fpaths may be "absolute", meaning that they start from the root directory
  (ROOT "": dir), or anchored, meaning that they start from a directory designated
  by an anchor name.

  It is assumed that in an fpath, parsed into arcs, all but the last arc will "designate"
  a directory, with respect to which the following arc will be interpreted.

** apath (aka prepath).
 
  This is a sort of "abstract" file path. The property of being absolute or relative is
  given by the "isAbs" field, and if absolute it is rooted as the volume designated by the
  "vol" field. If the last (innermost) arc is an anchor (starts with "$"), then we assume
  that the apath is relative (what would "/$a" mean?).  Only the innermost arc (last element
  of revarcs) should be an anchor arc (what would $a/b/$c mean?).

  Note that it is trivial to map back and forth between fpaths and apaths.  Given canonicalization
  of paths, these location representations are isomorphic.

** dpath ("directory-based" path, aka prefile)

  The arcs are interpreted relative to the "dir" component. In the case of the CWD, ROOT,
  and ANCHOR dir variants, it seems clear what this means (these should correspond with
  CWD-relative, absolute, and anchor-relative file paths).

  The DIR variant is more complicated. DIR constructors serve to append, or chain, the arc-paths
  of the current "directory" point with those of another, predecessor, dpath, except that the
  predicessor path may (usually, always) actually designates a nondirectory file, such as a
  CDF (CM description file).

     dpath0 = D0 % a % b % c  =  {dir = D0, arcs = ["a", "b, "c"]}

     dpath1 = {dir = dpath0, arcs = ["d", "e"]}

  Here it will be the case that <D0>/a/b/c is not a directory, but <D0>/a/b will be,
  and 

     dpath1 ~~ <D0>/a/b/d/e = {dir = D0, arcs = ["a", "b", "c", "d", "e"]}

  Example: the segmented path

     $SMLNJ-LIB/PrettyPrint/prettyprint-lib.cm:src/prettyprint.cm

  found in a PIDMAP file, is decoded into the dpath

     {dir = {dir = A0, arcs = ["PrettyPrint", "prettyprint-lib.cm"]},
      arcs = ["src", "prettyprint.cm"]}

     ==>

     {dir = A0, arcs = ["PrettyPrint", "src, "prettyprint.cm"]}

  where 

     A0 = ANCHOR {name = "SMLNJ-LIB",
                  apath = </User/dbm/sml/Dev/github/smlnj/smlnj-lib>,
		  dpathOp = NONE(?)}

  Note: DIR seems like a slightly misleading constructor name, since DIR constructs 
  may not, or generally do not, correspond with directories. The usual situation is that
  they are "links" of a sort to extend the previous dir at (or near) the end of a previous
  dpath (down a path from a "parent" dir).

  There is another conceptual mismatch, since an ANCHOR would seem to designate a directory
  that is the anchor point of its anchor (found at its apath).  But DIR seems to designate
  a file (generally/always a CDF).  Of course CWD and ROOT dirs obviously are associated
  with directories.

** "anchor points", anchor environments

  I will call the FS location designated by an anchor the "anchor point" for that anchor,
  and it can be described by an fpath, an apath, or a dpath.

  There is a global, "base" environment mapping anchor strings to the corresponding
  anchor point represented by an apath. This global environment is implemented by the
  ApathEnv structure, whose interface consists of the functions

     get : anchor -> apath option         -- NONE if not bound
     set : anchor * apath option -> unit  -- bind, rebind, or remove an anchor from the environment
     defined : anchor -> bool	          -- is the anchor bound in the evironment
     reset : unit -> unit		  -- reset the global environment to the empty environment

  This environment obviously uses apaths to designature the anchor point of an anchor.

  The second form of anchor environment is given by the type dpathEnv, which the the type
  of "functional" environments that map anchors to dpaths.

  My conjecture is that the dpathEnv environments are used to implement "temporary" or 
  "dynamic" bindings associated with "bind" declarations in CFDs. Both environments are
  accessed in the lookAnchor function, where the dpathEnv parameter has precedence over the
  global ApathEnv environment.

  An obvious question is why the global environment maps to apaths and the "dynamic" or
  local environments map to dpaths? Why couldn't they both map to the same kind of 
  file-locator type?

  Note that it is fairly easy to map from dpaths to apaths (see the dpathToApath function).
  But no inverse function that maps apaths to dpaths is defined, though one could use a variant
  of the decodeFpath function to map from apaths to dpaths with the helpe of a dpathEnv,
  which is "needed" to create ANCHOR dirs for anchor arcs (& anchored apaths).
  It is obvious that anchor environments are relevant to translating anchored apaths (or fpaths)
  to dpaths, which have to include dir components. So a translation function mapping dpaths to
  apaths probably must require a dpathEnv argument.

  A related question is: Why do the ANCHOR dir values need both an apath and a dpath (optional)?
  This appears to be simply because the anchor point of an ANCHOR dir may come from either
  the global ApathEnv environment, or a local dpathEnv environment.


* Interning, creating files

  Earlier versions had a "fileInfo" type which was a precursor type for the file type.

  In this version of clarified SrcPath, we dispense with fileInfo and build "incomplete"
  or preliminary files.  A file is a record contining 3 fields

     dpath: dpath -- the location of the file (would apath work as well?)
     fid: FileId.id option ref -- a place to put the actual FS file_id if it exists
          (via FileId)
     sid: stableid -- the integer stable it, generated for the file in the intern function

  Preliminary representations of files ("prefiles"?) are built with "dummy" values
  for fid (ref NONE) and sid (0, an out-of-range stable id value, because < 1).
  See dpathToFile.

  The intern function is based on a stateful set of interned files (known : FileSet.set ref).
  When a file (or "prefile"?) is interned the first time, its file_id is determined
  (via FileId, using OS.FileSys.fileId) and stored in the fid field, and a new stableid is
  generated and stored in the sid field. The intern function returns the completed file.

  The "sync" funcion "rebuilds" this set of interned files. [Why does this need to be done?]


* Reanchors and reanchoring

  See comments in the code.  Reanchors are built primarily from files (actually the
  dpaths of files).  They are used map to new apaths/fpaths given a new anchor to 
  apath/fpath association.

* What has been discarded

  - elab

  - fileInfo

  - reanchor as an elab component (functional representation)
      (now a computed attribute of a file or dpath)

  - the "combined" env, now split into ApathEnv environment (state) and the dpathEnv
    type.  dpathEnv environments behave "functionally" (based on StringMap).

  - prepath, prefile renamed to apath, dpath

  - We are down to 3 ways of specifying FS locations: fpath, apath, dpath.  It is still
    unclear to me why we need both apath and dpath.

* Has any essential functionality been lost?


------------------------------------------------------------------------------------------
11. Phase 5 Simplification: Eliminating dpaths [prefiles] and the dir datatype.

This version is an attempt to simplify the file "location paths" down to only fpaths and apaths,
eliminating dpaths [formerly prefiles].

This is a significant simplification suggested by the fact that using both apaths [prepaths]
and dpaths [prefiles] seems to be redundant.

Eliminating dpaths [prefiles] implies that we can also eliminate the dir datatype.

Most of the functions mapping from and to dpaths can be deleted. There are several
functions that involve dir and dpaths that can either be deleted or become fairly trivial,
including:

    dpathToString
    pickle
    unpickle
    osstring_dpath_relative
    osstring_relative

The DIR constructor for the dir type seems to be used to create what I will call 
"segmented paths", and these paths are represented in the fpath (string) form by 
a concatentation of fpath segments separated by the colon character.  So far, I have found
such segmented paths used in the PIDMAP file, but there may be other uses.

The decodeFpath function deals with these segmented paths internally, and produces a file,
which formerly could have a "segmented" dpath involving DIR segmentation. If "segmented paths"
are really essential, I believe they might be "simulated" by using a list of apaths, with one
apath for each segement, and where all but the first segment would be relative.

Again the question is whether functionality that is essential for SrcPath clients is
lost in this simplification (i.e. did the simplification "overshoot" it goal?).
My conjecture is that any essential functionality could be recovered without needing to
reintroduce the dir type and the dpath [prefile] type. If we really need "segmented" paths,
it might be enough to model them as lists of apaths, where all but the last apath in the
list must be relative.
  
It is clear how to translate a dpath of the form {dir = ROOT vol, arcs} into an apath:

    {dir = ROOT vol, arcs}  -->  {isAbs = true, vol = vol, arcs = arcs}

But what about the {dir = CWD, arcs} and the {dir = ANCHOR _, arcs} forms?

    {dir = CWD, arcs}  -->  {isAbs = false, vol = "", arcs = arcs}

If we can assume (in general) that relative paths are always relative the the CWD.
For the anchor variant of dpath, how about

    {dir = ANCHOR{name, ...}, arcs}  --> {isAbs = false, vol = "", revarcs = rev ("$"^name :: rev revarcs)}

or can there be absolute anchored paths? Example?  ["/$anchor/..." doesn't seem to make
sense as an anchored path.]

Conversely, should apath of the forms:

   {isAbs = true, vol, revarcs = [ ... , $name]}

   {isAbs = _, vol, revarcs = [..., $name, ..., arc]}

be considered ill-formed and illegal?


--------------------------------------------------------------------------------
Name changes and new names:

Renamed:

encode0		dpathToString (type changed)
bracket		show_anchors  -- parameter of encodeDpath (was encode0)
encode*		encodeFile
encodingIsAbsolute*  absoluteFpath
file0 [type]	fileInfo  -- now a simple record type, not a datatype (defined using "withtype")
file0 [fun]	dpathToFileInfo
file*  		dpathToFile
context 	dir   -- fileInfo[file0] field and various argument names
get_free	ApathEnv.get
set_free	ApathEnv.set
set0		setRelative
pp2name		apathToFpath
F0M		FileInfoMap [structure, used in intern, clear, sync]
dir*		fileToDir
desc*		fileToFpath
extend*		extendDpath
augPP		extendApath
dirPP		parentApath
pre*		fileToApath
idOf		getFileId
raw*		mkDpath
decode*		decodeFpath (segmented fpaths?)
pre0		fileInfoToDpath
pre*		fileToApath
prepath	[type]	apath
prefile* [type]	dpath

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
bogus_elab	.  -- "
look		.  -- ANCHOR argument field

Added:

.		dpathToFileInfo
.		dpathToFile
.		dirToReanchor
.		fileInfoToReanchor
.		dpathToReanchor
.		fileToReanchor

* Some of the former "elab" functions are replaced by "reanchor" functions.

* In some cases, an env parameter that provided access to the anchor -> apath
  environment is removed, and instead the global apath environment in ApathEnv
  is accessed directly. In other cases (like bind and lookAnchor) where the env 
  parameter provided access to the "bound" dpath environment, the env parameter
  is replaced by a dpathEnv parameter.

--------------------------------------------------------------------------------

END NOTES
*)
