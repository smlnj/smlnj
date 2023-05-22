(* dbm-srcpath.sml
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Operations over abstract names for CM source files.
 *
 * Author: Matthias Blume
 * Edited by: DBM
 *)

structure SrcPath :> SRCPATH =
struct

local

  structure EM = ErrorMsg

  structure P = OS.Path     (* Basis *)
  structure F = OS.FileSys  (* Basis *)
  structure FI = FileId     (* srcpath-lib.cm: ./fileid.sml *)
  structure SM = StringMap  (* ../util/sources.cm: cm/util/stringmap.sml *)

in

    exception Format
     (* This is raised only in the exported function unpickle.
        Why not use the existing UnpickleUtil.Format exception, as is done in other places
	like stable/stabilize.sml dnd smlfile/skel-io.sml? 
        [This would require importing "$pickl-lib.cm" in srcpath-lib.cm, as is done in
        cm-lib.cm.]
	There is one specific "handle SrcPath.Format, occuring in the file stable/stabilize.sml
        where the handler just raises UnpickleUtil.Format in place of this Format exn. *)

    fun impossible msg = EM.impossible ("SrcPath: " ^ msg)
    fun error (msgs: string list) = Say.say (concat msgs ^ "\n")

    type anchor = string  (* the name of an anchor, e.g. "smlnj" for the anchor $smlnj *)

    type filepath = string (* a file path string using the same format as OS.Path uses *)

    type stableid = int

    (* A prepath is similar to the result of OS.Path.fromString except that
     * we keep the list of arcs in reversed order.  This makes adding
     * and removing arcs at the end easier. *)
    type prepath = { revarcs: string list, vol: string, isAbs: bool }

    datatype reanchor  (* relative to an anchor *)
      = Anchor of anchor
      | Parent of reanchor
      | Extend of string list (*arcs*) * reanchor
      | NoAnchor  (* corresponding to the NONE cases *)

    (* DBM: What is an elab?  -- looks like a sort of "object"
     *   -- an "elaborated" file, internal or "semantic" representation of a file
     * What is its purpose?
     * What is the type name "elab" supposed to suggest? What would be a better
     *   name for the elab type?
     * What does the valid function do?
     *   -- confirms that the prepath still specifies a correct path for this file?
     * When is an elab valid? Is its validity dependent on the state of the file system?
     *   -- if a file is renamed its "elab" may have an out-of-date prepath?
     *      or the elab itself may be "invalid" if its file has been deleted?
     * What could change the validity of an elab?
     *   -- renaming (moving) the file, or deleting the file? Or otherwise "invalidating"
     *      the file's prepath (its internal idea of its location).
     * Would "valid : bool ref" serve instead of making the valid field a function?
     * !! Don't think that the valid function does anything essential! Hence dropped it.
     * What does the reanchor function do? What should be passed as its argument?
     * What is an example of a "non-trivial" reanchor function, and where is such a function
     *   defined? E.g. an example of a reanchor function that does not return NONE?
     * NOTE: elab is not exported, so it is only used internally in SrcPath. This also means
     *   that none of the functions returning elabs (e.g. elabDir, elabFileInfo, elabFile) are
         exported. And its name is not known to, or relevant to, the rest of CM.
     *)
    type elab = { pp: prepath,      (* the "location" of the file, as a prepath *)
		  reanchor: reanchor option }

    (* anchorInfo: information associated with an ANCHOR dir (directory).
     * [What does look do? -- replaced by an elab field]
     * We try using a simple elab field instead (given that it can easily
     *  be computed in the mk_anchor function. Any other places where anchorInfo records are created?
     * Encode produces a corresponding filepath, with "annotations" if the boolean argument is true. *)
    type anchorInfo =
         {name: anchor,  (* i.e. string *)
	  elab: elab,    (* replaces:  look: unit -> elab, -- Do we need to thunkify it? *)
	  encode : bool -> string option}
             (* encode maps the anchor to an (optionally "anchor-annotated") filepath *)

    (* dir: supposed to denote file system directories? *)
    datatype dir
      = CWD                   (* the current working directory, the filepath for which can
				 be obtained either from the cwd_filepath ref, or as returned
				 by a call of F.getDir (), normally through the cwd function *)
      | ANCHOR of anchorInfo  (* generally, anchors "denote" directories *)
      | ROOT of filepath      (* Unix: string = "", Windows: string = a volume name *)
      | DIR of fileInfo       (* a representation of a file-system directory? *)

    (* fileInfo: representation of a (semantic?) file?
     * How are (dir, arcs) related to the prepath component of the elab component?
     * Should they be consistent? If so, is this enforced?
     * When will the elab component be changed?  (* elabFileInfo, ... *)
     * When will the id component be changed? -- set by intern, and reset by sync
     * Here we have used "withtype" instead of introducing the not usefule PATH constructor.
     * !!! elab component deleted. An elab can be computed when necessary by fileInfoToElab.
     * Now a fileInfo is just a prefile with an added id field, which will be set to a FileId.id
     * by intern (or reset by sync) *)
    withtype fileInfo = (* PATH constructor unnecessary (using "withthpe"), so deleted *)
      { dir: dir,               (* formerly "context" *)
	arcs: string list,      (* filepath relative to dir: INVARIANT: length arcs >= 1 *)
	id: FI.id option ref }  (* updatable, optional file id *)

    (* prefile: represents a file in terms of a directory and a list of arcs relative to that
     * directory. As usual, a corresponding file may not exist in the file system.
     * The arcs are in normal (file system) order, outermost first (unlike for prepaths).
     * The arc list can be empty (?), in which case the prefile designates the "directory".
     * [Why does prefile need an err function field?  Ans: We have deleted this field.
     *   Where is a non-trivial err function defined for a prefile?
     *   When is the err function called?
     *   Let's try dropping the err component and see what breaks.] *)
    type prefile = { dir: dir, arcs: string list }

    (* file: a file is a fileInfo record with a stableid (int) attached.
     * The stableid numbers are generated by the intern function, which also fills in the
     * id field of the fileInfo part. The interned files are "recorded" in a finite map of
     * type FileIdSidMap.map *)		       
    type file = fileInfo * stableid

(*
    (* ord_key and compare can be used for defining sets and mappings over files,
     * based on their stableids.
     * But where do we use these?  They don't seem to used locally or elsewhere in CM.
     * We don't seem to have any maps or sets over file the file type. *)

    type ord_key = file

    (* stable comparison -- compare the stableids of the files *)
    fun compare (f1: file, f2: file) = Int.compare (#2 f1, #2 f2)
*)

    (* env: a kind of "object-like", stateful environment? *)
    type env =
	 { get: anchor -> elab,
	   set: anchor * prepath option -> unit,
	   defined: anchor -> bool,
	   reset: unit -> unit,
	   bound: prefile SM.map }

    val null_pp : prepath = 

    val bogus_elab : elab =
	{ pp = { revarcs = [], vol = "", isAbs = false },
	  reanchor = fn _ => NONE }

    (* filepathToPrepath : filepath -> prepath *)
    (* use OS.Path.fromString to parse the filepath, then convert result to a prepath
     * by reversing the arcs *)
    fun filepathToPrepath (filepath: string) =
	let val { arcs, vol, isAbs } = P.fromString filepath
         in { revarcs = rev arcs, vol = vol, isAbs = isAbs }
	end


  (* current working directory (CDW) *)

    (* cwd_filepath : filepath ref *)
    (* a ref initialized with the raw filepath of the current working directory (CWD);
     * If the CWD changes, this record will be updated if and when the cwd function is called
     * subsequently.
     * Why store this value anyway?  Why not just call F.getDir whenever one needs to know
     * the current working directory (as a filepath or prepath). The reason, apparently, is that
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

    (* cwd : unit -> string *)
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
	    if newCwdFilepath = !cwd_filepath
	    then newCwdFilepath (* no change; cwd_filepath is validated, return newCwdFilepath *)
	    else (* if not, CWD must have changed since the previous call of cwd *)
	      (cwd_filepath := newCwdFilepath;
	       if !cwd_notify then app (fn c => c ()) (!clients) else ();
	       cwd_notify := false; (* why do this? *)
	       newCwdFilepath)
	end


    (* unintern : file -> fileInfo *)
    (* returns the fileInfo component (#1) of a file *)
    fun unintern (f: file) : fileInfo = #1 f

    (* fileInfoToPrefile [pre0] : fileInfo -> prefile *)
    fun fileInfoToPrefile ({ dir, arcs, ... }: fileInfo) : prefile =
	{ dir = dir, arcs = arcs }

    (* fileToPrefile [pre] : file -> prefile *)
    fun fileToPrefile (f: file) = fileInfoToPrefile (unintern f)


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
							       
    (* stringToCodes : string -> string [was ta] *)
    (* translate only special characters in the string to their \ddd codes.
     * Thus the result string does not contain any of the special characters. *)
    val transSpecial = String.translate specialToCode

    (* transAll : string -> string [was ta'] *)
    (* replace all characters in the string with their \ddd codes *)
    val transAll = String.translate charToCode

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
    fun transArc (a: string) =
	if a = P.currentArc then "."        (* i.e. a *)
	else if a = P.parentArc then ".."   (* i.e. a *)
	else if a = "." then transAll "."   (* "\\046" -- very unlikely case *)
	else if a = ".." then transAll ".." (* "\\046\\046" -- very unlikely case *)
	else transSpecial a  (* the general, or "ordinary" case *)


    (* encodePrefile : [show_anchors:]bool -> prefile -> string *)
    (* encodePrefile appears to translate a prefile into a string that uses filepath notation
     * conditionally enhanced with some special anchor notations, controlled by the show_anchors argument.
     * If show_anchors is true, sometimes an "expansion" of an anchor is included? e.g. "foo(=bar)" can
     * be produced when the encode component of an ANCHOR dir returns SOME "bar".
     * Also, if show_anchors is true, the "$/" abbreviation is used when the first arc matches the
     * anchor name.
     * Arc strings are "canonicalized" by applying transArc, which replaces "special" characters
     * in the arc string with their escape codes (of the form "\ddd").
     * The internal function names, e_ac and e_c, are not informative and could be improved.
     * The parameter name "show_anchors" could be changed to something more descriptive. *)
    fun encodePrefile (show_anchors: bool) ({dir, arcs}: prefile): string =
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
	      | dir_path (ANCHOR {name, encode, ...}, path, firstArcOp) =
		  let val name' = transSpecial name (* get rid of special chars in anchor name *)
		      val path : string list =
			    (* path is a list of path component strings consisting of arcs, 
			       "$", "/", ":" and other anchor-related notations? *)
			    (case encode show_anchors (* try "expanding" the anchor to a filepath *)
			       of SOME fp => (* anchor "expands to" fp? *)
				    if show_anchors
				    then "$" :: name' :: "(=" :: fp :: ")/" :: path
				    else fp :: "/" :: path
				| NONE => 
				    (case firstArcOp
				       of SOME firstArc =>
					    if show_anchors andalso firstArc = name'
					    then "$/" :: path
					    else "$" :: firstArc :: "/" :: path
					| NONE => "$" :: name' :: "/" :: path))
		   in concat path
		  end
	      | dir_path (DIR ({ dir, arcs, ... }: fileInfo), path, _) =
		  arcs_path (dir, arcs, true, ":" :: path)  (* introduce the "segment divider", ":" *)

	 in arcs_path (dir, arcs, false, [])
	end (* fun encodePrefile *)

    (* encodeFile : file -> string *)
    val encodeFile = (encodePrefile false) o fileToPrefile


    (* parentPrepath [dirPP] : prepath -> prepath *)
    (* returns the prepath of the parent directory *)
    fun parentPrepath ({ revarcs = _ :: revarcs, vol, isAbs }: prepath) : prepath =
	  { revarcs = revarcs, vol = vol, isAbs = isAbs }
      | parentPrepath _ = impossible "parentPrepath"

    (* parentElab [dirElab] : elab -> elab *)
    fun parentElab ({ pp, reanchor }: elab) : elab  =
	{ pp = parentPrepath pp,
	  reanchor = Option.map Parent reanchor }

    (* extendPrepath : string list -> prepath -> prepath *)
    fun extendPrepath (arcs: string list) ({ revarcs, vol, isAbs }: prepath) : prepath  =
	{ revarcs = List.revAppend (arcs, revarcs), vol = vol, isAbs = isAbs }

    (* extendElab : string list -> elab -> elab *)
    fun extendElab arcs { pp, reanchor } =
	{ pp = extendPrepath arcs pp,
	  reanchor = Option.map (fn ra => Extend (arcs, ra)) reanchor) }

    (* rootPrepath : prepath *)
    fun rootPrepath (vol : string) : prepath =
	{ revarcs = nil, vol = vol, isAbs = true }

    (* dirToElab : dir -> elab *)
    fun dirToElab CWD =
	  let val cwd_filepath = cwd ()     (* use cwd to get the CWD filepath *)
	   in { pp = filepathToPrepath cwd_filepath,  (* derive a prepath from it *)
		reanchor = NONE }
	  end
      | dirToElab (ANCHOR { name, elab, encode }) = elab
      | dirToElab (ROOT vol) = { pp = rootPrepath vol, reanchor = NONE }
      | dirToElab (DIR fi) = parentElab (fileInfoToElab fi)

    (* fileInfoToElab : fileInfo -> elab *)
    and fileInfoToElab ({ dir, arcs, id }: fileInfo) =
	(* generate an elab from dir, arcs *)
	let val elab = extendElab arcs (dirToElab dir)  
	 in id := NONE; (* DBM: why set id to NONE? *)
	    elab  
	end

    (* prefileToElab : prefile -> elab *)
    fun prefileToElab ({dir, arcs} : prefile) = 
	extendElab arcs (dirToElab dir)

    (* fileToElab : file -> elab *)
    fun fileToElab (file: file) = fileInfoToElab (unintern file)

    (* mk_anchor : env * anchor -> anchorInfo *)
    fun mk_anchor ({bound, get, ...} : env, anchor: anchor) =
	case SM.find (bound, anchor)
	  of SOME prefile =>  (* anchor is in the domain of the bound map *)
	      { name = anchor,
		elab = prefileToElab prefile, (* create the elab for the anchor from the prefile *)
		encode = (fn show_anchors => SOME (encodePrefile show_anchors prefile))}
	   | NONE => (* anchor is not in the domain of bound; try get to define the elab *)
	      { name = anchor,
		elab = get anchor,  (* get will fail if not (defined anchor) *)
		encode = fn _ => NONE }

    (* prepathToFilepath : prepath -> string *)
    fun prepathToFilepath ({ revarcs, vol, isAbs }: prepath) =
	P.toString { arcs = rev revarcs, vol = vol, isAbs = isAbs }


   (* interning files: getting file_ids and and generating stableids *********************** *)

    (* getFileId [idOf] : fileInfo -> FI.id *)
    (* returns the fileId associated with the file, defining it if necessary *)
    fun getFileId (fi as { id, ... }: fileInfo) =
	let val { pp, ... } = fileInfoToElab fi (* compute the elab and extract pp from it *)
	 in case !id
	      of SOME i => i
	       | NONE =>
		   let val i = FI.fileId (prepathToFilepath pp)
		    in id := SOME i; i
		   end
	end

    (* compareFileInfo : fileInfo * fileInfo -> order *)
    (* compare fileInfo values by comparing their id fields as accessed/computed by getFileId *)
    fun compareFileInfo (f1, f2) = FI.compare (getFileId f1, getFileId f2)

    structure FileInfoMap =
      RedBlackMapFn (type ord_key = fileInfo
                     val compare = compareFileInfo)

    local
	(* known: a finite mapping from fileInfo
	val known : int FileInfoMap.map ref = ref FileInfoMap.empty
	val next = ref 0
    in
        (* clear : unit -> unit *)
        fun clear () = known := FileInfoMap.empty

	(* intern : fileInfo -> file *)
	(* generate a stableid (sequentially) to add to a fileInfo to make a file. If the fileInfo
	 * has already been interned before, just return the associated stableid. If it is new
	 * generate the next stableid and also (through the getFileId function called by insert) 
	 * define the id field of the fileInfo. *)
	fun intern (fi: fileInfo) : file =
	    case FileInfoMap.find (!known, fi)
	      of SOME stableid => (fi, stableid)
	       | NONE =>
		   let val stableid = !next
		    in next := stableid + 1;
		       known := FileInfoMap.insert (!known, fi, stableid);
		         (* record the (fi, stableid) in known *)
		       (fi, stableid)  (* return the interned file *)
		   end
        
	(* sync : unit -> unit *)
	(* sync causes the file ids of all the interned files to be reset to the actual
	   file system file ids (obtained from OS.FileSys.fileId) *)
	fun sync () =
	    let val km = !known
		fun invalidateFileId ({ id, ... }: fileInfo, _) = id := NONE
		fun reinsert (k, i, m) = FileInfoMap.insert (m, k, i)
	     in FileInfoMap.appi invalidateFileId km;
		  (* This will cause the id fields to be reassigned using the file systems file ids
		     when they are accessed via getFileId for the insert operation. *)
		known := FileInfoMap.foldli reinsert FileInfoMap.empty km  (* DBM: why? *)
	    end
    end

    (* fileToDir : file -> dir *)
    fun fileToDir (f: file) = DIR (unintern f)

    (* fileToFilepath [desc] : file -> string *)
    val fileToFilepath = encodePrefile true o fileToPrefile

    (* newEnv : unit -> env *)
    (* creates a new anchor environment "object"
     * The environment mapping (SM.map) is stored in the anchorMapRef reference.
     * This mapping maps anchors to (prepath * bool ref), where the bool ref is the "validity" flag.
     * So an anchor can be mapped in !anchorMapRef, but still be "invalid" if the validity flag
     * is false. Conversely, an anchor binding may be removed from !anchorMapRef, yet its validity
     * flag may have been incorporated into existing elabs.
     * get [get_free] and set [set_free] and defined [is_set] are not called outside this file.
     *   while (some version of?) reset is called in many files in cm.
     * get [get_free] is only called in get_anchor and mk_anchor.
     * set [set_free] is only called in set0 [set0]
     * DEFN: An anchor is "valid" if it is in the domain of the !anchorMapRef map. *)
    fun newEnv () : env =
	let val anchorMapRef : prepath SM.map ref = ref SM.empty

	    (* find : anchor -> prepath option *)
	    fun find anchor = SM.find (!anchorMapRef, anchor *)
		
	    (* defined : anchor -> bool *)
	    (* Is anchor bound in !anchorMapRef? *)
	    fun defined anchor = SM.inDomain (!anchorMapRef, anchor)

	    (* get : anchor -> elab *) 
	    (* look up anchor in !anchorMapRef. If found, return a new elab for the anchor
	     * containing the same prepath and validity as the existing binding. If not
	     * found (anchor is not in the domain of !anchorMapRef), produces an undefined
	     * anchor fatal error (impossible, compiler bug. So get should only be called with
	     * an anchor that is known to be defined. *)
	    fun get anchor =
		case find anchor
		  of SOME pp =>
		       { pp = pp,
			 reanchor = (fn cvt => SOME (filepathToPrepath (cvt anchor))) }
		   | NONE => impossible ["get -- undefined anchor: $", anchor]

	    (* set : anchor * prepath option -> unit *) 
            (* If prepathOp is SOME prepath, binds or rebinds anchor to prepath. 
             * If prepathOp is NONE, unbinds anchor in !anchorMapRef. *)
	    fun set (anchor, prepathOp) =
		case find anchor
		  of SOME pp =>
		       anchorMapRef :=
		         (case prepathOp
		            of SOME pp => SM.insert (!anchorMapRef, anchor, pp)
			     | NONE => #1 (SM.remove (!anchorMapRef, anchor))))
		               (* this won't raise NotFound because SM.find returned SOME *)
		   | NONE =>
		       (case prepathOp
			  of SOME pp => anchorMapRef := SM.insert (!anchorMapRef, anchor, pp))
			   | NONE => ())
		    
	    (* reset : unit -> unit *)
	    (* set validity flags of all entries in !anchorMapRef to false and assign SM.empty
	     * to the anchorMapRef ref. The validity flags may have been incorporated into
	     * previously created elabs, and so remain relevant. *)
	    fun reset () = anchorMapRef := SM.empty  (* reset anchorMapRef to the empty map *)

	    val bound : prefile SM.map = SM.empthy

	 in { get = get, set = set, defined = defined, reset = reset, bound = bound }
	end

    (* get_anchor : env * anchor -> filepath option *)
    fun get_anchor ({defined, get, ...}: env, anchor) =
	Option.map prepathToFilepath (get anchor)

    (* setRelative [set0]: env * anchor * filepath option * filepath-> unit *)
    fun setRelative ({set, ...}: env, anchor: anchor, filepathOp: filepath option,
		     relativeTo: filepath) =
	let fun fp_pp (filepath: filepath) =
		let val fp1 = P.mkAbsolute {path = filepath, relativeTo = relativeTo}
		    val fp2 = if P.isAbsolute filepath then filepath else fp1
		 in filepathToPrepath fp2
		end
	 in set (anchor, Option.map fp_pp filepathOp)
	end

    (* set_anchor : env * anchor * filepath option * filepath -> unit *)
    fun set_anchor (env, anchor, filepathOp) =
	setRelative (env, anchor, filepathOp, F.getDir ()) before sync ()

    (* reset_anchors : env -> unit *)
    fun reset_anchors ({reset, ...}: env) = (reset (); sync ())

    (* processSpecFile : env * filepath -> TextIO.instream -> unit *)
    fun processSpecFile (e, filepath) =
	let val local_dir = P.dir (F.fullPath filepath)
	    fun set (env, anchor, filepathOp) =
		setRelative (env, anchor, filepathOp, local_dir)

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
					  (set (e, a, SOME (mknative isnative d));
					   loop isnative)
				      | ["-"] => (#reset e (); loop isnative)
				      | [a] => (set (e, a, NONE); loop isnative)
				      | [] => loop isnative
				      | _ => (error [f, ": malformed line (ignored)"];
					      loop isnative)
		 in loop true
		end

	 in work
	end (* fun processSpecFile *)

    datatype stdspec
      = RELATIVE of string list
      | ABSOLUTE of string list
      | ANCHORED of anchor * string list

    (* parseFilepathNative [parseNativeSpec]: filepath -> stdspec *)
    fun parseFilepathNative filepath =
	let val {isAbs, arcs, vol} = OS.Path.fromString filepath
         in case arcs
	      of [""] => impossible "parseFilepathNative -- zero-length name"
	       | [] => impossible "parseFilepathNative -- no fields"
	       | (["$"] | "$"::""::_) =>
		   (error ["invalid zero-length anchor name in: ", filepath]);
		    RELATIVE arcs)
	       | "$" :: (arcs' as (arc1 :: _)) => (* "$/arc1/..." *)
		   if isAbs
		     then RELATIVE arcs
		     else ANCHORED(arc1, arcs')
	       | arc1 :: arcn =>
		   if String.sub(arc1, 0) = #"$"
		     then ANCHORED(String.extract(arc1, 1, NONE), arcn)
		   else if isAbs
		     then ABSOLUTE arcs
		     else RELATIVE arcs
	     (* end case *)
        end

    (* parseFilepathStandard [parseStdspec]: (string -> unit ) -> filepath -> stdspec *)
    fun parseFilepathStandard filepath =
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

    (* bind : env -> (anchor * prefile) list -> env *)
    (* produces a new env record with only the "bound" field altered.
     * Anchors are bound to corresponding prefiles, with these bindings being
     * added to the existing "bound" mapping.
     * (Perhaps?) called only in main/general-params.sml *)
    fun bind ({get, defined, set, reset, defined, bound}: env) alist =
	let fun folder (anchor, prefile), m) = SM.insert (m, anchor, pf)
	 in { get = get, set = set, reset = reset, defined = defined,
	      bound = foldl folder bound alist }
	end

    (* prefileToFileInfo : prefile -> fileInfo *)
    fun prefileToFileInfo ({ dir, arcs }: prefile) : fileInfo =
	{ dir = dir,
	  arcs = (case arcs
		    of nil => (error ["path needs at least one arc relative to `",
				      prepathToFilepath (#pp (dirToElab context)), "'"];
			       ["<bogus>"])   (* DBM: Is a fileInfo with no arcs bogus? *)
		     | _ => arcs),
	  elab = ref bogus_elab, (* why bogus? why not create an "elab" here? *)
	  id = ref NONE }  (* why not generate a new fileid here? *)
	     
    (* prefileToFile : prefile -> file *)
    val prefileToFile = intern o prefileToFileInfo

    (* raw : dir * filepath -> prefile *)
    fun raw (dir, filepath) =
        let val {arcs, vol, isAbs} = P.fromString filepath
	    val dir = if isAbs then ROOT vol else dir
	 in {dir = dir, arcs = arcs}
	end

    (* native : env -> dir * filepath -> prefile *)
    fun native env (dir, filepath) =
          (case parseFilepathNative filepath
             of RELATIVE arcs => {dir = dir, arcs = arcs}
              | ABSOLUTE arcs => {dir = ROOT "", arcs = arcs}
              | ANCHORED (anchor, arcs) => {dir = ANCHOR (mk_anchor (env, anchor)), arcs = arcs}
          (* end case *))

    (* standard : env -> dir * filepath -> prefile *)
    fun standard env (dir, filepath) =
	(case parseFilepathStandard filepath
	   of RELATIVE arcs => {dir = dir, arcs = arcs}
	    | ABSOLUTE arcs => {dir = ROOT "", arcs = arcs}
	    | ANCHORED (anchor, arcs) => {dir = ANCHOR (mk_anchor (env, anchor)), arcs = arcs}
        (* end case *))

    (* extendPrefile: prefile -> string list -> prefile *)
    fun extendPrefile ({ dir, arcs }: prefile) (morearcs: string list) =
	{ dir = dir, arcs = arcs @ morearcs }

    (* osstring : file -> string *)
    val osstring = FI.canonical o prepathToFilepath o #pp o fileInfoToElab o unintern

    (* osstring_prefile : prefile -> string *)
    fun osstring_prefile { dir, arcs } =
	FI.canonical (prepathToFilepath (#pp (extendElab arcs (dirToElab dir))))

    (* osstring_dir : dir -> string *)
    fun osstring_dir d =
	case prepathToFilepath (#pp (dirToElab d))
	  of "" => P.currentArc
	   | s => FI.canonical s

    (* osstring' : file -> string *)
    fun osstring' f =
	let val oss = osstring f
	 in if P.isAbsolute oss
	    then let val ross =
			 P.mkRelative { path = oss, relativeTo = #name (!cwd_info) }
		  in if size ross < size oss then ross else oss
		 end
	    else oss
	end

    (* do_reanchor : (string -> filepath) -> reanchor -> prepath *)
    fun do_reanchor (cvt: string -> filepath) reanchor =
	case reanchor
	  of Anchor of anchor => filepathToPrepath (cvt anchor)
	   | Parent of reanchor => parentPrepath (do_reanchor cvt reanchor)
	   | Extent of (arcs, reanchor) => extendPrepath arcs (do_reanchor cvt reanchor)

    (* do_reanchorOp : (string -> filepath) -> reanchor option -> prepath option *)
    fun do_reanchorOp (cvt: string -> filepath) reanchorOp =
        case reanchorOp
	  of NONE => NONE
	   | SOME reanchor => SOME (do_reanchor cvt reanchor)

    (* osstring_reanchored : (string -> string) -> file -> filepath option *)
    (* used once in main/filename-policy.sml (FilenamePolicyFn) *)
    fun osstring_reanchored cvt file =
	let val {reanchor, ...} = fileToElab file
	 in Option.map (FI.canonical o prepathToFilepath) (do_reanchorOp cvt reanchor)
	end

    (* osstring_prefile_relative : prefile -> filepath *)
    fun osstring_prefile_relative (pf as { dir, arcs }: prefile) =
	case dir
	  of DIR _ => FI.canonical (P.toString { arcs = arcs, vol = "", isAbs = false })
	   | _ => osstring_prefile pf

    (* osstring_relative : file -> filepath *)
    val osstring_relative = osstring_prefile_relative o fileToPrefile
    
    (* tstamp : file -> TSamp.t *)
    fun tstamp f = TStamp.fmodTime (osstring f)

    (* pickle : (bool * string -> unit) -> {prefile: prefile, relativeTo : file}
                -> string list list *)
    (* nontrivial warn is used in function prepath2list in stabilize.sml *)
    fun pickle warn { prefile as {dir,arcs}: prefile, relativeTo = (fi, _) } =
	let fun warn' (abs_or_rel: bool) =
		warn (abs_or_rel, encodePrefile false { dir = dir, arcs = arcs })
		     (* HACK! We are cheating here, turning the prefile into a file
		      * even if there are no arcs.  This is ok because of the false
		      * arg for encodePrefile. *)

	    fun p_p p = p_pf (fileInfoToPrefile p)

	    and p_pf ({ dir, arcs } : prefile) = arcs :: p_d dir

	    and p_d (ROOT vol) = (warn' true; [[vol, "r"]])
	      | p_d CWD = impossible "pickle: CWD"
	      | p_d (ANCHOR { name, ... }) = [[name, "a"]]
	      | p_d (DIR dir_fi) =
		(case compareFileInfo (dir_fi, fi)
		   of EQUAL => (warn' false; [["c"]])
		    | _ => p_p f0)

	 in p_pf prefile
	end

    (* unpickle : env -> {pickled: string list list, relativeTo: file} -> prefile *)
    fun unpickle env { pickled, relativeTo } =
	let fun u_pf (arcs :: l) = {dir = u_d l, arcs = arcs}
	      | u_pf _ = raise Format

	    and u_p l = prefileToFileInfo (u_pf l)

	    and u_d [[vol, "r"]] = ROOT vol
	      | u_d [["c"]] = fileToDir relativeTo
	      | u_d [[n, "a"]] = ANCHOR (mk_anchor (env, n))
	      | u_d l = DIR (u_p l)

	 in u_pf pickled
	end

    (* decode : env -> filepath -> file *)
    fun decode env filepath =
	let fun isChar (c1: char) c2 = c1 = c2

	    (* transCode: string -> string *)
	    (* converting \ddd codes in a string back into "special" characters, i.e.
	     * the inverse of transSpecial *)
	    fun transCode (s: string) : string =
	        let val dc = Char.chr o valOf o Int.fromString o implode
		    fun loop ([], r) = String.implode (rev r)
		      | loop (#"\\" :: d0 :: d1 :: d2 :: l, r) =
			  (loop (l, dc [d0, d1, d2] :: r)
			  handle _ => loop (l, d2 :: d1 :: d0 :: #"\\" :: r))
		      | loop (c :: l, r) = loop (l, c :: r)
		 in loop (String.explode s, [])
		end

	    (* arc: string -> string *)
	    fun arc "." = P.currentArc  (* = "." *)
	      | arc ".." = P.parentArc  (* = ".." *)
	      | arc a = transCode a

	    (* addseg : string * fileInfo -> fileInfo *)
	    fun addseg (seg, fi) =
		prefileToFileInfo (DIR fi, map arc (String.fields (isChar #"/") seg))

	    (* firstSet : string -> fileInfo *)
	    fun firstSeg s =
		(case String.fields (isChar #"/") s
		   of nil => impossible "decode: no fields in segment 0"
		    | arc0 :: arcs =>
		       let val arcs = map arc arcs
			   val arc0' = transCode (String.extract (arc0, 1, NONE))
			in if arc0 = ""
			   then prefileToFileInfo (ROOT "", arcs)
			   else (case String.sub (arc0, 0)
				   of #"%" => prefileToFileInfo (ROOT (xtr ()), arcs)
				    | #"$" =>
					let val n = xtr ()
					 in prefileToFileInfo (ANCHOR (mk_anchor (env, n)), arcs)
					end
				    | _ => prefileToFileInfo (cwd (), arc arc0 :: arcs))
		       end)

	 in case String.fields (isChar #":") filepath
	      of nil => impossible "decode: no segments"
	       | seg0 :: segs => intern (foldl addseg (firstSeg seg0) segs)
	end (* fun decodeFile *)

    (* encodeingIsAbsolute : filepath -> bool *)
    fun absoluteFilepath s =
	(case String.sub (s, 0) of (#"/" | #"%") => true | _ => false)
	handle _ => false

end (* top local *)
end (* structure StrPath *)


(* ================================================================================ *)

(* NOTES:

1. The design seems to be designed to accomodate possible changes in the underlying file system
   (e.g. maybe renaming and deleting files, directories, changing the current working
    directory) while maintaining "validity" of the CM representations (of files).

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

   2. prepath: a structure similar to the record that OS.Path.fromString produces, but with arcs
      in reverse order (deepest arc first),

   3. prefile: a base directory (type dir), originally called the "context", plus a relative path
      (list of arcs) from that directory in outer-to-inner order,

   4. fileInfo: the information in a prefile (dir, arcs) plus
        (1) an elab ref and
        (2) an "id :FileId.id option ref", initially NONE, but reset to SOME FileId.id by intern
	    and sync, where the FileId.id is derived from the actual OS-FS file_id if it exists,

   5. file: fileInfo plus a "stableid" integer (an "interned" fileInfo),

   6. "elab": a prepath plus a validity flag and a "reanchor" function (whatever that does).
      elabs can be generated from other representations (e.g. prefile) with initial valid value true.
      They can be "invalidated" if their associated env binding is 

There is a lot of redundancy among these six ways of representing files, and several of them
are translatable into each other. The fileInfo, file, and elab types could be considered a bit more
"semantic" (i.e. deeper) because they involve a bit more than a purely syntactic description.
The purpose of the valid and reanchor components of an elab is not clear.

There is also a the DIR variety of the dir datatype that has an associated fileInfo, so dir values
can indirectly involve a file.

Could this multiplicity of file representations be simplified?  How many different represenations do
we really need? Note that prefile, file, and dir are the only ones exported by the SRCPATH signature,
and the only representations present in the exported val types are string, prefile, file, and dir.
Thus fileInfo, elab, and prepath are only used internally in SrcPath.

There is a couple progressions:

     filepath (string) --[P.fromString]--> prepath --[prepathToFilepath]--> filepath

     prefile --[intern?]--> fileInfo (adding undefined file-id)
             --[intern?]--> file (adding stableid and defining file-id)

Where do prefiles come from?  Where is the link between prepaths and prefiles?


6. There are two mappings over anchors, both associated with the env type:

   1. The implicit string map (anchorMapRef) of an env is operated on by the get, set,
      defined, and reset components of the env record. This maps anchors (strings) to
      pairs of prefiles and "validity" flags (bool ref). The validity flag is by default
      true, but can be set false either by the set function, when rebinding an anchor that
      is already mapped, or wholesale by the reset function. These validity flags are
      (assumed to be) shared with elab records (associated with the anchor).

      The set function (formerly set_free) can add new bindings of anchors to the map, or
      "rebind" an anchor that was already in the domain of the map, while "invalidating"
      the old binding by setting its validity flag to false.

      The get function, if applied to a mapped anchor (i.e. an anchor in the domain of !anchorMapRef)
      will return an elab generated from the (prepath, validity) that the anchor maps to. 
      When get is applied to an anchor that is not mapped, it causes a fatal error.

      So the "validity" flag associated with an anchor (in the !anchorMapRef map) will be set
      false when a binding of the anchor is removed (or overwritten) in the map by set or reset.

   2. The bound field of an env record is a mapping from anchors to a prefile (formerly an
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

      val reanchor: (anchor -> string) -> prepath option

   Takes a conversion function mapping an anchor to a string (normally a filepath?) and returns a
   prepath option.  In most cases this is initialized to a trivial (fn _ => NONE): for instance:

     bogus_elab, 
     absoluteElab

   Where is the reanchor component of an elab actually applied?

   osstring_reanchored (only application)

    fun osstring_reanchored cvt file =
	let val {reanchor, ...} = fileToElab file
	 in Option.map (FI.canonical o prepathToFilepath) (reanchor cvt)
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

         (fn (cvt: string -> filepath) => SOME (filepathToPrepath (cvt anchor))

     Anchor "tranforms":

       osstring_prefile_reanchored:

	 fun osstring_reanchored cvt file =
	     let val {reanchor, ...} = fileToElab file
	      in Option.map (FI.canonical o prepathToFilepath) (reanchor cvt)
	     end

       parentElab:

         fun parentElab ({ pp, reanchor }: elab) : elab  =
	     { pp = parentPrepath pp,
	       reanchor = Option.map parentPrepath o reanchor }

      extendElab:

	 fun extendElab arcs { pp, reanchor } =
	     { pp = extendPrepath arcs pp,
	       reanchor = Option.map (extendPrepath arcs) o reanchor }


   A datatype to encode the reanchor construction

      datatype reanchor  (* relative to an anchor *)
        = Anchor of anchor
        | Parent of reanchor
        | Extend of string list (*arcs*) * reanchor
	| NoAnchor  (* corresponding to the NONE cases *)

      (* do_reanchor : (string -> filepath) -> reanchor -> prepath
      fun do_reanchor (cvt: string -> filepath) reanchor =
        case reanchor
	  of Anchor of anchor => filepathToPrepath (cvt anchor)
           | Parent of reanchor => parentPrepath (do_reanchor cvt reanchor)
           | Extent of (arcs, reanchor) => extendPrepath arcs (do_reanchor cvt reanchor)

      Actually, parentPrepath and extendPrepath operate only on the revarcs component of a
      prepath, leaving the vol and isAbs fields alone, so possibly the Parent case could
      just do a tl and the Excend case could just do a revappend on the revarcs ("on the way
      out" after having recursed down to the Anchor node).

--------------------------------------------------------------------------------
Name changes and new names:

encode0		encodePrefile
bracket		show_anchors  -- parameter of encodePrefile (was encode0)
encode		encodeFile
encodingIsAbsolute  absoluteFilepath
file0 [type]	fileInfo  -- now a simple record type, not a datatype (defined using "withtype")
file0 [fun]	prefileToFileInfo
context 	dir   -- fileInfo[file0] field and various argument names
look		elab  -- anchorInfo field (argument type for ANCHOR dir constructor); changed type
get_free	get (env field label)
set_free	set (env field label)
set0		setRelative
elab_file	fileInfoToElab
elab_dir	dirToElab
pp2name		prepathToFilepath
F0M		FileInfoMap [structure]
dir		fileToDir
desc		fileToFilepath
extend		extendPrefile
augPP		extendPrepath
augElab		extendElab
dirPP		parentPrepath
dirElab		parentElab
extend		extendPrefile
pre		fileToPrefile

null_pp		.  -- removed, only used once and replaced with its defn
revalidateCwd	.  -- removed, functionality merged into fun cwd
absElab		.

.               anchorInfo     -- new type; argument type of ANCHOR dir constructor
.		prefileToElab  -- new function (used internally)
