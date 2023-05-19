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
  structure I = FileId      (* srcpath-lib.cm: ./fileid.sml *)
  structure SM = StringMap  (* ../util/sources.cm: cm/util/stringmap.sml *)

in

    exception Format

    fun impossible msg = EM.impossible ("SrcPath: " ^ msg)
    fun error (msgs: string list) = Say.say (concat msgs ^ "\n")

    type anchor = string  (* the "name" of an anchor, e.g. "smlnj" for the anchor $smlnj? *)

    type filepath = string (* the file path string of a file in the host OS's native format *)

    type stableid = int

    (* A pre-path is similar to the result of OS.Path.fromString except that
     * we keep the list of arcs in reversed order.  This makes adding
     * and removing arcs at the end easier. *)
    type prepath = { revarcs: string list, vol: string, isAbs: bool }

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
     * What does the reanchor function do? What should be passed as its argument?
     * What is an example of a "non-trivial" reanchor function, and where is such a function
     *   defined? E.g. an example of a reanchor function that does not return NONE?
     *)
    type elab = { pp: prepath,
		  valid: bool ref,  (* maybe we don't need this -- just use NONE : elab option *)
		  reanchor: (anchor -> string) -> prepath option }  (* ??? *)

    (* anchorInfo: information associated with an ANCHOR dir (directory).
     * What does look do?  We try using a simple elab field instead (looking at how the 
     * mk_anchor function behaves).
     * What does encode do?  What is its bool argument for? A non-trivial example?
     *)
    type anchorInfo =
         {name: anchor,  (* i.e. string *)
	  elab: elab,  (* replacement for:  look: unit -> elab, *)
	  encode : bool -> string option}  (* anchor expansion? replacing an anchor with a filepath? *)

    (* dir: supposed to denote file system directories? *)
    datatype dir
      = CWD                   (* the current working directory, either as recorded in cwd_filepath
			       * or as returned by a call of F.getDir (), possibly through the cwd function *)
      | ANCHOR of anchorInfo  (* generally, anchors "denote" directories *)
      | ROOT of string        (* Unix: string = "", Windows: string = a volume name *)
      | DIR of file0          (* a representation of a file-system directory? *)

    (* file0: representation of a (semantic?) file?
     * How are (dir, arcs) related to the prepath component of the elab component?
     * Should they be consistent? If so, is this enforced?
     * When will the elab component be changed?  (* elab_file, ... *)
     * When will the id component be changed?
     * Could have used "withtype" instead of introducing the PATH constructor. *)
    withtype file0 = (* PATH constructor deleted *)
      { dir: dir,               (* formerly "context" *)
	arcs: string list,	(* filepath relative to dir: INVARIANT: length arc >= 1 *)
	elab: elab option ref,  (* the elab should be derived from, or at least consistent with
				   the file's dir and arcs.  See elab_dir, elab_file. *)
	id: I.id option ref }

    type file = file0 * stableid

    (* ord_key and compare can be used for defining sets and mappings over files,
     * based on their stableids *)
    type ord_key = file

    (* stable comparison -- compare the stableids of the files *)
    fun compare (f1: file, f2: file) = Int.compare (#2 f1, #2 f2)
						   
    (* prefile: a record that designates a file in terms of a directory and a list of arcs relative
     * to that directory. A corresponding file may not exist in the file system.
     * The arcs are in normal (file system) order, outermost first (unlike for prepaths).
     * [Why does prefile need an err function field?  Ans: We have deleted this field.]
     *   [Where is a non-trivial err function defined for a prefile?]
     *   [When is the err function called?]
     *   [Let's try dropping the err component and see what breaks.] *)
    type prefile = { dir: dir, arcs: string list }

    (* rebindings: a finite map from anchors (strings) to prefiles *)
    type rebindings = { anchor: anchor, value: prefile } list

    (* DBM: What is this? Some kind of anchor "thunk"? *)
    type anchorval = (unit -> elab) * (bool -> string)

    (* env: a kind of "object-like", stateful environment? *)
    type env =
	 { get: anchor -> elab,
	   set: anchor * prepath option -> unit,
	   defined: anchor -> bool,
	   reset: unit -> unit,
	   bound: anchorval SM.map }

    (* for defining sets and mappings over files, based on their stableids *)
    type ord_key = file

    (* stable comparison -- compare the stableids of the files *)
    fun compare (f1: file, f2: file) = Int.compare (#2 f1, #2 f2)

    val null_pp : prepath = { revarcs = [], vol = "", isAbs = false }

    val bogus_elab : elab =
	{ pp = null_pp, valid = fn _ => false, reanchor = fn _ => NONE }

    (* filepathToPrepath : string -> prepath *)
    (* use OS.Path to parse the filename, then convert result to a prepath *)
    fun filepathToPrepath (filepath: string) =
	let val { arcs, vol, isAbs } = P.fromString filepath
         in { revarcs = rev arcs, vol = vol, isAbs = isAbs }
	end

    (* cwd_filepath : filepath ref *)
    (* a ref initialized with the raw filepath of the current working directory;
     * If the CWD changes, this record will be updated if and when the cwd function is called
     * subsequently.
     * Why store this value anyway?  Why not just call F.getDir whenever one needs to know
     * the current working directory (as a filepath or prepath). The reason, apparently, is that
     * if the cwd function is called after a change in CWD, "client" processes(?) will be notified. *)
    val cwd_filepath = ref (F.getDir ())

    (* cwd_notify : bool ref -- a control flag that, if true, will cause "clients" to be notified
     * if CWD changes between calls of the "cwd" function below *)
    val cwd_notify = ref true

    (* clients : (unit -> unit) list ref *)
    (* the registered functions, when called, should notify associated "client" processes that a
     * change in the CWD has been detected in a call to the cwd function. *)
    val clients = ref ([] : (string -> unit) list)

    (* addClientToBeNotified : (string -> unit) -> unit *)
    fun addClientToBeNotified c = clients := c :: !clients

    (* scheduleNotification : unit -> unit *)
    fun scheduleNotification () = cwd_notify := true

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
	 in if newCwdFilepath = !cwd_filepath (* check whether CWD has changed *)
	    then newCwdFilepath (* no change; cwd_filepath is validated, return newCwdFilepath *)
	    else (* if not, CWD must have changed since the previous call of cwd *)
	      (cwd_info := newCwdFilepath;
	       if !cwd_notify then app (fn c => c ()) (!clients) else ();
	       cwd_notify := false; (* why do this? *)
	       newCwdFilepath)
	end

    (* absoluteElab [absElab] : string list * string -> elab *)			 
    fun absoluteElab (arcs, vol) =
	{ pp = { revarcs = rev arcs, vol = vol, isAbs = true },
	  valid = fn () => true, reanchor = fn _ => NONE }

    (* unintern : file -> file0 *)
    fun unintern (f: file) : file0 = #1 f

    (* file0ToPrefile [pre0] : file0 -> prefile *)
    fun file0ToPrefile ({ dir, arcs, ... }: file0) = { dir = dir, arcs = arcs }

    (* fileToPrefile [pre] : file -> prefile *)
    fun fileToPrefile (f: file) = file0ToPrefile (unintern f)


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


    (* encodePrefile : [bracket:]bool -> prefile -> string *)
    (* encodePrefile appears to translate a prefile into a string that uses filepath notation
     * conditionally enhanced with some special anchor notations, controlled by the bracket argument.
     * If bracket is true, sometimes an "expansion" of an anchor is included? e.g. "foo(=bar)" can
     * be produced when the encode component of an ANCHOR dir returns SOME "bar".
     * Also, if bracket is true, the "$/" abbreviation is used when the first arc matches the
     * anchor name.
     * Arc strings are "canonicalized" by applying transArc, which replaces "special" characters
     * in the arc string with their escape codes (of the form "\ddd").
     * The internal function names, e_ac and e_c, are not informative and could be improved. *)
    fun encodePrefile (bracket: bool) ({dir = prefile_dir, arcs = prefile_arcs}: prefile) =
	let 
	    (* e_ac : dir * string list * bool * string list -> string *)
	    (* the dir argument is just passed through without change to calls of e_c *)
	    fun e_ac (dir: dir, []: string list, _: bool, path: string list) =
		  e_c (dir, path, NONE)
	      | e_ac (dir, arcs, ctxt, path) =
		  let val arcs' as (arc0 :: _) =  (* arc0 is bound to the outermost arc? *)
			  map transArc arcs       (* get rid of any special characters in arc strings *)
		      val arcs'' as (first :: rest) = rev arcs' (* don't need to map transArc again! *)
		      val first' :: rest' =
			  (* arcs'', rest are reversed arc lists, first is "innermost" arc *)
			    if ctxt andalso bracket
			    then concat ["(", first, ")"] :: rest  (* first' = (first) *)
			    else arcs''
		      val path : string list =  (* reversed again by foldl to restore order *)
			    foldl (fn (arc, path) => arc :: "/" :: path) (first' :: path) rest'
		   in e_c (dir, path, SOME arc0)
		  end

	    (* e_c : dir * string list * string option -> string *)
	    and e_c (ROOT "", path, _) = concat ("/" :: path)
                   (* make path absolute by adding "/" at front *)
	      | e_c (ROOT vol, path, _) = concat ("%" :: transSpecial vol :: "/" :: path)
		   (* make path "volume-rooted" by adding "%<volume>" at front *)
	      | e_c (CWD, path, _) = concat path  (* make filepath relative to c.w.d. *)
	      | e_c (ANCHOR {name, encode, ...}, path, firstArcOp) =
		  let val name' = transSpecial name (* get rid of special chars in anchor name *)
		      val path : string list =
			    (* path is a list of path component strings consisting of arcs, 
			       "$", "/", ":" and other anchor-related notations? *)
			    (case encode bracket
			       of SOME str1 => (* anchor "expands to" str1? *)
				    if bracket
				    then "$" :: name' :: "(=" :: str1 :: ")/" :: path
				    else str1 :: "/" :: path
				| NONE => 
				    (case firstArcOp
				       of SOME firstArc =>
					    if bracket andalso firstArc = name'
					    then "$/" :: path
					    else "$" :: firstArc :: "/" :: path
					| NONE => "$" :: name' :: "/" :: path))
		   in concat path
		  end
	      | e_c (DIR ({ dir, arcs, ... }: file0), path, _) =
		  e_ac (dir, arcs, true, ":" :: path)  (* introduce the "segment divider", ":" *)

	 in e_ac (prefile_dir, prefile_arcs, false, [])
	end (* fun encodePrefile *)


    (* mk_anchor : env * anchor -> anchorInfo *)
    fun mk_anchor ({bound, get, ...} : env, anchor: anchor) =
	case SM.find (bound, anchor)
	  of SOME prefile =>
	      { name = anchor,
		elab = elabPrefile prefile,
		encode = (fn bracket => SOME (encodePrefile b prefile))}
	   | NONE =>
	      { name = anchor,
		elab = get anchor,  (* get anchor could fail! *)
		encode = fn _ => NONE }

    (* encodePrefilePlain : prefile -> string *)
    val encodePrefilePlain = encodePrefile false

    (* encode : file -> string *)
    val encode = encodePrefilePlain o fileToPrefile

    (* parentPrepath [dirPP] : prepath -> prepath *)
    fun parentPrepath { revarcs = _ :: revarcs, vol, isAbs } =
	  { revarcs = revarcs, vol = vol, isAbs = isAbs }
      | parentPrepath _ = impossible "parentPrepath"

    (* parentElab [dirElab] : elab -> elab *)
    fun parentElab { pp, valid, reanchor } =
	{ pp = parentPrepath pp, valid = valid,
	  reanchor = Option.map parentPrepath o reanchor }

    (* extendPrepath : string list -> prepath -> prepath *)
    fun extendPrepath arcs { revarcs, vol, isAbs } =
	{ revarcs = List.revAppend (arcs, revarcs), vol = vol, isAbs = isAbs }

    (* extendElab : string list -> elab -> elab *)
    fun extendElab arcs { pp, valid, reanchor } =
	{ pp = extendPrepath arcs pp, valid = valid,
	  reanchor = Option.map (extendPrepath arcs) o reanchor }

    (* elab_dir : dir -> elab *)
    fun elab_dir CWD =
	  let val name = cwd ()
	      val valid = ref true  (* DBM: could be out of date! *)
	      fun reanchor (a: anchor -> string) = NONE
	   in { pp = filepathToPrepath name, valid = valid, reanchor = reanchor }
	  end
      | elab_dir (ANCHOR { name, elabOp, encode }) = look ()
      | elab_dir (ROOT vol) = absoluteElab ([], vol)
      | elab_dir (DIR p) = parentElab (elab_file p)

    (* elab_prefile : prefile -> elab *)
    fun elab_prefile ({dir, arcs} : prefile) = 
	extendElab arcs (elab_dir dir)

    (* elab_file0 : file0 -> elab *)
    and elab_file0 ({ dir, arcs, elab, id }: file0) =
	let val e as { valid, ... } = !elab
	 in if valid ()
	    then e  (* the file's stored elab is valid, leave it in place;
		     * We assume that its prepath agrees with the dir + arcs fields. *)
	    else let val e' = extendElab arcs (elab_dir dir)
		  in elab := e'; id := NONE; e'
		 end
	end


    (* prepathToFilepath : prepath -> string *)
    fun prepathToFilepath { revarcs, vol, isAbs } =
	P.toString { arcs = rev revarcs, vol = vol, isAbs = isAbs }

    (* idOf : file0 -> I.id *)
    (* returns the fileId associated with the file, defining it if necessary *)
    fun idOf (p as { id, ... }: file0) =
	let val { pp, ... } = elab_file p (* compute the elab and extract pp from it *)
	 in case !id
	      of SOME i => i
	       | NONE =>
		   let val i = I.fileId (prepathToFilepath pp)
		    in id := SOME i; i
		   end
	end

    (* compare0 : file0 * file0 -> order *)
    fun compare0 (f1, f2) = I.compare (idOf f1, idOf f2)

    structure F0M = RedBlackMapFn (type ord_key = file0
                                   val compare = compare0)

    local
	val known = ref (F0M.empty: int F0M.map)
	val next = ref 0
    in
        (* clear : unit -> unit *)
        fun clear () = known := F0M.empty

	(* intern : file0 -> file *)
	fun intern f =
	    case F0M.find (!known, f)
	      of SOME i => (f, i)
	       | NONE =>
		   let val i = !next
		    in next := i + 1;
		       known := F0M.insert (!known, f, i);
		       (f, i)
		   end
        
        (* sync : unit -> unit *)
	fun sync () =
	    let val km = !known
		fun invalidate ({ id, ... }: file0, _) = id := NONE
		fun reinsert (k, v, m) = F0M.insert (m, k, v)
	     in F0M.appi invalidate km;
		known := F0M.foldli reinsert F0M.empty km
	    end
    end

    (* dir0 : file0 -> dir *)
    val dir0 = DIR

    (* dir : file -> dir *)
    fun dir (f: file) = DIR (unintern f)

    (* descr : file -> string *)
    val descr = encodePrefile true o fileToPrefile

    (* newEnv : unit -> env *)
    (* creates a new anchor environment "object"
     * The environment mapping (SM.map) is stored in the anchorMapRef reference.
     * This mapping maps anchors to (prepath * bool ref), where the bool ref is the "validity" flag.
     * So an anchor can be mapped in !anchorMapRef, but still be "invalid" if the validity flag
     * is false. Conversely, an anchor binding may be removed from !anchorMapRef, yet its validity
     * flag may have been incorporated into existing elabs. *)
    fun newEnv () =
	let val anchorMapRef : (prepath * bool ref) SM.map ref = ref SM.empty

	    (* find : anchor -> (prepath * bool ref) option *)
	    fun find anchor = SM.find (!anchorMapRef, anchor *)
		
	    (* defined : anchor -> bool *)
	    fun defined anchor = SM.inDomain (!anchorMapRef, anchor)

	    (* get : anchor -> elab *) 
	    (* look up anchor in !anchorMapRef. If found, return a new elab for the anchor
	     * containing the same prepath and validity as the existing binding. If not
	     * found (anchor is not in the domain of !anchorMapRef), produces an undefined
	     * anchor error message and raises the BadAnchor exception. *)
	    fun get anchor =
		case find anchor
		  of SOME (pp, validity) =>
		       let fun reanchor cvt = SOME (filepathToPrepath (cvt anchor))
			in { pp = pp,
			     valid = fn () => !validity,
			     reanchor = reanchor }
		       end
		   | NONE => impossible ["get -- undefined anchor: $", anchor]

	    (* set : anchor * prepath option -> unit *) 
	    fun set (anchor, prepathOp) =
		case find anchor
		  of SOME (pp, validity) =>
		       (validity := false;   (* invalidate previously created elabs *)
		        anchorMapRef :=
		          (case prepathOp
		             of SOME pp => SM.insert (!anchorMapRef, anchor, (pp, ref true))
			      | NONE => #1 (SM.remove (!anchorMapRef, anchor))))
		                 (* this won't raise NotFound because SM.find returned SOME *)
		   | NONE => impossible ("set -- undefined anchor:" ^ anchor)))
		    
	    (* reset : unit -> unit *)
	    (* set validity flags of all entries in !anchorMapRef to false and assign SM.empty
	     * to the anchorMapRef ref. The validity flags may have been incorporated into
	     * previously created elabs, and so remain relevant. *)
	    fun reset () =
		let fun invalidate (_, validity) = validity := false
	         in SM.app invalidate (!anchorMapRef);  (* "invalidate" all entries in the old map *)
		    anchorMapRef := SM.empty            (* reset anchorMapRef to the empty map *)
		end

	 in { get = get, set = set, defined = defined,
	      reset = reset, bound = SM.empty } : env
	end

    (* get_anchor : env * anchor -> filepath option *)
    fun get_anchor ({defined, get, ...}: env, anchor) =
	if defined anchor
	then SOME (prepathToFilepath (#pp (get (anchor, fn _ => ()))))
	else NONE

    (* set0: (filepath -> filepath) -> env * anchor * filepath option -> unit *)
    fun set0 mkAbsolute ({set, ...}: env, anchor, filepathOp) =
	let fun fp_pp (filepath: filepath) =
		filepathToPrepath (if P.isAbsolute filepath then filepath else mkAbsolute filepath)
	 in set (anchor, Option.map fp_pp filepathOp)
	end

    (* set_anchor : env * anchor * filepath option -> unit *)
    fun set_anchor args =
	set0 (fn filepath => P.mkAbsolute { path = filepath, relativeTo = F.getDir () }) args
	before sync ()

    (* reset_anchors : env -> unit *)
    fun reset_anchors ({reset, ...}: env) = (reset (); sync ())

    (* processSpecFile : {env: env, specfile: filepath} -> (TextIO.instream -> unit) *)
    fun processSpecFile { env = e, specfile = filepath } =
	let val local_dir = P.dir (F.fullPath filepath)
	    fun set x = set0 (fn fp => P.mkAbsolute { path = fp, relativeTo = local_dir }) x

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

    (* bind : env -> {anchor: anchor, pf: prefile} list -> env *)
    (* redefines only the "bound" field of the env, i.e. produces a new env record
     * with (only) the bound field modified.  Anchors are bound to corresponding 
     * prefiles, with these bindings being added to the existing "bound" mapping.
     * (Perhaps?) called only in main/general-params.sml *)
    fun bind ({get, defined, set, reset, defined, bound}: env) l =
	let fun folder ({ anchor, value = pf }, m) = SM.insert (m, anchor, pf)
	 in { get = get, set = set, reset = reset, defined = defined,
	      bound = foldl folder bound l }
	end

    (* file0 : prefile -> file0 *)
    fun file0 ({ dir, arcs }: prefile) : file0 =
	{ dir = dir,
	  arcs = (case arcs
		    of nil => (error ["path needs at least one arc relative to `",
				      prepathToFilepath (#pp (elab_dir context)), "'"];
			       ["<bogus>"])   (* DBM: Is a file0 with no arcs bogus? *)
		     | _ => arcs),
	  elab = ref bogus_elab, (* why bogus? why not create an "elab" here? *)
	  id = ref NONE }  (* why not generate a new fileid here? *)
	     
    (* file : prefile -> file *)
    val file = intern o file0

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
              | ANCHORED (anchor, arcs) => {dir = ANCHOR(mk_anchor (env, anchor)), arcs = arcs}
          (* end case *))

    (* standard : env -> dir * filepath -> prefile *)
    fun standard env (dir, filepath) =
	(case parseFilepathStandard filepath
	   of RELATIVE arcs => {dir = dir, arcs = arcs}
	    | ABSOLUTE arcs => {dir = ROOT "", arcs = arcs}
	    | ANCHORED (anchor, arcs) => {dir = ANCHOR (mk_anchor (env, anchor)), arcs = arcs}
        (* end case *))

    (* extend: prefile -> string list -> prefile *)
    fun extend { dir, arcs } morearcs =
	{ dir = dir, arcs = arcs @ morearcs }

    (* osstring : file -> string *)
    val osstring = I.canonical o prepathToFilepath o #pp o elab_file o unintern

    (* osstring_prefile : prefile -> string *)
    fun osstring_prefile { dir, arcs } =
	I.canonical (prepathToFilepath (#pp (extendElab arcs (elab_dir dir))))

    (* osstring_dir : dir -> string *)
    fun osstring_dir d =
	case prepathToFilepath (#pp (elab_dir d))
	  of "" => P.currentArc
	   | s => I.canonical s

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

    (* osstring_reanchored : (string -> string) -> file -> filepath option *)
    (* used once in main/filename-policy.sml (FilenamePolicyFn) *)
    fun osstring_reanchored cvt file =
	Option.map (I.canonical o prepathToFilepath)
		   (#reanchor (elab_file (unintern file)) cvt)

    (* osstring_prefile_relative : prefile -> filepath *)
    fun osstring_prefile_relative (pf as { dir, arcs }: prefile) =
	case dir
	  of DIR _ => I.canonical (P.toString { arcs = arcs, vol = "", isAbs = false })
	   | _ => osstring_prefile pf

    (* osstring_relative : file -> filepath *)
    val osstring_relative = osstring_prefile_relative o fileToPrefile
    
    (* tstamp : file -> TSamp.t *)
    fun tstamp f = TStamp.fmodTime (osstring f)

    (* pickle : (bool * string -> unit) -> {file: prefile, relativeTo : ?} -> ? *)
    fun pickle warn { file = (pf as {dir,arcs}: prefile), relativeTo = (gf, _) } =
	let fun warn' (flag: bool) =
		warn (flag, encodePrefilePlain { arcs = arcs, dir = dir })
		     (* HACK! We are cheating here, turning the prefile into
		      * a file even when there are no arcs.  This is ok
		      * because of (bracket = false) for encodePrefilePlain. *)

	    fun p_p p = p_pf (file0ToPrefile p)

	    and p_pf ({ dir, arcs } : prefile) = arcs :: p_d dir

	    and p_d (ROOT vol) = (warn true; [[vol, "r"]])
	      | p_d CWD = impossible "pickle: CWD"
	      | p_d (ANCHOR { name, ... }) = [[name, "a"]]
	      | p_d (DIR f0) =
		(case compare0 (f0, gf)
		   of EQUAL => (warn' false; [["c"]])
		    | _ => p_p f0)

	 in p_pf pf
	end

    (* unpickle : env -> {ickled: ?, relativeTo: ?} -> ? *)
    fun unpickle env { pickled, relativeTo } =
	let fun u_pf (arcs :: l) = {dir = u_d l, arcs = arcs}
	      | u_pf _ = raise Format

	    and u_p l = file0 (u_pf l)

	    and u_d [[vol, "r"]] = ROOT vol
	      | u_d [["c"]] = dir relativeTo
	      | u_d [[n, "a"]] = ANCHOR (mk_anchor (env, n))
	      | u_d l = DIR (u_p l)

	 in u_pf pickled
	end

    (* decode : env -> string -> ? *)
    fun decode env s =
	let fun isChar (c1: char) c2 = c1 = c2

	    fun transCode (s: string) : string =
	        let val dc = Char.chr o valOf o Int.fromString o implode
		    fun loop ([], r) = String.implode (rev r)
		      | loop (#"\\" :: d0 :: d1 :: d2 :: l, r) =
			  (loop (l, dc [d0, d1, d2] :: r)
			  handle _ => loop (l, d2 :: d1 :: d0 :: #"\\" :: r))
		      | loop (c :: l, r) = loop (l, c :: r)
		 in loop (String.explode s, [])
		end

	    fun arc "." = P.currentArc  (* = "." *)
	      | arc ".." = P.parentArc  (* = ".." *)
	      | arc a = transCode a

	    (* file : dir * string list -> file0 *)
	    fun file (dir, arcs) = file0 {dir = dir, arcs = arcs}

	    fun addseg (seg, p) =
		file (dir0 p, map arc (String.fields (isChar #"/") seg))

	    fun firstSeg s =
		(case String.fields (isChar #"/") s
		   of nil => impossible "decode: no fields in segment 0"
		    | arc0 :: arcs =>
		       let val arcs = map arc arcs
			   val arc0' = transCode (String.extract (arc0, 1, NONE))
			in if arc0 = ""
			   then file (ROOT "", arcs)
			   else (case String.sub (arc0, 0)
				   of #"%" => file (ROOT (xtr ()), arcs)
				    | #"$" =>
					let val n = xtr ()
					 in file (ANCHOR (mk_anchor (env, n)), arcs)
					end
				    | _ => file (cwd (), arc arc0 :: arcs))
		       end)

	 in case String.fields (isChar #":") s
	      of nil => impossible "decode: no segments"
	       | seg0 :: segs => intern (foldl addseg (firstSeg seg0) segs)
	end (* fun decode *)

    (* encodeingIsAbsolute : string -> bool *)
    fun encodingIsAbsolute s =
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
*)
