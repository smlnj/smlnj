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
  structure I = FileId      (* srcpath-lib.cm *)
  structure SM = StringMap  (* ../util/sources.cm *)

in

    exception Format
    exception BadAnchor

    fun impossible msg = EM.impossible ("SrcPath: " ^ msg)

    type anchor = string  (* the "name" of an anchor, e.g. "smlnj" for the anchor $smlnj? *)

    type filepath = string (* the file path string of a file in the host OS *)

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
     *   -- renaming (moving) the file, or deleting the file?
     * What does the reanchor function do? What should be passed as its argument?
     *)
    type elab = { pp: prepath,
		  valid: unit -> bool,
		  reanchor: (anchor -> string) -> prepath option }

    (* anchorInfo: information associated with an ANCHOR dir (directory).
     * What does look do?
     * What does encode do?  What is the bool argument for?
     *)
    type anchorInfo =
         {name: anchor,  (* i.e. string *)
	  look: unit -> elab,
	  encode : bool -> string option}

    (* type cwdInfo: information defining the current working directory.
     * The pp prepath is derived from the name, which is obtained from F.getDir.
     * The information for the current working directory is stored in the global
     * ref variable cwd_info. The current working directory can be changed (from
     * SML) by calling OS.FileSys.chDir. If this is done, the value stored in
     * cwd_info can become out of date. What should happen then? *)
    type cwdInfo =
	   { name: string,  (* the raw filepath of the current working directory *)
	     pp: prepath }  (* the prepath derived from the filepath *)

    (* dir: supposed to denote file system directories? *)
    datatype dir
      = CWD                   (* "the" current working directory, as stored in !cwd_info *)
      | ANCHOR of anchorInfo  (* generally, anchors "denote" directories *)
      | ROOT of string        (* Unix: string = "", Windows: string is a volume name *)
      | DIR of file0          (* a representation of a file-system directory? *)

    (* file0: representation of a (semantic?) file?
     * How are (dir, arcs) related to the prepath component of the elab component?
     * Should they be consistent? If so, is this enforced?
     * When will the elab component be changed?  (* elab_file, ... *)
     * When will the id component be changed?
     * Could have used "withtype" instead of introducing the PATH constructor. *)
    withtype file0 = (* PATH constructor deleted *)
      { dir: dir,             (* formerly "context" *)
	arcs: string list,	  (* filepath relative to dir: INVARIANT: length arc >= 1 *)
	elab: elab ref,       (* the elab should be derived from, or at least consistent with
				 the file's dir and arcs.  See elab_dir, elab_file. *)
	id: I.id option ref }

    type file = file0 * stableid

    (* prefile: a record that designates a file that may not exist in the file system.
     * Are the arcs in normal (file system) order, or are they reversed as in prepaths?
     *   *** Let's assume (tentatively) that they are in "file system order", with outermost
             arc first. ***
     * When is the err function called? *)
    type prefile = { dir: dir,
		     arcs: string list,
		     err: string -> unit }  (* where/when is the err component called? *)

    (* rebindings: a finite map from anchors (strings) to prefiles *)
    type rebindings = { anchor: anchor, value: prefile } list

    (* DBM: What is this? Some kind of anchor "thunk"? *)
    type anchorval = (unit -> elab) * (bool -> string)

    (* env: a kind of "object-like", stateful environment? *)
    type env =
	 { get_free: anchor * (string -> unit) -> elab,
	   set_free: anchor * prepath option -> unit,
	   is_set: anchor -> bool,
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

    (* cwd_info : cwdInfo ref *)
    (* a ref initialized with the name and prepath of the current working directory;
     * this should be updated if the currend working directory is changed? *)
    val cwd_info =
	let val filepath : string = F.getDir ()  (* full, absolute, filepath of c.w.d. *)
	    val cwdPrepath = filepathToPrepath filepath  (* absolute prepath *)
	 in ref { name = filepath, pp = cwdPrepath }
	end

    (* cwd_notify : bool ref -- a simple control flag? *)
    val cwd_notify = ref true

    (* absoluteElab [absElab] : string list * string -> elab *)			 
    fun absoluteElab (arcs, vol) =
	{ pp = { revarcs = rev arcs, vol = vol, isAbs = true },
	  valid = fn () => true, reanchor = fn _ => NONE }

    (* unintern : file -> file0 *)
    fun unintern (f: file) : file0 = #1 f

    (* file0ToPrefile [pre0] : file0 -> prefile *)
    fun file0ToPrefile ({ dir, arcs, ... }: file0) =
	{ arcs = arcs, dir = dir, err = fn (_: string) => () }

    (* fileToPrefile [pre] : file -> prefile *)
    fun fileToPrefile (f: file) = file0ToPrefile (unintern f)

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

    (* encode0 : [bracket:]bool -> prefile -> string *)
    (* encode0 appears to translate a prefile into a string using filepath notation enhanced
     * somehow with the anchor notation, and if bracket is true, sometimes an "alias" of an
     * anchor is included? e.g. foo(=bar) when the encode component of n ANCHOR returns
     * SOME "bar". The internal function names, e_ac and e_c, are not informative and need
     * to be changed. *)
    fun encode0 (bracket: bool) ({dir = prefile_dir, arcs = prefile_arcs, ...}: prefile) =
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
	    and e_c (ROOT "", path, _) = concat ("/" :: path)  (* make path absolute, rooted *)
	      | e_c (ROOT vol, path, _) = concat ("%" :: transSpecial vol :: "/" :: path)
	      | e_c (CWD, path, _) = concat path  (* file path relative to c.w.d. *)
	      | e_c (ANCHOR {name, encode, ...}, path, stringOp2) =
		  let val name' = transSpecial name (* get rid of special chars in anchor name *)
		      val path : string list =
			    (* path is a list of path component strings consisting of arcs, 
			       "$", "/", ":" and other anchor related notations? *)
			    (case encode bracket
			       of SOME str1 => (* anchor has an "alias" valof(str1)? *)
				    if bracket
				    then "$" :: name' :: "(=" :: str1 :: ")/" :: path
				    else str1 :: "/" :: path
				| NONE => 
				    (case stringOp2
				       of SOME str2 =>
					    if bracket andalso str2 = name'
					    then "$/" :: path
					    else "$" :: str2 :: "/" :: path
					| NONE => "$" :: name' :: "/" :: path))
		   in concat path
		  end
	      | e_c (DIR ({ dir, arcs, ... }: file0), path, _) =
		  e_ac (dir, arcs, true, ":" :: path)

	 in e_ac (prefile_dir, prefile_arcs, false, [])
	end (* fun encode0 *)


    (* mk_anchor : env * anchor * (string -> unit) -> anchorInfo *)
    fun mk_anchor ({bound, get_free, ...} : env, a, err) =
	case SM.find (bound, a)
	  of SOME (elaborate, encode) =>
	      { name = a , look = elaborate, encode = SOME o encode }
	   | NONE =>
	      { name = a, look = fn () => get_free (a, err),
		encode = fn _ => NONE }

    (* encode_prefile : prefile -> string *)
    val encode_prefile = encode0 false

    (* encode : file -> string *)
    val encode = encode_prefile o fileToPrefile

    (* clients : (string -> unit) list ref *)
    val clients = ref ([] : (string -> unit) list)

    (* addClientToBeNotified : (string -> unit) -> unit *)
    fun addClientToBeNotified c = clients := c :: !clients

    (* revalidateCwd : unit -> unit *)
    fun revalidateCwd () =
	let val { name = filepath, pp = {revarcs, vol, ...}} = !cwd_info
	    val new_cwd = F.getDir ()
	 in if filepath = new_cwd
	    then ()  (* validated, meaning !cwd_info is still current *)
	    else (* current working directory must have changed *)
	      let val pp' = filepathToPrepath new_cwd
	       in cwd_info := { name = new_cwd, pp = pp' };
		  cwd_notify := true
	      end
	    if !cwd_notify
	    then let val prefile = { arcs = rev revarcs,
				     dir = ROOT vol,   (* vol = "" for Unix *)
				     err = fn (_: string) => () }
		     val ep = encode_prefile prefile
		  in app (fn c => c ep) (!clients);
		     cwd_notify := false
		 end
	    else ()
	end

    (* scheduleNotification : unit -> unit *)
    fun scheduleNotification () = cwd_notify := true

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

    (* currentWorkingDirectory : unit -> string *)
    (* returns the filepath for the current working directory, according to the contents of cwd_info.
     * This filepath should be absolute (and == OS.FileSys.getDir()?).
     * Or should we call OS.FileSys.getDir for the result, and if necessary reset cwd_info? *)
    fun currentWorkingDirectory () =
	let val {name, ...} = !cwd_info
	 in name
	end

    (* elab_dir : dir -> elab *)
    fun elab_dir CWD =
	  let val {name, pp} = !cwd_info
	      fun valid ()  = (currentCwd () = name)  (* DBM: could be out of date! *)
	      fun reanchor (a: anchor -> string) = NONE
	   in { pp = filepathToPrepath name, valid = valid, reanchor = reanchor }
	  end
      | elab_dir (ANCHOR { name, look, encode }) = look ()
      | elab_dir (ROOT vol) = absoluteElab ([], vol)
      | elab_dir (DIR p) = parentElab (elab_file p)

    (* elab_file : file0 -> elab *)
    and elab_file ({ dir, arcs, elab, id }: file0) =
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

    (* cwd : unit -> string *)
    (* returns the filepath of the current working directory, making sure that cwd_info is
     * up-to-date. *)			  
    fun cwd () = (revalidateCwd (); !cwd_info)
    		     
    (* osstring : file -> string *)
    val osstring = I.canonical o prepathToFilepath o #pp o elab_file o unintern

    (* osstring_prefile : prefile -> string *)
    fun osstring_prefile { dir, arcs, err } =
	I.canonical (prepathToFilepath (#pp (extendElab arcs (elab_dir context))))

    (* descr : file -> string *)
    val descr = encode0 true o fileToPrefile

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

    (* newEnv : unit -> env *)
    (* creates a new anchor environment "object"
     * The environment mapping (SM.map) is stored in the anchorMapRef reference. It maps anchors
     * (i.e. strings) to (prepath * bool ref), where the bool ref is the "validity" flag.
     * So an anchor can be mapped in !freeMap, but still be "invalid" if the validity flag
     * is false. Conversely, a mapping entry may be removed from !anchorMapRef, yet its validity
     * flag may have been incorporated into existing elabs. *)
    fun newEnv () =
	let val anchorMapRef : (prepath * bool ref) SM.map ref = ref SM.empty

	    (* fetch : anchor -> (prepath option * bool ref) *)
	    fun fetch anchor = SM.find (!anchorMapRef, anchor *)
		
	    (* get_free : anchor * (anchor -> unit) -> elab *) 
	    (* look up anchor in !anchorMapRef. If found, return a new elab for the anchor
	     * containing the same prepath and validity as the existing binding. If not
	     * found (anchor is not in the domain of !anchorMapRef), produces an undefined
	     * anchor error message and raises the BadAnchor exception. *)
	    fun get_free (anchor, err) =
		case fetch anchor
		  of SOME (pp, validity) =>
		       let fun reanchor cvt = SOME (filepathToPrepath (cvt anchor))
			in { pp = pp, valid = fn () => !validity,
			     reanchor = reanchor }
		       end
		   | NONE =>
		       (err ("(get_free) undefined anchor $" ^ anchor);
		        raise BadAnchor)

	    (* set_free : anchor * prepath option -> unit *) 
	    fun set_free (anchor, prepathOp) =
		case SM.find (!anchorMapRef, anchor)
		  of SOME (pp, validity) =>
		       (validity := false;   (* invalidate previously created elabs *)
		        anchorMapRef :=
		          (case prepathOp
		             of SOME pp => SM.insert (!anchorMapRef, anchor, (pp, ref true))
			      | NONE => #1 (SM.remove (!anchorMapRef, anchor)))) (* won't raise NotFound! *)
		   | NONE => impossible ("(set_free) undefined anchor:" ^ anchor)))
		end
		    
	    (* is_set : anchor -> bool *)
	    fun is_set anchor = SM.inDomain (!anchorMapRef, a)

	    (* reset : unit -> unit *)
	    (* set validity flags of all entries in !anchorMapRef to false and assign SM.empty
	     * to the anchorMapRef ref. The validity flags may have been incorporated into
	     * previously created elabs, and so remain relevant. *)
	    fun reset () =
		let fun invalidate (_, validity) = validity := false
	         in SM.app invalidate (!anchorMapRef);
		    anchorMapRef := SM.empty
		end

	 in { get_free = get_free, set_free = set_free, is_set = is_set,
	      reset = reset, bound = SM.empty } : env
	end

    (* get_anchor : env * anchor -> filepath option *)
    fun get_anchor ({is_set, get_free, ...}: env, anchor) =
	if is_set anchor
	then SOME (prepathToFilepath (#pp (get_free (anchor, fn _ => ()))))
	else NONE

    (* set0: (filepath -> filepath) -> env * anchor * filepath option -> unit *)
    fun set0 mkAbsolute ({set_free, ...}: env, anchor, filepathOp) =
	let fun fp_pp (filepath: filepath) =
		filepathToPrepath (if P.isAbsolute filepath then filepath else mkAbsolute filepath)
	 in set_free (anchor, Option.map fp_pp filepathOp)
	end

    (* set_anchor : env * anchor * filepath option -> unit *)
    fun set_anchor args =
	set0 (fn filepath => P.mkAbsolute { path = filepath, relativeTo = F.getDir () }) args
	before sync ()

    (* reset_anchors : env -> unit *)
    fun reset_anchors (e: env) = (#reset e (); sync ())

    (* processSpecFile : {env: env, specfile: filepath, say: string -> unit} -> (TextIO.instream -> ?) *)
    fun processSpecFile { env = e, specfile = filepath, say } =
	let val local_dir = P.dir (F.fullPath filepath)
	    fun set x = set0 (fn fp => P.mkAbsolute { path = fp, relativeTo = local_dir }) x

	    fun mknative true d = d
	      | mknative false d =
		  let fun return (abs, arcs) =
			  P.toString { vol = "", isAbs = abs, arcs = arcs }
		   in case String.fields (fn c => c = #"/") d
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
				      | _ => (say [f, ": malformed line (ignored)\n"];
					      loop isnative)
		 in loop true
		end

	 in work
	end (* fun processSpecFile *)

    datatype stdspec
      = RELATIVE of string list
      | ABSOLUTE of string list
      | ANCHORED of anchor * string list

    fun parseNativeSpec err s = let
          val impossible = fn s => impossible ("AbsPath.parseNativeSpec: " ^ s)
          val {isAbs, arcs, vol} = OS.Path.fromString s
          in
            case arcs
             of [""] => impossible "zero-length name"
              | [] => impossible "no fields"
              | (["$"] | "$"::""::_) => (
                  err (concat ["invalid zero-length anchor name in: `", s, "'"]);
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

    fun parseStdspec err s = let
	fun delim #"/" = true
	  | delim #"\\" = true
	  | delim _ = false
	fun transl ".." = P.parentArc
	  | transl "." = P.currentArc
	  | transl arc = arc
	val impossible = fn s => impossible ("AbsPath.parseStdspec: " ^ s)
    in
	case map transl (String.fields delim s) of
	    [""] => impossible "zero-length name"
	  | [] => impossible "no fields"
	  | "" :: arcs => ABSOLUTE arcs
	  | arcs as (["$"] | "$" :: "" :: _) =>
	    (err (concat ["invalid zero-length anchor name in: `", s, "'"]);
	     RELATIVE arcs)
	  | "$" :: (arcs as (arc1 :: _)) => ANCHORED (arc1, arcs)
	  | arcs as (arc1 :: arcn) =>
	    if String.sub (arc1, 0) <> #"$" then RELATIVE arcs
	    else ANCHORED (String.extract (arc1, 1, NONE), arcn)
    end

    fun bind (env: env) l = let
	fun b ({ anchor, value = pf as { arcs, context, err } }, m) =
	    SM.insert (m, anchor,
			      (fn () => extendElab arcs (elab_dir context),
			       fn brack => encode0 brack pf))

    in
	{ get_free = #get_free env, set_free = #set_free env,
	  reset = #reset env, is_set = #is_set env,
	  bound = foldl b (#bound env) l }
    end

    (* file0 : prefile -> file0 *)
    fun file0 ({ dir, arcs, err }: prefile) : file0 =
	{ dir = dir,
	  arcs = (case arcs
		    of nil => (err (concat
				  ["path needs at least one arc relative to `",
				   prepathToFilepath (#pp (elab_dir context)), "'"]);
				  ["<bogus>"])
		     | _ => arcs),
	  elab = ref bogus_elab, id = ref NONE }
	     
    val file = intern o file0

    (* prefile : dir * string list * (string -> unit) ->  prefile *)
    fun prefile (dir: dir, arcs: string list, err: string -> unit) : prefile =
	{ dir = dir, arcs = arcs, err = err }

    fun raw { err } { context, spec } =
	case P.fromString spec of
	    { arcs, vol, isAbs = true } => prefile (ROOT vol, arcs, err)
	  | { arcs, ... } => prefile (context, arcs, err)

    fun native { env, err }  { context, spec } = (
          case parseNativeSpec err spec
           of RELATIVE l => prefile (context, l, err)
            | ABSOLUTE l => prefile (ROOT "", l, err)
            | ANCHORED(a, l) => prefile (ANCHOR(mk_anchor (env, a, err)), l, err)
        (* end case *))

    fun standard { env, err } { context, spec } = (
	case parseStdspec err spec
	 of RELATIVE l => prefile (context, l, err)
	  | ABSOLUTE l => prefile (ROOT "", l, err)
	  | ANCHORED (a, l) =>
	      prefile (ANCHOR (mk_anchor (env, a, err)), l, err)
        (* end case *))

    fun extend { context, arcs, err } morearcs =
	{ context = context, arcs = arcs @ morearcs, err = err }

    fun osstring_reanchored cvt f =
	Option.map (I.canonical o prepathToFilepath)
		   (#reanchor (elab_file (unintern f)) cvt)

    fun osstring_prefile_relative (p as { arcs, context, ... }) =
	case context of
	    DIR _ => I.canonical
			 (P.toString { arcs = arcs, vol = "", isAbs = false })
	  | _ => osstring_prefile p

    val osstring_relative = osstring_prefile_relative o fileToPrefile
    
    fun tstamp f = TStamp.fmodTime (osstring f)

    fun pickle { warn } { file = (f: prefile), relativeTo = (gf, _) } = let
	val warn =
	    fn flag =>
	       warn (flag,
		     (* HACK! We are cheating here, turning the prefile into
		      * a file even when there are no arcs.  This is ok
		      * because of (bracket = false) for encode0. *)
		     encode_prefile { arcs = #arcs f,
				      context = #context f,
				      err = fn (_: string) => () })
	fun p_p p = p_pf (file0ToPrefile p)
	and p_pf { arcs, context, err } =
	    arcs :: p_c context
	and p_c (ROOT vol) = (warn true; [[vol, "r"]])
	  | p_c CWD = impossible "pickle: CWD"
	  | p_c (ANCHOR { name, ... }) = [[name, "a"]]
	  | p_c (DIR p) = if compare0 (p, gf) = EQUAL then
			      (warn false; [["c"]])
			  else p_p p
    in
	p_pf f
    end

    fun unpickle env { pickled, relativeTo } = let
	fun err _ = raise Format
	fun u_pf (arcs :: l) = prefile (u_c l, arcs, err)
	  | u_pf _ = raise Format
	and u_p l = file0 (u_pf l)
	and u_c [[vol, "r"]] = ROOT vol
	  | u_c [["c"]] = dir relativeTo
	  | u_c [[n, "a"]] = ANCHOR (mk_anchor (env, n, err))
	  | u_c l = DIR (u_p l)
    in
	u_pf pickled
    end

    fun decode env s = let
	fun isChar (c1: char) c2 = c1 = c2
	fun unesc s = let
	    val dc = Char.chr o valOf o Int.fromString o implode
	    fun loop ([], r) = String.implode (rev r)
	      | loop (#"\\" :: d0 :: d1 :: d2 :: l, r) =
		(loop (l, dc [d0, d1, d2] :: r)
		 handle _ => loop (l, d2 :: d1 :: d0 :: #"\\" :: r))
	      | loop (c :: l, r) = loop (l, c :: r)
	in
	    loop (String.explode s, [])
	end
	fun arc "." = P.currentArc
	  | arc ".." = P.parentArc
	  | arc a = unesc a
	fun err s = raise Fail ("SrcPath.decode: " ^ s)
	fun file (c, l) = file0 (prefile (c, l, err))
	fun addseg (seg, p) =
	    file (dir0 p, map arc (String.fields (isChar #"/") seg))
	fun doseg0 s =
	    case String.fields (isChar #"/") s of
		[] => impossible "decode: no fields in segment 0"
	      | arc0 :: arcs => let
		    val arcs = map arc arcs
		    fun xtr () = unesc (String.extract (arc0, 1, NONE))

		    fun say l = TextIO.output (TextIO.stdErr, concat l)
		in
		    if arc0 = "" then file (ROOT "", arcs)
		    else
			case String.sub (arc0, 0) of
			    #"%" => file (ROOT (xtr ()), arcs)
			  | #"$" => let
				val n = xtr ()
			    in
				file (ANCHOR (mk_anchor (env, n, err)), arcs)
			    end
			  | _ => file (cwd (), arc arc0 :: arcs)
		end
    in
	case String.fields (isChar #":") s of
	    [] => impossible "decode: no segments"
	  | seg0 :: segs => intern (foldl addseg (doseg0 seg0) segs)
    end

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
   Was this for "efficiency", or to try to insulate from a changing file system?

3. How much of the "tolerance" of background change is actually being used or is really justified?
   I can see how avoiding dependence on the current working directory would be good, i.e. refering
   to files by paths relative to the current working directory.  But it seems exessive to try to
   accomodate things like someone in the background renaming or deleting files, etc.

*)
