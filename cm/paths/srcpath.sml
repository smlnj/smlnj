(* srcpath.sml
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Operations over abstract names for CM source files.
 *
 * Author: Matthias Blume
 *)

signature SRCPATH =
  sig

    exception Format
    (* When faced with an undefined anchor, pressing on does not
     * make much sense.  Therefore, we raise an exception in this
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
end

structure SrcPath :> SRCPATH =
  struct

    exception Format
    exception BadAnchor

    structure P = OS.Path
    structure F = OS.FileSys
    structure I = FileId
    structure StringMap = RedBlackMapFn (struct type ord_key = string
						val compare = String.compare
					 end)

    fun impossible s = raise Fail ("impossible error in SrcPath: " ^ s)

    type anchor = string

    type stableid = int

    (* A pre-path is similar to the result of P.fromString except that
     * we keep the list of arcs in reversed order.  This makes adding
     * and removing arcs at the end easier. *)
    type prepath = { revarcs: string list, vol: string, isAbs: bool }

    type elab = { pp: prepath,
		  valid: unit -> bool,
		  reanchor: (anchor -> string) -> prepath option }

    type anchorval = (unit -> elab) * (bool -> string)

    datatype dir =
	CWD of { name: string, pp: prepath }
      | ANCHOR of { name: anchor, look: unit -> elab,
		    encode : bool -> string option }
      | ROOT of string
      | DIR of file0

    and file0 =
	PATH of { context: dir,
		  arcs: string list,	(* at least one arc! *)
		  elab: elab ref,
		  id: I.id option ref }

    type file = file0 * stableid

    type prefile = { context: dir, arcs: string list, err: string -> unit }

    type rebindings = { anchor: anchor, value: prefile } list

    type env =
	 { get_free: anchor * (string -> unit) -> elab,
	   set_free: anchor * prepath option -> unit,
	   is_set: anchor -> bool,
	   reset: unit -> unit,
	   bound: anchorval StringMap.map }

    type ord_key = file

    (* stable comparison *)
    fun compare (f1: file, f2: file) = Int.compare (#2 f1, #2 f2)

    val null_pp : prepath = { revarcs = [], vol = "", isAbs = false }
    val bogus_elab =
	{ pp = null_pp, valid = fn _ => false, reanchor = fn _ => NONE }

    fun string2pp n = let
	val { arcs, vol, isAbs } = P.fromString n
    in
	{ revarcs = rev arcs, vol = vol, isAbs = isAbs }
    end

    val cwd_info =
	let val n = F.getDir ()
	in ref { name = n, pp = string2pp n }
	end
    val cwd_notify = ref true

    fun absElab (arcs, vol) =
	{ pp = { revarcs = rev arcs, vol = vol, isAbs = true },
	  valid = fn () => true, reanchor = fn _ => NONE }

    fun unintern (f: file) = #1 f

    fun pre0 (PATH { arcs, context, ... }) =
	{ arcs = arcs, context = context, err = fn (_: string) => () }
    val pre = pre0 o unintern

    fun encode0 bracket (pf: prefile) = let
	fun needesc c = not (Char.isPrint c) orelse Char.contains "/:\\$%()" c
	fun esc c =
	    "\\" ^ StringCvt.padLeft #"0" 3 (Int.toString (Char.ord c))
	fun tc c = if needesc c then esc c else String.str c
	val ta = String.translate tc
	val (dot, dotdot) = let
	    val ta' = String.translate esc
	in
	    (ta' ".", ta' "..")
	end
	infixr 5 ::/::
	fun arc ::/:: [] = [arc]
	  | arc ::/:: a = arc :: "/" :: a
	fun arc a =
	    if a = P.currentArc then "."
	    else if a = P.parentArc then ".."
	    else if a = "." then dot
	    else if a = ".." then dotdot
	    else ta a
	fun e_ac ([], context, _, a) = e_c (context, a, NONE)
	  | e_ac (arcs, context, ctxt, a) =
	    let val l = map arc arcs
		val a0 = List.hd l
		val l' = map arc (rev l)
		val l'' = if ctxt andalso bracket then
			      concat ["(", List.hd l', ")"] :: List.tl l'
			else l'
		val a' = foldl (fn (x, l) => x ::/:: l)
			      (List.hd l'' :: a) (List.tl l'')
	    in e_c (context, a', SOME a0)
	    end
	and e_c (ROOT "", a, _) = concat ("/" :: a)
	  | e_c (ROOT vol, a, _) = concat ("%" :: ta vol ::/:: a)
	  | e_c (CWD _, a, _) = concat a
	  | e_c (ANCHOR x, a, a1opt) =
	    (case (#encode x bracket, a1opt) of
		 (SOME ad, _) =>
		 if not bracket then concat (ad ::/:: a)
		 else concat ("$" :: ta (#name x) :: "(=" :: ad :: ")/" :: a)
	       | (NONE, NONE) => concat ("$" :: ta (#name x) ::/:: a)
	       | (NONE, SOME a1) => let
		     val a0 = ta (#name x)
		 in
		     concat (if bracket andalso a0 = a1 then "$/" :: a
			     else "$" :: a0 ::/:: a)
		 end)
	  | e_c (DIR (PATH { arcs, context, ... }), a, _) =
	    e_ac (arcs, context, true, ":" :: a)
    in
	e_ac (#arcs pf, #context pf, false, [])
    end

    fun mk_anchor (e: env, a, err) =
	case StringMap.find (#bound e, a) of
	    SOME (elaborate, encode) =>
	      { name = a , look = elaborate, encode = SOME o encode }
	  | NONE =>
	      { name = a, look = fn () => #get_free e (a, err),
		encode = fn _ => NONE }

    val encode_prefile = encode0 false
    val encode = encode_prefile o pre

    val clients = ref ([] : (string -> unit) list)
    fun addClientToBeNotified c = clients := c :: !clients

    fun revalidateCwd () = let
	val { name = n, pp } = !cwd_info
	val n' = F.getDir ()
	val pp' = string2pp n'
    in
	if n = n' then ()
	else (cwd_info := { name = n', pp = pp' };
	      cwd_notify := true);
	if !cwd_notify then
	    let val pf = { arcs = rev (#revarcs pp),
			   context = ROOT (#vol pp),
			   err = fn (_: string) => () }
		val ep = encode_prefile pf
	    in
		app (fn c => c ep) (!clients);
		cwd_notify := false
	    end
	else ()
    end

    fun scheduleNotification () = cwd_notify := true

    fun dirPP { revarcs = _ :: revarcs, vol, isAbs } =
	{ revarcs = revarcs, vol = vol, isAbs = isAbs }
      | dirPP _ = impossible "dirPP"

    fun dirElab { pp, valid, reanchor } =
	{ pp = dirPP pp, valid = valid,
	  reanchor = Option.map dirPP o reanchor }

    fun augPP arcs { revarcs, vol, isAbs } =
	{ revarcs = List.revAppend (arcs, revarcs), vol = vol, isAbs = isAbs }

    fun augElab arcs { pp, valid, reanchor } =
	{ pp = augPP arcs pp, valid = valid,
	  reanchor = Option.map (augPP arcs) o reanchor }

    fun elab_dir (CWD { name, pp }) =
	let fun valid () = name = #name (!cwd_info)
	    fun reanchor (a: anchor -> string) = NONE
	in
	    if valid () then { pp = null_pp, valid = valid,
			       reanchor = reanchor }
	    else { pp = pp, valid = fn () => true, reanchor = reanchor }
	end
      | elab_dir (ANCHOR { name, look, encode }) = look ()
      | elab_dir (ROOT vol) = absElab ([], vol)
      | elab_dir (DIR p) = dirElab (elab_file p)

    and elab_file (PATH { context, arcs, elab, id }) =
	let val e as { pp, valid, reanchor } = !elab
	in
	    if valid () then e
	    else let val e' = augElab arcs (elab_dir context)
		 in elab := e'; id := NONE; e'
		 end
	end

    fun pp2name { revarcs, vol, isAbs } =
	P.toString { arcs = rev revarcs, vol = vol, isAbs = isAbs }

    fun idOf (p as PATH { id, ... }) =
	let val { pp, ... } = elab_file p
	in
	    case !id of
		SOME i => i
	      | NONE => let
		    val i = I.fileId (pp2name pp)
		in
		    id := SOME i; i
		end
	end

    fun compare0 (f1, f2) = I.compare (idOf f1, idOf f2)

    structure F0M = RedBlackMapFn (type ord_key = file0
                                     val compare = compare0)

    local
	val known = ref (F0M.empty: int F0M.map)
	val next = ref 0
    in
        fun clear () = known := F0M.empty

	fun intern f =
	    case F0M.find (!known, f) of
		SOME i => (f, i)
	      | NONE => let
		    val i = !next
		in
		    next := i + 1;
		    known := F0M.insert (!known, f, i);
		    (f, i)
		end

	fun sync () = let
	    val km = !known
	    fun inval (PATH { id, ... }, _) = id := NONE
	    fun reinsert (k, v, m) = F0M.insert (m, k, v)
	in
	    F0M.appi inval km;
	    known := F0M.foldli reinsert F0M.empty km
	end
    end

    val dir0 = DIR
    val dir = dir0 o unintern

    fun cwd () = (revalidateCwd (); CWD (!cwd_info))

    val osstring = I.canonical o pp2name o #pp o elab_file o unintern

    fun osstring_prefile { context, arcs, err } =
	I.canonical (pp2name (#pp (augElab arcs (elab_dir context))))

    val descr = encode0 true o pre

    fun osstring_dir d =
	case pp2name (#pp (elab_dir d)) of
	    "" => P.currentArc
	  | s => I.canonical s

    fun osstring' f = let
	val oss = osstring f
    in
	if P.isAbsolute oss then
	    let val ross =
		    P.mkRelative { path = oss, relativeTo = #name (!cwd_info) }
	    in
		if size ross < size oss then ross else oss
	    end
	else oss
    end

    fun newEnv () = let
	val freeMap = ref StringMap.empty
	fun fetch a =
	    case StringMap.find (!freeMap, a) of
		SOME (pp, validity) => (SOME pp, validity)
	      | NONE => (NONE, ref false)
		(*
		let
		    val validity = ref true
		    val pp = { revarcs = [concat ["$Undef<", a, ">"]],
			       vol = "", isAbs = false }
		    val x = (pp, validity)
		in
		    freeMap := StringMap.insert (!freeMap, a, x);
		    x
		end
		 *)
	fun get_free (a, err) =
	    case fetch a of
		(SOME pp, validity) =>
		  let fun reanchor cvt = SOME (string2pp (cvt a))
		  in { pp = pp, valid = fn () => !validity,
		       reanchor = reanchor }
		end
	      | (NONE, _) => (err ("anchor $" ^ a ^ " not defined");
			      raise BadAnchor)
	fun set_free (a, ppo) = let
	    val (_, validity) = fetch a
	in
	    validity := false;		(* invalidate earlier elabs *)
	    freeMap :=
	    (case ppo of
		 NONE => #1 (StringMap.remove (!freeMap, a))
	       | SOME pp => StringMap.insert (!freeMap, a, (pp, ref true)))
	end
	fun is_set a = StringMap.inDomain (!freeMap, a)
	fun reset () = let
	    fun invalidate (_, validity) = validity := false
	in
	    StringMap.app invalidate (!freeMap);
	    freeMap := StringMap.empty
	end
    in
	{ get_free = get_free, set_free = set_free, is_set = is_set,
	  reset = reset, bound = StringMap.empty } : env
    end

    fun get_anchor (e: env, a) =
	if #is_set e a then SOME (pp2name (#pp (#get_free e (a, fn _ => ()))))
	else NONE

    fun set0 mkAbsolute (e: env, a, so) = let
	fun name2pp s = string2pp (if P.isAbsolute s then s else mkAbsolute s)
    in
	#set_free e (a, Option.map name2pp so)
    end

    fun set_anchor x =
	set0 (fn n => P.mkAbsolute { path = n, relativeTo = F.getDir () }) x
	before sync ()

    fun reset_anchors (e: env) = (#reset e (); sync ())

    fun processSpecFile { env = e, specfile = f, say } = let
	val d = P.dir (F.fullPath f)
	fun set x = set0 (fn n => P.mkAbsolute { path = n, relativeTo = d }) x
	fun mknative true d = d
	  | mknative false d = let
		fun return (abs, arcs) =
		    P.toString { vol = "", isAbs = abs, arcs = arcs }
	    in
		case String.fields (fn c => c = #"/") d of
		    "" :: arcs => return (true, arcs)
		  | arcs => return (false, arcs)
	    end
	fun work s = let
	    fun loop isnative =
		case TextIO.inputLine s of
		    NONE => ()
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
	in
	    loop true
	end
    in
	work
    end

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
	    StringMap.insert (m, anchor,
			      (fn () => augElab arcs (elab_dir context),
			       fn brack => encode0 brack pf))

    in
	{ get_free = #get_free env, set_free = #set_free env,
	  reset = #reset env, is_set = #is_set env,
	  bound = foldl b (#bound env) l }
    end

    fun file0 ({ context, arcs, err }: prefile) =
	PATH { context = context, elab = ref bogus_elab, id = ref NONE,
	       arcs = (case arcs of
			   [] => (err (concat
				  ["path needs at least one arc relative to `",
				   pp2name (#pp (elab_dir context)), "'"]);
				  ["<bogus>"])
			 | _ => arcs) }

    val file = intern o file0

    fun prefile (c, l, e) = { context = c, arcs = l, err = e }

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
	Option.map (I.canonical o pp2name)
		   (#reanchor (elab_file (unintern f)) cvt)

    fun osstring_prefile_relative (p as { arcs, context, ... }) =
	case context of
	    DIR _ => I.canonical
			 (P.toString { arcs = arcs, vol = "", isAbs = false })
	  | _ => osstring_prefile p

    val osstring_relative = osstring_prefile_relative o pre

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
	fun p_p p = p_pf (pre0 p)
	and p_pf { arcs, context, err } =
	    arcs :: p_c context
	and p_c (ROOT vol) = (warn true; [[vol, "r"]])
	  | p_c (CWD _) = impossible "pickle: CWD"
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
end
