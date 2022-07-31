(*
 * semantic actions to go with the grammar for CM description files
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature CM_SEMANT = sig

    type context = SrcPath.dir
    type region = SourceMap.region
    type ml_symbol
    type cm_symbol
    type cm_class
    type cm_version = Version.t

    type group = GroupGraph.group

    type privilegespec
    type aexp
    type exp
    type members			(* still conditional *)
    type exports			(* still conditional *)

    type toolopt
    type toolregistry

    type complainer = string -> unit

    val newToolRegistry : unit -> toolregistry

    (* getting elements of primitive types (pathnames and symbols) *)
    val file_native : string * context * complainer -> SrcPath.prefile
    val file_standard :
	GeneralParams.info -> string * context * complainer -> SrcPath.prefile
    val cm_symbol : string -> cm_symbol
    val cm_version : string * complainer -> cm_version
    val ml_structure : string -> ml_symbol
    val ml_signature : string -> ml_symbol
    val ml_functor : string -> ml_symbol
    val ml_funsig : string -> ml_symbol
    val class : cm_symbol -> cm_class

    (* getting the full analysis for a group/library *)
    val group : { path: SrcPath.file,
		  privileges: privilegespec,
		  exports: exports,
		  members: members,
		  gp: GeneralParams.info,
		  curlib: SrcPath.file option,
		  initgroup: group } -> group

    val library : { path: SrcPath.file,
		    privileges: privilegespec,
		    exports: exports,
		    version : cm_version option,
		    members: members,
		    gp: GeneralParams.info,
		    initgroup: group } -> group

    (* assembling privilege lists *)
    val initialPrivilegeSpec : privilegespec
    val require : privilegespec * cm_symbol * complainer -> privilegespec
    val wrap : privilegespec * cm_symbol * complainer -> privilegespec

    (* constructing member collections *)
    val emptyMembers : members
    val member :
	{ gp: GeneralParams.info,
	  rparse: SrcPath.file option ->
		  SrcPath.file * Version.t option * SrcPath.rebindings ->
		  group,
	  load_plugin: SrcPath.dir -> string -> bool }
	-> { name: string,
	     mkpath: unit -> SrcPath.prefile,
	     group: SrcPath.file * region,
	     class: cm_class option,
	     tooloptions: toolopt list option,
	     local_registry: toolregistry,
	     context: SrcPath.dir }
	-> members
    val members : members * members -> members
    val guarded_members :
	exp * (members * members) * (string -> unit) -> members
    val error_member : (unit -> unit) -> members

    (* constructing symbol sets *)
    val export : ml_symbol * complainer -> exports
    val union : exports * exports -> exports
    val difference : exports * exports -> exports
    val intersection : exports * exports -> exports
    val exportsource : (unit -> SrcPath.file option) * complainer -> exports
    val exportgroup : (unit -> SrcPath.file option) * complainer -> exports
    val exportlibrary : (unit -> SrcPath.file) * complainer *
			{ hasoptions: bool,
			  elab: unit -> members,
			  curlib: SrcPath.file option }
			-> exports

    (* constructing export lists *)
    val emptyExports : exports
    val guarded_exports : exp * (exports * exports) * complainer -> exports
    val default_group_exports : exports
    val error_export : (unit -> unit) -> exports

    (* groups of operator symbols (to make grammar smaller) *)
    type addsym
    val PLUS : addsym
    val MINUS : addsym

    type mulsym
    val TIMES : mulsym
    val DIV : mulsym
    val MOD : mulsym

    type eqsym
    val EQ : eqsym
    val NE : eqsym

    type ineqsym
    val GT : ineqsym
    val GE : ineqsym
    val LT : ineqsym
    val LE : ineqsym

    (* arithmetic (number-valued) expression *)
    val number : int -> aexp
    val variable : GeneralParams.info -> cm_symbol -> aexp
    val add : aexp * addsym * aexp -> aexp
    val mul : aexp * mulsym * aexp -> aexp
    val sign : addsym * aexp -> aexp
    val negate : aexp -> aexp

    (* (bool-valued) expressions *)
    val boolean : bool -> exp
    val ml_defined : ml_symbol -> exp
    val cm_defined : GeneralParams.info -> cm_symbol -> exp
    val conj : exp * exp -> exp
    val disj : exp * exp -> exp
    val beq : exp * eqsym * exp -> exp
    val not : exp -> exp
    val ineq : aexp * ineqsym * aexp -> exp
    val eq : aexp * eqsym * aexp -> exp

    (* tool options *)
    val string : { name: string, mkpath: unit -> SrcPath.prefile } -> toolopt
    val subopts : { name: string, opts: toolopt list } -> toolopt
end

structure CMSemant :> CM_SEMANT = struct

    structure SymPath = SymPath
    structure EM = ErrorMsg
    structure GG = GroupGraph
    structure MC = MemberCollection

    type context = SrcPath.dir
    type region = SourceMap.region
    type ml_symbol = Symbol.symbol
    type cm_symbol = string
    type cm_class = string
    type cm_version = Version.t

    type group = GG.group
    type privilegespec = { required: GG.privileges, wrapped: GG.privileges }

    type environment = MC.collection

    type exports = environment -> (environment -> unit) SymbolMap.map

    type aexp = environment -> int
    type exp = environment -> bool
    type members = environment * SrcPath.file option -> MC.collection

    type toolopt = PrivateTools.toolopt
    type toolregistry = PrivateTools.registry

    val newToolRegistry = PrivateTools.newRegistry

    type complainer = string -> unit

    fun saveEval (exp, env, error) =
	exp env
	handle exn =>
	    (error ("expression raises exception: " ^ General.exnMessage exn);
	     false)

    fun file_native (s, d, err) =
	SrcPath.raw { err = err } { context = d, spec = s }
    fun file_standard (gp: GeneralParams.info) (s, d, err) =
	SrcPath.standard { env = #penv (#param gp), err = err }
			 { context = d, spec = s }
    fun cm_symbol s = s
    fun cm_version (s, error) =
	case Version.fromString s of
	    SOME v => v
	  | NONE => (error "ill-formed version specification"; Version.zero)
    val ml_structure = Symbol.strSymbol
    val ml_signature = Symbol.sigSymbol
    val ml_functor = Symbol.fctSymbol
    val ml_funsig = Symbol.fsigSymbol

    fun class s = String.map Char.toLower s

    fun applyTo mc e = e mc

    fun sgl2sll subgroups = let
	fun sameSL (p, _, _) (p', _, _) = SrcPath.compare (p, p') = EQUAL
	fun add (x, l) =
	    if List.exists (sameSL x) l then l else x :: l
	fun oneSG (x as (_, gth, _), l) =
	    case gth () of
		GG.GROUP { kind, sublibs, ... } =>
		(case kind of
		     GG.NOLIB _ => foldr add l sublibs
		   | _ => add (x, l))
	      | _ => l
    in
	foldr oneSG [] subgroups
    end

    (* Filter out unused stuff and thunkify the group. *)
    fun filt_th_sgl (sgl, imp_syms) = let
	(* Add fake "structure <Pervasive>" so that we are sure not to lose
	 * the initgroup when filtering. *)
	val ss = SymbolSet.add (imp_syms, PervAccess.pervStrSym)
	fun add ((_, GG.ERRORGROUP, _), l) = l
	  | add ((p, g as GG.GROUP { exports, ... }, rb), l) = let
		fun defined_here sy = SymbolMap.inDomain (exports, sy)
	    in
		if SymbolSet.exists defined_here ss then
		    (p, fn () => g, rb) :: l
		else l
	    end
    in
	foldr add [] sgl
    end

    val \/ = StringSet.union
    infix \/

    fun getExports (mc, e) =
	SymbolMap.foldli (fn (sy, c, s) => (c mc; SymbolSet.add (s, sy)))
			 SymbolSet.empty
			 (applyTo mc e)

    fun group arg = let
	val { path = g, privileges = p, exports = e, members = m,
	      gp, curlib, initgroup } = arg
	val mc = applyTo (MC.implicit gp initgroup, curlib) m
	val filter = getExports (mc, e)
	val pfsbn = let
	    val { exports, ... } =
		case initgroup of
		    GG.GROUP x => x
		  | GG.ERRORGROUP =>
		      EM.impossible "semant.sml: group: bad init group"
	in
	    #1 (valOf (SymbolMap.find (exports, PervAccess.pervStrSym)))
	end
	val _ = MC.mkIndex (gp, g, mc)
	val (exports, rp, isl) =
	    MC.build (g, mc, filter, gp, pfsbn ())
	val subgroups = filt_th_sgl (MC.subgroups mc, isl)
	val { required = rp', wrapped = wr } = p
    in
	if StringSet.isEmpty wr then ()
	else EM.impossible "group with wrapped privileges";
	GG.GROUP { exports = exports,
		   kind = GG.NOLIB { subgroups = subgroups,
				     owner = curlib },
		   required = rp' \/ rp \/ wr,
		   grouppath = g,
		   sources = MC.sources mc,
		   sublibs = sgl2sll subgroups }
    end

    fun library arg = let
	val { path = g, privileges = p, exports = e, members = m,
	      version, gp, initgroup } = arg
	val mc = applyTo (MC.implicit gp initgroup, SOME g) m
	val filter = getExports (mc, e)
	val pfsbn = let
	    val { exports, ... } =
		case initgroup of
		    GG.GROUP x => x
		  | GG.ERRORGROUP =>
		    EM.impossible "semant.sml: lib: bad init group"
	in
	    #1 (valOf (SymbolMap.find (exports, PervAccess.pervStrSym)))
	end
	val _ = MC.mkIndex (gp, g, mc)
	val (exports, rp, isl) =
	    MC.build (g, mc, filter, gp, pfsbn ())
	val subgroups = filt_th_sgl (MC.subgroups mc, isl)
	val { required = rp', wrapped = wr } = p
    in
	GG.GROUP { exports = exports,
		   kind = GG.LIB { version = version,
				   kind = GG.DEVELOPED { subgroups = subgroups,
							 wrapped = wr } },
		   required = rp' \/ rp \/ wr,
		   grouppath = g,
		   sources = MC.sources mc,
		   sublibs = sgl2sll subgroups }
    end

    local
	val isMember = StringSet.member
	fun sanity ({ required, wrapped }, s, error) =
	    if isMember (required, s) orelse isMember (wrapped, s) then
		error ("duplicate privilege name: " ^ s)
	    else ()
    in
	val initialPrivilegeSpec = { required = StringSet.empty,
				     wrapped = StringSet.empty }
	fun require (a as ({ required, wrapped }, s, _)) =
	    (sanity a;
	     { required = StringSet.add (required, s), wrapped = wrapped })
	fun wrap (a as ({ required, wrapped }, s, _)) =
	    (sanity a;
	     { required = required, wrapped = StringSet.add (wrapped, s) })
    end

    fun emptyMembers (env, _) = env
    fun member { gp, rparse, load_plugin } arg (env, curlib) = let
	val coll = MC.expandOne
		       { gp = gp, rparse = rparse curlib,
			 load_plugin = load_plugin }
		       arg
	val group = #group arg
	val error = GroupReg.error (#groupreg gp) group
	fun e0 s = error EM.COMPLAIN s EM.nullErrorBody
    in
	MC.sequential (env, coll, e0)
    end
    fun members (m1, m2) (env, curlib) = m2 (m1 (env, curlib), curlib)
    fun guarded_members (c, (m1, m2), error) (env, curlib) =
	if saveEval (c, env, error) then m1 (env, curlib) else m2 (env, curlib)
    fun error_member thunk (env, _) = (thunk (); env)

    fun symerr s = concat ["exported ",
			   Symbol.nameSpaceToString (Symbol.nameSpace s),
			   " not defined: ", Symbol.name s]

    fun export (s, error) env = let
	fun check final_env =
	    if MC.ml_look final_env s then ()
	    else error (symerr s)
    in
	SymbolMap.singleton (s, check)
    end

    fun union (x, y) env = SymbolMap.unionWith #1 (x env, y env)
    fun difference (x, y) env = let
	val ymap = y env
	fun inY (s, _) = SymbolMap.inDomain (ymap, s)
    in
	SymbolMap.filteri (not o inY) (x env)
    end
    fun intersection (x, y) env = SymbolMap.intersectWith #1 (x env, y env)

    local
	fun withCheckers (ss, error) = let
	    fun add1 (s, m) = let
		fun check final_env =
		    if MC.ml_look final_env s then ()
		    else error (symerr s)
	    in
		SymbolMap.insert (m, s, check)
	    end
	in
	    SymbolSet.foldl add1 SymbolMap.empty ss
	end
	fun exportfile F (foptth, error: string -> unit) (env: environment) =
	    withCheckers (F (env, foptth (), error), error)
    in
        val exportsource =
	    exportfile MC.smlexports
	val exportgroup =
	    exportfile MC.groupexports
	fun exportlibrary (pth, error, { hasoptions, elab, curlib }) env = let
	    fun elab' () = elab () (MC.emptycollection, curlib)
	    val raw = MC.libraryexports (env, pth(), error, hasoptions, elab')
	in
	    withCheckers (raw, error)
	end
    end

    fun emptyExports env = SymbolMap.empty
    fun guarded_exports (c, (e1, e2), error) env =
	if saveEval (c, env, error) then e1 env else e2 env
    fun default_group_exports env =
	union (exportsource (fn () => NONE, fn s => ()),
	       exportgroup (fn () => NONE, fn s => ()))
	      env
    fun error_export thunk env = (thunk (); SymbolMap.empty)

    datatype addsym = PLUS | MINUS
    datatype mulsym = TIMES | DIV | MOD
    datatype eqsym = EQ | NE
    datatype ineqsym = GT | GE | LT | LE

    fun number i _ = i
    fun variable gp v e = MC.num_look gp e v
    fun add (e1, PLUS, e2) e = e1 e + e2 e
      | add (e1, MINUS, e2) e = e1 e - e2 e
    fun mul (e1, TIMES, e2) e = e1 e * e2 e
      | mul (e1, DIV, e2) e = e1 e div e2 e
      | mul (e1, MOD, e2) e = e1 e mod e2 e
    fun sign (PLUS, ex) e = ex e
      | sign (MINUS, ex) e = ~(ex e)
    fun negate ex e = ~(ex e)

    fun boolean b _ = b
    fun ml_defined s e = MC.ml_look e s
    fun cm_defined gp s e = MC.cm_look gp e s
    fun conj (e1, e2) e = e1 e andalso e2 e
    fun disj (e1, e2) e = e1 e orelse e2 e
    fun beq (e1: exp, EQ, e2) e = e1 e = e2 e
      | beq (e1, NE, e2) e = e1 e <> e2 e
    fun not ex e = Bool.not (ex e)
    fun ineq (e1, LT, e2) e = e1 e < e2 e
      | ineq (e1, LE, e2) e = e1 e <= e2 e
      | ineq (e1, GT, e2) e = e1 e > e2 e
      | ineq (e1, GE, e2) e = e1 e >= e2 e
    fun eq (e1: aexp, EQ, e2) e = e1 e = e2 e
      | eq (e1, NE, e2) e = e1 e <> e2 e

    val string = PrivateTools.STRING
    val subopts = PrivateTools.SUBOPTS
end
