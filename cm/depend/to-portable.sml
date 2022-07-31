(* to-portable.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *
 * Generate list-of-edges dependency graph representation from
 * internal CM data structures.
 *
 * author: Matthias Blume (blume@research.bell-labs.com)
 *)
structure ToPortable : sig
    val export : GroupGraph.group * GeneralParams.info ->
		 PortableGraph.graph * SrcPath.file list
end = struct

    structure SS = SymbolSet
    structure GG = GroupGraph
    structure DG = DependencyGraph
    structure P = PortableGraph

    structure SLM = RedBlackMapFn
        (type ord_key = string list
	 fun compare ([], []) = EQUAL
	   | compare ([], _) = LESS
	   | compare (_, []) = GREATER
	   | compare (h :: t, h' :: t') =
	     (case String.compare (h, h') of
		  EQUAL => compare (t, t')
		| unequal => unequal))

    structure FM = RedBlackMapFn
	(type ord_key = string * string
	 fun compare ((v, f), (v', f')) =
	     case String.compare (v, v') of
		 EQUAL => String.compare (f, f')
	       | unequal => unequal)

    structure SSM = RedBlackMapFn
	(type ord_key = SS.set
	 val compare = SS.compare)

    structure IM = RedBlackMapFn
	(type ord_key = SrcPath.file * string
	 fun compare ((p, s), (p', s')) =
	     case SrcPath.compare (p, p') of
		 EQUAL => String.compare (s, s')
	       | unequal => unequal)

    val ignoredSyms = SS.addList (SS.empty,
				  [PervAccess.pervStrSym, CoreSym.coreSym])

    fun export (GG.ERRORGROUP, _) = raise Fail "ToPortable.export ERRORGROUP"
      | export (GG.GROUP { exports, sublibs, grouppath, ... }, gp) = let

	    val cwd = OS.FileSys.getDir ()

	    fun toAbsolute p =
		if OS.Path.isAbsolute p then p
		else OS.Path.mkAbsolute { path = p, relativeTo = cwd }

	    val groupdir =
		OS.Path.dir (toAbsolute (SrcPath.osstring grouppath))

	    local
		fun mkInvMap [] = (SmlInfoMap.empty, StableMap.empty)
		  | mkInvMap ((p, gth, _) :: ls) = let
			val (sm, bm) = mkInvMap ls
			fun update (find, insert) (m, i, (p, ex)) =
			    case find (m, i) of
				NONE => insert (m, i, (p, ex))
			      | SOME (p', ex') => 
				insert (m, i, (p', SS.union (ex, ex')))
			val su = update (SmlInfoMap.find, SmlInfoMap.insert)
			val bu = update (StableMap.find, StableMap.insert)
			fun oneE (sy, (th, _, ex), (sm, bm)) =
			    case th () of
				(_, DG.SB_BNODE (DG.BNODE n, _, _)) =>
				(sm, bu (bm, #bininfo n, (p, ex)))
			      | (_, DG.SB_SNODE (DG.SNODE n)) =>
				(su (sm, #smlinfo n, (p, ex)), bm)
		    in
			case gth () of
			    GG.GROUP { exports, ... } =>
			    SymbolMap.foldli oneE (sm, bm) exports
			  | _ => (sm, bm)
		    end
		val (sm, bm) = mkInvMap sublibs
		fun trim (p, ex) = (p, SS.difference (ex, ignoredSyms))
		val sm = SmlInfoMap.map trim sm
		val bm = StableMap.map trim bm
	    in
	    fun lookupBin i =
		case StableMap.find (bm, i) of
		    SOME pex => pex
		  | NONE => raise Fail "lookupBin"

	    fun lookupSml i = SmlInfoMap.find (sm, i)
	    end

	    local
		val bindings = ref []
	    in
	    fun genBind (lhs, rhs) =
		bindings := P.DEF { lhs = lhs, rhs = rhs } :: !bindings
	    fun allBindings () = rev (!bindings)
	    end

	    fun relname i = let
		val p = toAbsolute (SrcPath.osstring (SmlInfo.sourcepath i))
		val s = OS.Path.mkRelative { path = p, relativeTo = groupdir }
		val { arcs, isAbs, vol } = OS.Path.fromString s
		fun badarc a =
		    a <> OS.Path.currentArc andalso
		    a <> OS.Path.parentArc andalso
		    (a = "." orelse a = ".." orelse Char.contains a #"/")
		fun toUnix [] = "."
		  | toUnix (h :: t) = let
			fun trans a =
			    if a = OS.Path.currentArc then "."
			    else if a = OS.Path.parentArc then ".."
			    else a
		    in
			concat (rev (foldl (fn (a, l) =>
					       trans a :: "/" :: l)
					   [trans h] t))
		    end
	    in
		if isAbs orelse vol <> "" orelse List.exists badarc arcs then
		    (s, true)
		else
		    (toUnix arcs, false)
	    end

	    val gensym = let val next = ref 0
			 in fn prefix =>
			       let val i = !next
			       in prefix ^ Int.toString i before next := i + 1
			       end
			 end

	    val smlmap = ref SmlInfoMap.empty
	    val imports = ref SrcPathMap.empty

	    fun genLIB p =
		case SrcPathMap.find (!imports, p) of
		    SOME v => v
		  | NONE => let val v = gensym "l"
			    in
				imports := SrcPathMap.insert (!imports, p, v);
				v
			    end

	    local
		val symbols = ref SymbolMap.empty
	    in
	    fun genSYM s =
		case SymbolMap.find (!symbols, s) of
		    SOME v => v
		  | NONE => let val (p, ns) =
				    case Symbol.nameSpace s of
					Symbol.SIGspace => ("sgn", P.SGN)
				      | Symbol.STRspace => ("str", P.STR)
				      | Symbol.FCTspace => ("fct", P.FCT)
				      | Symbol.FSIGspace =>
					raise Fail "funsig not permitted in portable graphs"
				      | _ => raise Fail "unexpected namespace"
				val v = gensym p
			    in
				genBind (v, P.SYM (ns, Symbol.name s));
				symbols := SymbolMap.insert (!symbols, s, v);
				v
			    end
	    end

	    local
		val sets = ref SSM.empty
	    in
	    fun genSYMS ss =
		case SSM.find (!sets, ss) of
		    SOME v => v
		  | NONE => let val v = gensym "ss"
				val sl = SS.listItems ss
			    in
				genBind (v, P.SYMS (map genSYM sl));
				sets := SSM.insert (!sets, ss, v);
				v
			    end
	    end

	    local
		val filters = ref FM.empty
		val imps = ref IM.empty
	    in
	    fun preventFilter (e, f) =
		filters := FM.insert (!filters, (e, f), e)

	    fun genFILTER (v, f) = let
		val s = genSYMS f
	    in
		case FM.find (!filters, (v, s)) of
		    SOME e => e
		  | NONE => let val e = gensym "e"
			    in
				genBind (e, P.FILTER { env = v, syms = s });
				filters := FM.insert (!filters, (v, s), e);
				preventFilter (e, s);
				e
			    end
	    end

	    fun genFILTER' (vex as (v, ex), f) = let
		val f' = SS.intersection (ex, f)
	    in
		if SS.equal (ex, f') then vex
		else (genFILTER (v, f'), f')
	    end

	    fun unlayer l = let
		fun loop ([], _, a) = rev a
		  | loop ((h, hss) :: t, ss, a) = let
			val i = SS.intersection (ss, hss)
			val u = SS.union (ss, hss)
			val f = SS.difference (hss, ss)
		    in
			if SS.isEmpty f then loop (t, u, a)
			else if SS.isEmpty i then loop (t, u, h :: a)
			else loop (t, u, genFILTER (h, f) :: a)
		    end
	    in
		loop (l, SS.empty, [])
	    end

	    local
		val merges = ref SLM.empty
	    in
	    fun genMERGE [e] = e
	      | genMERGE l =
		(case SLM.find (!merges, l) of
		     SOME e => e
		   | NONE => let val e = gensym "e"
			     in
				 genBind (e, P.MERGE l);
				 merges := SLM.insert (!merges, l, e);
				 e
			     end)
	    end

	    fun genCOMPILE (v, s, e, ex) = let
		val ss = genSYMS ex
	    in
		preventFilter (v, ss);
		genBind (v, P.COMPILE { src = s, env = e, syms = ss })
	    end

	    fun genIMPORT (lib, ex) =
		if SS.isEmpty ex then ("dummy", ex)
		else
		    let val s = genSYMS ex
		    in case IM.find (!imps, (lib, s)) of
			   SOME v => (v, ex)
			 | NONE =>
			   let val v = gensym "e"
			       val l = genLIB lib
			   in
			       imps := IM.insert (!imps, (lib, s), v);
			       genBind (v, P.IMPORT { lib = l, syms = s });
			       preventFilter (v, s);
			       (v, ex)
			   end
		    end
	    end

	    fun smlImport i =
		case lookupSml i of
		    NONE => NONE
		  | SOME lex => SOME (genIMPORT lex)

	    fun binImport i = genIMPORT (lookupBin i)

	    fun sn (DG.SNODE { smlinfo, localimports, globalimports }) =
		case SmlInfoMap.find (!smlmap, smlinfo) of
		    SOME vex => vex
		  | NONE => let
			val v = gensym "e"
			val ex = case SmlInfo.exports gp smlinfo of
				     SOME ex => ex
				   | NONE => raise Fail "cannot parse SML file"
			val vex = (v, ex)
			val _ =
			    smlmap := SmlInfoMap.insert (!smlmap, smlinfo, vex)
			val gi = map fsbn globalimports
			val li = map sn localimports
			val e = genMERGE (unlayer (li @ gi))
		    in
			genCOMPILE (v, relname smlinfo, e, ex);
			vex
		    end

	    and fsbn (NONE, n) = sbn n
	      | fsbn (SOME f, n) = genFILTER' (sbn n, f)

	    and sbn (DG.SB_SNODE (n as DG.SNODE { smlinfo, ... })) =
		(case smlImport smlinfo of
		     NONE => sn n
		   | SOME vex => vex)
	      | sbn (DG.SB_BNODE (DG.BNODE { bininfo, ... }, _, _)) =
		binImport bininfo

	    fun impexp (th, _, ss) = #1 (genFILTER' (fsbn (th ()), ss))

	    val iel = SymbolMap.foldr (fn (ie, l) => impexp ie :: l) [] exports

	    val export = genMERGE iel
	in
	    (P.GRAPH { imports = SrcPathMap.listItems (!imports),
		       defs = allBindings (),
		       export = export },
	     SrcPathMap.listKeys (!imports))
	end
end
