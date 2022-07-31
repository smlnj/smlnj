(* reconstruct.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *
 * author: Matthias Blume (blume@research.bell-labs.com)
 *)
local structure P = PortableGraph in
structure ReconstructPortable :> sig

    type lib type env type sym type syms type misc
    type graph = P.graph
    type context = (lib, env, sym, syms, graph, misc) PGOps.context

    val reconstruct : (context -> lib list -> graph) * int -> graph
end = struct

    type lib = P.varname
    type env = P.varname
    type sym = P.varname
    type syms = P.varname
    type misc = int * P.def list
    type graph = P.graph
    type context = (lib, env, sym, syms, graph, misc) PGOps.context

    fun reconstruct (gt, nlibs) = let

	fun varname i = "v" ^ Int.toString i

	fun Bind (r, (i, d)) = let
	    val v = varname i
	    val i' = i + 1
	    val d' = P.DEF { lhs = v, rhs = r } :: d
	in
	    ((i', d'), v)
	end

	fun Sgn m s =
	    Bind (P.SYM (P.SGN, s), m)
	fun Str m s =
	    Bind (P.SYM (P.STR, s), m)
	fun Fct m s =
	    Bind (P.SYM (P.FCT, s), m)
	fun Syms m sl =
	    Bind (P.SYMS sl, m)
	fun Imp m (l, ss) =
	    Bind (P.IMPORT { lib = l, syms = ss }, m)
	fun Com m (s, e, ss, n) =
	    Bind (P.COMPILE { src = (s, n), env = e, syms = ss }, m)
	fun Fil m (e, ss) =
	    Bind (P.FILTER { env = e, syms = ss }, m)
	fun Mer m el =
	    Bind (P.MERGE el, m)

	val imports = List.tabulate (nlibs, varname)

	fun Exp (i, d) e =
	    P.GRAPH { imports = imports, defs = rev d, export = e }
    in
	gt { Ops = { Sgn = Sgn, Str = Str, Fct = Fct,
		     Exp = Exp, Syms = Syms,
		     Imp = Imp, Com = Com, Fil = Fil, Mer = Mer },
	     Misc = (nlibs, []) }
	   imports
    end
end
end
