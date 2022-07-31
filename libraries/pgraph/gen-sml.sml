(* gen-sml.sml
 *
 * Generate SML source code for a given library.
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *
 * author: Matthias Blume (blume@research.bell-labs.com)
 *)
local structure P = PortableGraph in
structure GenSML : sig
    type typ = string
    type varname = string

    exception TypeError of typ * varname
    exception Unbound of varname
    exception ImportMismatch

    val gen : { graph: P.graph,
		nativesrc: string -> string,
		importstructs: string list,
		outstream: TextIO.outstream,
		exportprefix: string,
		use_toplocal: bool } -> unit
end = struct

    type typ = string
    type varname = string

    exception TypeError of typ * varname
    exception Unbound of varname
    exception ImportMismatch

    structure M = RedBlackMapFn (type ord_key = string
				 val compare = String.compare)

    type namespace = string
    type name = string

    type symbol = namespace * name

    fun symbol_compare ((ns, n), (ns', n')) =
	case String.compare (n, n') of
	    EQUAL => String.compare (ns, ns')
	  | unequal => unequal

    structure SS = RedBlackSetFn (type ord_key = symbol
				  val compare = symbol_compare)
    structure SM = RedBlackMapFn (type ord_key = symbol
				  val compare = symbol_compare)

    datatype binding =
	SYM of symbol
      | SYMS of SS.set
      | ENV of symbol SM.map

    fun gen args = let
	val { graph = P.GRAPH { imports, defs, export },
	      nativesrc,
	      importstructs,
	      outstream = outs,
	      exportprefix,
	      use_toplocal } = args

	val (xlocal, xin, xend) =
	    if use_toplocal then ("local", "in", "end")
	    else ("(* local *)", "(* in *)", "(* end *)")

	fun out l = app (fn s => TextIO.output (outs, s)) l

	val im =
	    if length imports = length importstructs then
		let fun add (v, str, m) = M.insert (m, v, str)
		    val m = ListPair.foldl add M.empty (imports, importstructs)
		in
		    fn v => M.find (m, v)
		end
	    else raise ImportMismatch

	val gensym =
	    let val next = ref 0
	    in
		fn () => let
		       val i = !next
		   in
		       next := i + 1;
		       "gs_" ^ Int.toString i
		   end
	    end

	fun genexport (ss, fmt) = let
	    val sl = SS.listItems ss
	    val sl' = map (fn (ns, n) => (ns, gensym ())) sl
	    fun oneline (sy, sy', e) = (fmt (sy, sy'); SM.insert (e, sy, sy'))
	in
	    ListPair.foldl oneline SM.empty (sl, sl')
	end

	fun import (lib, ss) = let
	    val lstruct =
		case im lib of
		    NONE => raise Unbound lib
		  | SOME n => n
	    fun fmt ((ns, n), (_, n')) =
		out [ns, " ", n', " = ", lstruct, n, "\n"]
	in
	    genexport (ss, fmt)
	end

	fun genimport ((ns, n), (_, n')) =
	    out ["    ", ns, " ", n, " = ", n', "\n"]

	fun compile (src, native, e, oss) = let
	    fun fmt ((ns, n), (_, n')) =
		out [ns, " ", n', " = ", n, "\n"]
	    fun copyfile src = let
		val ins = TextIO.openIn (if native then src else nativesrc src)
		fun copy () =
		    case TextIO.input ins of
			"" => TextIO.closeIn ins
		      | s => (out [s]; copy ())
	    in
		copy ()
	    end
	in
	    out [xlocal, "\n"];
	    SM.appi genimport e;
	    out [xin, "\n"];
	    copyfile src;
	    genexport (oss, fmt)
	    before out [xend, "\n"]
	end

	fun filter (e, ss) = SM.filteri (fn (sy, _) => SS.member (ss, sy)) e

	fun get dm v =
	    case M.find (dm, v) of
		NONE => raise Unbound v
	      | SOME d => d

	fun getENV dm v =
	    case get dm v of
		ENV m => m
	      | _ => raise TypeError ("env", v)

	fun namespace P.SGN = "signature"
	  | namespace P.STR = "structure"
	  | namespace P.FCT = "functor"

	fun onedef (P.DEF { lhs, rhs }, dm) = let
	    val get = get dm
	    val getENV = getENV dm

	    fun getSYM v =
		case get v of
		    SYM s => s
		  | _ => raise TypeError ("sym", v)
	    fun getSYMS v =
		case get v of
		    SYMS ss => ss
		  | _ => raise TypeError ("syms", v)
	in
	    M.insert (dm, lhs,
		      case rhs of
			  P.SYM (ns, n) => SYM (namespace ns, n)
			| P.SYMS vl => let
			      fun one (v, ss) = SS.add (ss, getSYM v)
			  in
			      SYMS (foldl one SS.empty vl)
			  end
			| P.IMPORT { lib, syms } =>
			  ENV (import (lib, getSYMS syms))
			| P.COMPILE { src = (src, native), env, syms } =>
			  ENV (compile (src, native, getENV env, getSYMS syms))
			| P.FILTER { env, syms } =>
			  ENV (filter (getENV env, getSYMS syms))
			| P.MERGE el => let
			      fun one (v, e) = SM.unionWith #2 (getENV v, e)
			  in
			      ENV (foldl one SM.empty el)
			  end)
	end

	val _ = out ["local\n"]

	val dm = foldl onedef M.empty defs

	val ee = getENV dm export

	fun libexport ((ns, n), (_, n')) =
	    out [ns, " ", exportprefix, n, " = ", n', "\n"]

    in
	out ["in\n"];
	SM.appi libexport ee;
	out ["end\n"]
    end
end
end
