(* format.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *
 * Format the list-of-edges dependency graph so it becomes a valid
 * ML program.
 *
 * author: Matthias Blume (blume@research.bell-labs.com)
 *)
structure FormatPortable : sig
    val output : PortableGraph.graph * TextIO.outstream -> unit
end = struct
    structure P = PortableGraph

    fun output (P.GRAPH { imports, defs, export }, outs) = let
        val context = "c"
        fun out l = app (fn x => TextIO.output (outs, x)) l

        fun varlist [] = "[]"
          | varlist [x] = concat ["[", x, "]"]
          | varlist (h :: t) =
            concat ("[" :: h :: foldr (fn (x, a) => ", " :: x :: a) ["]"] t)

        fun cfc (front, args) =
            (out [front];
             app (fn x => out [" ", x]) (context :: args))

        fun tos s = concat ["\"", String.toString s, "\""]

	fun tons P.SGN = "sgn"
	  | tons P.STR = "str"
	  | tons P.FCT = "fct"

        fun rhs (P.SYM (ns, n)) = cfc (tons ns, [tos n])
          | rhs (P.SYMS syms) = cfc ("syms", [varlist syms])
          | rhs (P.IMPORT { lib, syms }) = cfc ("import", [lib, syms])
          | rhs (P.COMPILE { src = (src, native), env, syms }) =
            cfc (if native then "ncompile" else "compile",
                 [tos src, env, syms])
          | rhs (P.FILTER { env, syms }) = cfc ("filter", [env, syms])
          | rhs (P.MERGE l) = cfc ("merge", [varlist l])

        fun dodef (P.DEF d) =
            (out ["       val (", context, ", ", #lhs d, ") = "];
	     rhs (#rhs d);
	     out ["\n"])
    in
        out ["val thelibrary = fn ", context, " => (\n"];
        out ["fn ", varlist imports, " => let open PGOps\n"];
        app dodef defs;
        out ["   in\n       export ", context, " ", export,
             "\n   end\n\
             \ | _ => raise Fail \"wrong number of input libraries\")\n"]
    end
end
