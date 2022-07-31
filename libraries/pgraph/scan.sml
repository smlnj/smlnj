(* scan.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *
 * Read the output of format.sml and reconstruct the original
 * PortableGraph.graph.
 *
 * author: Matthias Blume (blume@research.bell-labs.com)
 *)
structure ScanPortable : sig
    exception ParseError of string
    val input : TextIO.instream -> PortableGraph.graph
end = struct

    structure P = PortableGraph
    structure S = TextIO.StreamIO

    exception ParseError of string

    fun input ins = let
        val s = TextIO.getInstream ins

	fun skipLine s = getOpt (Option.map #2 (S.inputLine s), s)

        fun allof l s = foldl (fn (f, s) => f s) s l

        fun skipWS s =
            case S.input1 s of
                NONE => s
              | SOME (c, s') => if Char.isSpace c then skipWS s' else s

        fun maybeident s = let
            val s = skipWS s
            val finish = String.implode o rev
            fun loop (s, a) =
                case S.input1 s of
                    NONE => SOME (finish a, s)
                  | SOME (c, s') => if Char.isAlphaNum c then loop (s', c :: a)
                                    else SOME (finish a, s)
        in
            case S.input1 s of
                NONE => NONE
              | SOME (c, s') => if Char.isAlpha c then loop (s', [c])
                                else NONE
        end

        fun ident s =
            case maybeident s of
                NONE => raise ParseError "expected: identifier"
              | SOME (i, s') => (i, s')

        fun maybestring s = let
            val s = skipWS s
            fun eof () = raise ParseError "unexpected EOF in string"
            fun loop (s, a) =
                case S.input1 s of
                    NONE => eof ()
                  | SOME (#"\"", s') =>
                    (case String.fromString (String.implode (rev a)) of
                         SOME x => SOME (x, s')
                       | NONE => raise ParseError "illegal string syntax")
                  | SOME (#"\\", s') =>
                    (case S.input1 s' of
                         NONE => eof ()
                       | SOME (c, s'') => loop (s'', c :: #"\\" :: a))
                  | SOME (c, s') => loop (s', c :: a)
        in
            case S.input1 s of
                SOME (#"\"", s') => loop (s', [])
              | _ => raise ParseError "expected: string"
        end

        fun string s =
            case maybestring s of
                NONE => raise ParseError "expected: string"
              | SOME (x, s') => (x, s')

        fun expect c s = let
            val s = skipWS s
            fun notc what =
                raise ParseError (concat ["expected: ", Char.toString c,
                                          ", found: ", what])
        in
            case S.input1 s of
                NONE => notc "EOF"
              | SOME (c', s') => if c = c' then s' else notc (Char.toString c')
        end

        fun expectId i s = let
            val (i', s') = ident s
        in
            if i = i' then s'
            else raise ParseError (concat ["expected: ", i, ", found: ", i'])
        end

        fun varlist s = let
            fun eof () = raise ParseError "unexpected EOF in varlist"
            val s = allof [expect #"[", skipWS] s
            fun rest s = let
                val s = skipWS s
            in
                case S.input1 s of
                    NONE => eof ()
                  | SOME (#"]", s') => ([], s')
                  | SOME (#",", s') => let
                        val (h, s'') = ident s'
                        val (t, s''') = rest s''
                    in
                        (h :: t, s''')
                    end
                  | SOME (c, _) =>
                    raise ParseError
                              (concat ["expected , or ], found: ",
                                       Char.toString c])
            end
        in
            case S.input1 s of
                NONE => eof ()
              | SOME (#"]", s') => ([], s')
              | SOME _ => let
                    val (h, s') = ident s
                    val (t, s'') = rest s'
                in
                    (h :: t, s'')
                end
        end

        fun def s =
            case maybeident s of
                SOME ("val", s) =>
                let val s = allof [expect #"(", expectId "c", expect #","] s
                    val (lhs, s) = ident s
                    val s = allof [expect #")", expect #"="] s
                    val (f, s) = ident s
                    val s = expectId "c" s
                    fun def (rhs, s) =
                        SOME (P.DEF { lhs = lhs, rhs = rhs }, s)
                    fun comp native = let
                        val (r, s) = string s
                        val (e, s) = ident s
                        val (ss, s) = ident s
                    in
                        def (P.COMPILE { src = (r, native),
					 env = e, syms = ss },
			     s)
                    end
		    fun sym ns = let
                        val (n, s) = string s
                    in
                        def (P.SYM (ns, n), s)
                    end
                in
                    case f of
                        "syms" => let
                            val (l, s) = varlist s
                        in
                            def (P.SYMS l, s)
                        end
                      | "import" => let
                            val (l, s) = ident s
                            val (ss, s) = ident s
                        in
                            def (P.IMPORT { lib = l, syms = ss }, s)
                        end
                      | "compile" =>  comp false
                      | "ncompile" => comp true
                      | "merge" => let
                            val (l, s) = varlist s
                        in
                            def (P.MERGE l, s)
                        end
                      | "filter" => let
                            val (e, s) = ident s
                            val (ss, s) = ident s
                        in
                            def (P.FILTER { env = e, syms = ss }, s)
                        end
		      | "sgn" => sym P.SGN
		      | "str" => sym P.STR
		      | "fct" => sym P.FCT
                      | x => raise ParseError ("unknown function: " ^ x)
                end
              | _ => NONE

        fun deflist s = let
            fun loop (s, a) =
                case def s of
                    SOME (d, s') => loop (s', d :: a)
                  | NONE => (rev a, s)
        in
            loop (s, [])
        end

        fun graph s = let
            val s = allof [skipLine, expectId "fn"] s
            val (imports, s) = varlist s
            val s = allof [expect #"=", expect #">", expectId "let",
			   expectId "open", expectId "PGOps"] s
            val (defs, s) = deflist s
            val s = allof [expectId "in", expectId "export", expectId "c"] s
              val (export, s) = ident s
            (* gobble up remaining boilerplate... *)
            val s = allof [expectId "end", expect #"|", expect #"_",
                           expect #"=", expect #">", expectId "raise",
                           expectId "Fail", #2 o string, expect #")",
			   skipLine]
                          s
        in
            TextIO.setInstream (ins, s);
            P.GRAPH { imports = imports, defs = defs, export = export }
        end
    in
        graph s
    end
end
