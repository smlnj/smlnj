(* string-subst.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure StringSubst : sig

  (* `expand substitutions text`
   * expand the string `text` by replacing placeholders with their expansion as
   * specified in the list of id-value pairs `substitutions`.  Placeholders in `text`
   * have the syntax @<id>@ and are replaced with the string associated with <id>
   * in the list `substitutions`.  If <id> is empty, then no substitution
   * is applied, instead the "@@" is replaced by "@".
   *)
    val expand : (string * string) list -> string -> string

  end = struct

    structure SS = Substring
    structure Tbl = HashTableFn (
      struct
        type hash_key = substring
        val hashVal = HashString.hashSubstring
        fun sameKey (ss1, ss2) = (SS.compare (ss1, ss2) = EQUAL)
      end)

    fun expand subs = let
          val find = let
                val tbl = Tbl.mkTable (List.length subs, Fail "subst-tbl")
                fun ins ("", _) = raise Fail "Illegal empty placeholder"
                  | ins (s1, s2) = Tbl.insert tbl (SS.full s1, s2)
                in
                  Tbl.insert tbl (SS.full "", "@");
                  List.app ins subs;
                  Tbl.find tbl
                end
          fun scan (start, ss, n, frags) = (case SS.getc ss
                 of SOME(#"@", rest) => let
                      val frags = SS.slice(start, 0, SOME n) :: frags
                      val (expansion, rest) = scanPlaceholder rest
                      in
                        scan (rest, rest, 0, expansion::frags)
                      end
                  | SOME(_, rest) => scan (start, rest, n+1, frags)
                  | NONE => SS.concat(List.rev(start::frags))
                (* end case *))
          and scanPlaceholder start = let
                fun scan (ss, n) = (case SS.getc ss
                       of NONE => raise Fail "incomplete placeholder"
                        | SOME(#"@", rest) => (SS.slice(start, 0, SOME n), rest)
                        | SOME(_, rest) => scan (rest, n+1)
                      (* end case *))
                val (placeholder, rest) = scan (start, 0)
                in
                  case find placeholder
                   of SOME expansion => (SS.full expansion, rest)
                    | NONE => raise Fail(concat[
                          "unknown placeholder @", SS.string placeholder, "@"
                        ])
                  (* end case *)
                end
          fun expandStr s = let
                val ss = SS.full s
                in
                  scan (ss, ss, 0, [])
                end
          in
            expandStr
          end

  end
