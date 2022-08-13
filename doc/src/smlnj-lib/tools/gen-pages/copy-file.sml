(* copy-file.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure CopyFile : sig

  (* `copy subst (srcFile, outS)` copys the contents of `srcFile` to the output
   * stream while replacing strings of the form `@name@` with `value` where
   * `(name, value)` occurs in the substitution list `subst`.
   *)
    val copy : (string * string) list -> string * TextIO.outstream -> unit

  end = struct

    structure SS = Substring

    structure Tbl = HashTableFn (
      struct
        type hash_key = substring
        val hashVal = HashString.hashSubstring
        fun sameKey (ss1, ss2) = (SS.compare (ss1, ss2) = EQUAL)
      end)

    fun copy subst = let
          val find = let
                val tbl = Tbl.mkTable (List.length subst, Fail "subst-tbl")
                fun ins (ss1, s2) = Tbl.insert tbl (SS.full ss1, s2)
                in
                  ins ("", "@");	(* map "@@" to "@" *)
                  List.app ins subst;
                  Tbl.find tbl
                end
	  fun rewriteLine (outS, ln) = let
		fun scanLine ss = let
		      val (prefix, rest) = SS.splitl (fn #"@" => false | _ => true) ss
		      in
			TextIO.outputSubstr (outS, prefix);
			if (SS.size rest = 0)
			  then ()
			  else scanPlaceholder (SS.triml 1 rest)
		      end
	      (* scan a `@NAME@` placeholder starting from the first character of `NAME` *)
		and scanPlaceholder start = let
		      fun scan (ss, n) = (case SS.getc ss
			     of NONE => raise Fail "Incomplete placeholder"
			      | SOME(#"@", rest) => (SS.slice(start, 0, SOME n), rest)
			      | SOME(_, rest) => scan (rest, n+1)
			    (* end case *))
		      val (placeholder, rest) = scan (start, 0)
		      in
			case find placeholder
			 of SOME expansion => (
			      TextIO.output(outS, expansion);
			      scanLine rest)
			  | NONE => raise Fail(concat[
				"unknown placeholder @", SS.string placeholder, "@"
			      ])
			(* end case *)
		      end
		in
		  scanLine (SS.full ln)
		end
	  fun rewrite (srcFile, outS) = let
		val inS = TextIO.openIn srcFile
		fun lp () = (case TextIO.inputLine inS
		       of NONE => TextIO.closeIn inS
			| SOME s => (rewriteLine(outS, s); lp ())
		      (* end case *))
		in
		  lp()
		end
	  in
	    rewrite
	  end

  end
