(* expand-file.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * Copy a template file to an output file while expanding placeholders.
 * Placeholders are denoted by @id@ on a line by themselves.
 *)

structure ExpandFile :> sig

    (* a hook writes the substitution text to the output stream *)
    type hook = TextIO.outstream -> unit

    type template

    val mkTemplateFromFile : string -> template
    val mkTemplateFromList : string list -> template

    val expandTemplate : {
	  src : template,
	  dst : string, (* file name *)
	  hooks : (string * hook) list
        } -> unit

  end = struct

    structure TIO = TextIO
    structure SS = Substring
    structure Tbl = HashTableFn (
      struct
        type hash_key = substring
        val hashVal = HashString.hashSubstring
        fun sameKey (ss1, ss2) = (SS.compare (ss1, ss2) = EQUAL)
      end)

    type hook = TextIO.outstream -> unit
    type template = string

    fun mkTemplateFromFile fname = let
          val file = TIO.openIn fname
	  fun done () = TIO.closeIn file
	  in
            TIO.inputAll file handle ex => (done(); raise ex)
	    before done()
	  end

    fun mkTemplateFromList l = String.concat l

    exception OpenOut

    fun expandTemplate {src, dst, hooks} = let
          (* build the substitution table and return the `find` function *)
          val find = let
                val tbl = Tbl.mkTable (List.length hooks, Fail "subst-tbl")
                fun ins ("", _) = raise Fail "Illegal empty placeholder"
                  | ins (s1, s2) = Tbl.insert tbl (SS.full s1, s2)
                in
                  List.app ins hooks;
                  Tbl.find tbl
                end
	  val dstStrm = TIO.openOut dst
		handle ex => (
		  TIO.output(TIO.stdErr, concat[
		      "Error: unable to open output file \"",
		      dst, "\"\n"
		    ]);
		  raise OpenOut)
          (* scan the substring looking for the next placeholder. *)
          fun scan ss = (case SS.getc ss
                 of SOME(#"@", rest) => let
                      val (hook, rest) = scanPlaceholder rest
                      in
                        hook dstStrm;
                        scan rest
                      end
                  | SOME(c, rest) => (
                      TextIO.output1(dstStrm, c);
                      scan rest)
                  | NONE => TextIO.closeOut dstStrm
                (* end case *))
          and scanPlaceholder start = let
                fun scan (ss, n) = (case SS.getc ss
                       of NONE => raise Fail "incomplete placeholder"
                        | SOME(#"@", rest) => (SS.slice(start, 0, SOME n), rest)
                        | SOME(_, rest) => scan (rest, n+1)
                      (* end case *))
                val (placeholder, rest) = scan (start, 0)
                in
                  if SS.isEmpty placeholder
                    then (fn outS => TextIO.output1(outS, #"@"), rest)
                    else (case find placeholder
                       of SOME hook => (hook, rest)
                        | NONE => raise Fail(concat[
                              "unknown placeholder @", SS.string placeholder, "@"
                            ])
                      (* end case *))
                end
          in
            scan (SS.full src)
          end
	    handle OpenOut => ()

  end
