(* expand-file.sml
 *
 * COPYRIGHT (c) 2016 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Copy a template file to an output file while expanding placeholders.
 * Placeholders are denoted by @id@ on a line by themselves.
 *)

structure ExpandFile :> sig

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
    structure RE = RegExpFn (
      structure P = AwkSyntax
      structure E = BackTrackEngine)
    structure M = MatchTree

    type hook = TextIO.outstream -> unit
    type template = string list

    fun mkTemplateFromFile fname = let
          val file = TIO.openIn fname
	  fun done () = TIO.closeIn file
	  fun read () = (case TIO.inputLine file
		 of NONE => []
		  | SOME line => line::read()
		(* end case *))
	  in
            read() handle ex => (done(); raise ex)
	    before done()
	  end

    fun mkTemplateFromList l = l

    val placeholderRE = RE.compileString "[\\t ]*@([a-zA-Z][-a-zA-Z0-9_]*)@[\\t ]*"
    val prefixPlaceholder = RE.prefix placeholderRE SS.getc

    fun findPlaceholder s = (case prefixPlaceholder(SS.full s)
	   of SOME(M.Match(_, [M.Match({pos, len}, _)]), _) =>
		SOME(SS.string(SS.slice(pos, 0, SOME len)))
	    | _ => NONE
	  (* end case *))

  (* copy from inStrm to outStrm expanding placeholders *)
    fun copy (inStrm, outStrm, hooks) = let
	  fun lp [] = ()
	    | lp (s::ss) = (
	        case findPlaceholder s
		 of NONE => TIO.output (outStrm, s)
		  | (SOME id) => (
		      case (List.find (fn (id', h) => id = id') hooks)
		       of (SOME(_, h)) => h outStrm
			| NONE => raise Fail(concat["bogus placeholder '", id, "'"])
		      (* end case *))
	        (* end case *);
		lp(ss))
	  in
	    lp(inStrm)
	  end

    exception OpenOut

    fun expandTemplate {src, dst, hooks} = (let
	  val dstStrm = TIO.openOut dst
		handle ex => (
		  TIO.output(TIO.stdErr, concat[
		      "Error: unable to open output file \"",
		      dst, "\"\n"
		    ]);
		  raise OpenOut)
	  fun done () = (TIO.closeOut dstStrm)
	  in
	    copy (src, dstStrm, hooks) handle ex => (done(); raise ex);
	    done()
	  end
	    handle OpenOut => ())

  end
