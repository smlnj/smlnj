(* patch-file.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure PatchFile : sig

  (* given a list of command-line arguments that were supplied to the
   * installer, this function extracts the rewrite specifications,
   * which have the form `-s NAME=value`, and returns a function for
   * patching files by replacing occurrences of `@NAME@` with `value`.
   *)
    val patch : string list -> {src : string, dst : string} -> unit

  end = struct

    structure SS = Substring

    structure Tbl = HashTableFn (
      struct
        type hash_key = substring
        val hashVal = HashString.hashSubstring
        fun sameKey (ss1, ss2) = (SS.compare (ss1, ss2) = EQUAL)
      end)

    type subst = string * string

    fun mkSubst ("-s" :: arg :: rest) = let
	  val ss = SS.full arg
	  val (name, ss) = SS.splitl (fn #"=" => false | _ => true) ss
	  in
	    if (SS.size ss >= 1)
	      then (name, SS.string(SS.triml 1 ss)) :: mkSubst rest
	      else raise Fail "Invalid substitution argument"
	  end
      | mkSubst (_ :: rest) = mkSubst rest
      | mkSubst [] = []

    fun openFiles {src, dst} = let
	  val inS = if OS.FileSys.access(src, [OS.FileSys.A_READ])
		then TextIO.openIn src
		else raise Fail "Source file does not exist or is unreadable"
	  val outS = TextIO.openOut dst
	  in
	    (inS, outS)
	  end

    fun copy files = let
	  val (inS, outS) = openFiles files
	  fun lp () = (case TextIO.input inS
		 of "" => (TextIO.closeIn inS; TextIO.closeOut outS)
		  | s => (TextIO.output(outS, s); lp ())
		(* end case *))
	  in
	    lp()
	  end

    fun rewrite subst = let
          val find = let
                val tbl = Tbl.mkTable (List.length subst, Fail "subst-tbl")
                fun ins (ss1, s2) = Tbl.insert tbl (ss1, s2)
                in
                  Tbl.insert tbl (SS.full "", "@");	(* map "@@" to "@" *)
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
	  fun copy files = let
		val (inS, outS) = openFiles files
		fun lp () = (case TextIO.inputLine inS
		       of NONE => (TextIO.closeIn inS; TextIO.closeOut outS)
			| SOME s => (rewriteLine(outS, s); lp ())
		      (* end case *))
		in
		  lp()
		end
	  in
	    copy
	  end

    fun patch args = (case mkSubst args
	   of [] => copy
	    | subst => rewrite subst
	  (* end case *))

  end
