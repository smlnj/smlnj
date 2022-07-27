(* token.sml
 *
 * COPYRIGHT (c) 2006
 * All rights reserved.
 *
 * LaTeX output.
 * Contributed by Adam Shaw.
 *)

(* 
signature BACK_END =
  sig
    val output : (LLKSpec.grammar * TextIO.outstream * string)
                   -> unit
    end
*)

structure LaTeXOutput (* : BACK_END *) =
  struct

    structure S = LLKSpec

    (* containsChar : (char list) -> char -> bool *)
    fun containsChar (cs : char list) (x : char) = 
          (case cs
            of [] => false
	     | (ch :: chs) => (ch = x) orelse (containsChar chs x)
	   (* end case *))

    (* removeChar : (char list) -> char -> (char list) *)
    fun removeChar (cs : char list) (x : char) =
          (case cs
	    of [] => []
	     | (ch :: chs) => if (ch = x) then chs else (removeChar chs x)
           (* end case *))

    (* escape : char -> string -> string *)
    fun escape c = let
          fun backslash [] = []
            | backslash (x::xs) = 
                if x = c 
		then #"\\" :: x :: (backslash xs)
		else x :: (backslash xs)
          in
            (implode o backslash o explode)
          end

    (* backslashFirst : (char list) -> (char list) *)
    (* put backslash first if present, otherwise do nothing *)
    fun backslashFirst cs = 
          if containsChar cs #"\\"
	  then #"\\" :: (removeChar cs #"\\")
	  else cs 

    (* escapeL : (char list) -> string -> string *)
    (* escape the characters in cs by prepending backslashes *)
    (* escape backslash first to avoid double-escaping *)
    fun escapeL cs s = let
          val cs' = backslashFirst cs
          fun escl ([], s) = s
	    | escl (ch :: chs, s) = escl (chs, escape ch s)
          in
            escl (cs', s)
          end

    fun curry f = (fn a => fn b => f (a, b))
    fun flip f = (fn (a, b) => f (b, a))
    val drop = List.drop
    fun clipby n = implode o rev o (((curry o flip) drop) n) o rev o explode

    fun wrap d s = concat [d, s, d]

    fun maybewrap (d, t) = fn s => if s = t then wrap d t else s

    fun mathif t = maybewrap ("$", t)

    fun stripquotes s =
          if String.isPrefix "\"" s
          then stripquotes (String.extract (s, 1, NONE))
          else if String.isSuffix "\"" s
               then stripquotes (clipby 1 s)
               else s

    (* mathescape : string -> string -> string *)
    (* to surround all occurences of s with $ . $ *)
    fun mathescape s s' =
          if (size s) > (size s')
          then s'
          else if String.isPrefix s s'
               then concat [wrap "$" s, 
			    mathescape s (String.extract (s', String.size s, NONE))]
               else concat [String.substring (s', 0, 1), 
			    mathescape s (String.extract (s', 1, NONE))]

    (* mathescapeL : (string list) -> string -> string *)
    fun mathescapeL ss s' =
        (case ss
          of [] => s'
           | (st :: sts) => mathescapeL sts (mathescape st s')
	 (* end case *))

    (* isID: string -> bool *)
    (* returns true if a string starts with _ or an alphabetic character *)
    (* TODO implement this less cheesily *)
    fun isID s = let
          fun f [] = false
	    | f (p :: ps) = (String.isPrefix (Char.toString p) s) 
                            orelse (f ps)
          in 
            f (explode "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
          end

    fun ttif s s' = if s = s' then concat ["{\tt ", s', "}"] else s'

    val cat  = String.concat
    val catw = String.concatWith

    fun prods nt = let
          val (S.NT {prods, ...}) = nt
          in
	    !prods
          end

    val lhs = Prod.lhs
    val rhs = Prod.items

    val nt2s = Nonterm.toString
    val i2s  = Item.toString

    val spacer = " $\\;$ "
    val spacedPipe = " $\\;|\\;$ "

    fun tt s = concat ["{\\tt ", s, "}"]
    fun bf s = concat ["{\\bf ", s, "}"]

    (* some grammar.sty functions *)
    structure GrammarSty = struct
      fun tag t = (fn s => concat ["\\", t, "{", s, "}"])
      val RHS        = tag "RHS"
      val GRP        = tag "GRP"
      val OPT        = tag "OPT"
      val OPTGRP     = tag "OPTGRP"
      val LIST       = tag "LIST"
      val LISTGRP    = tag "LISTGRP"
      val LISTONE    = tag "LISTONE"
      val LISTONEGRP = tag "LISTONEGRP"
      val ITEM       = tag "ITEM"
      val kw         = tag "kw"
      val nt         = tag "nt"
      val term       = tag "term"
      val sym        = tag "sym"
      fun Grammar s = concat ["\\begin{Grammar}\n", s, "\n\n\\end{Grammar}\n"]
      fun Rules (nt, rhss) = concat (["\n\\begin{Rules}{", nt, "}\n"] @
				     [catw "\n" rhss] @ 
				     ["\n\\end{Rules}"])
    end

    structure G = GrammarSty

    fun prod isSubrule = (if isSubrule then (fn x => x) else G.RHS) o 
			 (catw " ") o (map (item isSubrule)) o rhs

    and nonterm isSubrule nt = let
          val ps = prods nt
          in
            if isSubrule 
	    then catw spacedPipe (map (prod true) ps)
	    else let val nt' = nt2s nt
                     in G.Rules (nt', map (prod isSubrule) ps)
                     end
          end

    and tok t = let
          val abbrev = Token.toString t
	  val scrub = (escapeL (explode "_{}")) o
		      (mathescapeL ["<", ">", "-", "|"]) o
		      (stripquotes)
          in
            if isID abbrev
	    then (G.kw  o (* G.ITEM o *) scrub) abbrev
	    else (G.sym o (* G.ITEM o *) scrub) abbrev
          end

    and item isSubrule i = 
          (case Item.sym i
	    of S.TOK t => tok t
	     | S.NONTERM (nt, _) => if Nonterm.isSubrule nt
				    then (G.nt o G.GRP o (nonterm true)) nt
				    else (G.nt o i2s) i
	     | S.CLOS nt => (G.LISTGRP o (nonterm true)) nt
	     | S.POSCLOS nt => (G.LISTONEGRP o (nonterm true)) nt
	     | S.OPT nt => (G.OPTGRP o (nonterm true)) nt
          (* end case *))

    fun nonterms [] = "the null grammar" 
      | nonterms ns = G.Grammar (catw "\n" (map (nonterm false) ns))
            
    (* output grammar *)
    fun grammarHook spec strm = let
	  val (S.Grammar {sortedTops, ...}, _) = spec
          val nts = List.concat sortedTops
	  val g = nonterms nts
          in
            TextIO.output (strm, g)
          end

    fun output (grm, pm, fname) = (print (" writing " ^ fname ^ ".tex\n");
          ExpandFile.expandTemplate {
	      src = LaTeXTemplate.template,
	      dst = fname ^ ".tex",
	      hooks = [("grammar", grammarHook (grm, pm))]
	    })

  end
