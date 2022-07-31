(* syntax.sml
 *
 * COPYRIGHT (c) 2008 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This is the abstract syntax tree used to represent regular expressions.
 * It serves as the glue between different front-ends (implementing
 * different RE specification languages), and different back-ends (implementing
 * different compilation/searching algorithms).
 *)

structure RegExpSyntax : REGEXP_SYNTAX =
  struct

    exception CannotCompile

    structure CharSet = ListSetFn (
       struct type ord_key = char
       val compare = Char.compare end)

    datatype syntax
      = Group of syntax
      | Alt of syntax list
      | Concat of syntax list
      | Interval of (syntax * int * int option)
      | MatchSet of CharSet.set
      | NonmatchSet of CharSet.set
      | Char of char
      | Begin
      | End

    fun optional re = Interval(re, 0, SOME 1)
    fun closure re = Interval(re, 0, NONE)
    fun posClosure re = Interval(re, 1, NONE)

    fun addRange (s, minC ,maxC) = let
	  val fst = ord minC
	  val lst = ord maxC
	  in
	    CharSet.addList (s, List.tabulate (lst - fst + 1, fn v => chr(v + fst)))
	  end
    fun fromRange (minC, maxC) = addRange (CharSet.empty, minC, maxC)

    val allChars = fromRange (Char.minChar, Char.maxChar)

    val digit = fromRange (#"0", #"9")
    val lower = fromRange (#"a", #"z")
    val upper = fromRange (#"A", #"Z")
    val alpha = CharSet.union(lower, upper)
    val alnum = CharSet.union(alpha, digit)
    val ascii = fromRange (#"\000", #"\127")
    val blank = CharSet.fromList [#" ", #"\t"]
    val cntl = addRange(CharSet.singleton #"\127", #"\000", #"\031")
    val graph = fromRange (#"\033", #"\126")
    val print = CharSet.add (graph, #" ")
    val punct = CharSet.fromList [
	    #"]", #"[", #"!", #"\"", #"#", #"$", #"%", #"&",
	    #"'", #"(", #")", #"*", #"+", #",", #".", #"/",
	    #":", #";", #"<", #"=", #">", #"?", #"@", #"\\",
	    #"^", #"_", #"`", #"{", #"|", #"}", #"~", #"-"
	  ]
    val space = CharSet.fromList [#"\t", #"\r", #"\n", #"\v", #"\f", #" "]
    val word = CharSet.add(alnum, #"_")
    val xdigit = addRange (addRange (digit, #"a", #"f"), #"A", #"F")

  end
