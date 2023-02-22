(* compiler-pp.sig
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * A specialization and extension of the PrettyPrint structure for the
 * SML/NJ compiler
 *)

structure CompilerPP :> COMPILER_PP =
  struct

    structure F = Format
    structure PP = PrettyPrint
    structure Dev = TextIODev

    type format = PP.format

    datatype alignment = datatype PP.alignment
    datatype break = datatype PP.break
    datatype element = datatype PP.element

    (* Basic formats and format building operations: *)

    (*** the basic block building functions ***)
    val basicBlock = PP.block
    val alignedBlock = PP.alignedBlock

    (*** block building functions for non-indenting blocks ***)

    (* block : element list -> format *)
    (* construct a block with explicit, possibly heterogeous, breaks. A synonym for basicBlock *)
    val block = basicBlock

    (* constructing aligned blocks: common abbreviations *)
    (* xblock : format list -> format, for x = h, v, p, c *)
    val hblock = PP.hBlock
    val vblock = PP.vBlock
    val pblock = PP.pBlock
    val cblock = pp.CBlock

    (* "conditional" formats *)
    val tryFlat : format -> format = PP.tryFlat
    val alt : format * format -> format = PP.alt
    val hvblock : format list -> format = PP.hvBlock

    (*** format-building utility functions for some primitive SML types ***)
    val empty   : format = PP.empty
    val text    : string -> format = PP.text
    val integer : int -> format = PP.lift Int.toString
    val string  : string -> format =
          PP.lift (fn s => String.concat["\"", String.toString s, "\""])
    val char    : char -> format  =
          PP.lift (fn c => String.concat["#\"", Char.toString c, "\""])
    val bool    : bool -> format = lift Bool.toString

    (*** "punctuation" characters and related symbols ***)
    val comma : format     = text ","
    val colon : format     = text ":"
    val semicolon : format = text ";"
    val period : format    = text "."
    val lparen : format    = text "("
    val rparen : format    = text ")"
    val lbracket : format  = text "["
    val rbracket : format  = text "]"
    val lbrace : format    = text "{"
    val rbrace : format    = text "}"
    val equal  : format    = text "="

    (*** xcat: binary versions of xblock functions (x = p, h, v) ***)

    (* pcat : format * format -> format
     * separate r, l with soft line break *)
    fun pcat (leftFmt : format, rightFmt : format) = PP.pBlock [leftFmt, rightFmt]

    (* hcat : format * format -> format
     * separate r, l with a space *)
    fun hcat (leftFmt : format, rightFmt : format) = PP.hBlock [leftFmt, rightFmt]

    (* vcat : format * format -> format
     * separate r, l with a hard line break *)
    fun vcat (leftFmt : format, rightFmt : format) = PP.vBlock [leftFmt, rightFmt]

    (* ccat : format * format -> format
     * concatenate left and right formats with no separater *)
    fun ccat (leftFmt : format, rightFmt : format) = PP.cBlock [leftFmt, rightFmt]


    (*** wrapping or closing formats, e.g. parenthesizing a format ***)

    (* enclose : {front : format, back : format} -> format -> format *)
    (* tight -- no space between front, back, and fmt *)
    fun enclose {front: format, back: format} = PP.enclose {left=front, right=back}

    (* parens : format -> format *)
    val parens = enclose {front = lparen, back = rparen}

    (* brackets : format -> format *)
    val brackets = enclose {front = lbracket, back = rbracket}

    (* braces : format -> format *)
    val braces = enclose {front = lbrace, back = rbrace}

    (* appendNewLine : format -> format *)
    fun appendNewLine (fmt : format) = block [FMT fmt, BRK HardLine]

    (* label : string -> format -> format *)
    (* labeled formats, i.e. formats preceded by a string label, a commonly occurring pattern *)
    fun label (str:string) (fmt: format) = hcat (text str, fmt)


    (*** functions for formatting sequences of formats (format lists) ***)

    (* sequence : alignement -> format -> format list -> format
     *  The second argument (sep: format) is normally a symbol (TEXT) such as comma or semicolon *)
    fun sequence (alignment: alignment) (sep: format) =
          PP.sequence {align = alignment, sep = sep}

    (* xsequence : [sep:]format -> format list -> format, x = h, v, p, c *)
    val hsequence = sequence H
    val psequence = sequence P
    val vsequence = sequence V
    val csequence = sequence C

    (* tupleFormats : format list -> format  -- parenthesized, comma separated, packed alignment sequence
     *  not really restricted to actual "tuples", just "tuple-style" formatting. Constituent formats can represent
     *  values of heterogeneous types. *)
    fun tupleFormats formats = parens (psequence comma formats)

    (* listFormats : format list -> format  -- packed alignment
     *  typically used for lists, but the constituent formats can represent values of heterogeneous types. *)
    fun listFormats formats = brackets (psequence comma formats)

    fun optionFormat (formatOp: format option) =
        case formatOp
          of NONE => text "NONE"
           | SOME fmt => ccat (text "SOME", parens fmt)


    (*** functions for formatting sequences of values (of homogeneous types, i.e. 'a lists) ***)

    (* formatSeq : {alignment: alignment, sep: format, formatter : 'a -> format} -> 'a list -> format *)
    fun 'a formatSeq {alignment: alignment, sep: format, formatter: 'a -> format} =
          PP.sequenceWithMap {align=alignment, sep=sep, fmt=formatter}

    (* formatClosedSeq :
         {alignment: alignment, front: format, sep: format, back: format, formatter: 'a -> format}
         -> 'a list
         -> format *)
    fun 'a formatClosedSeq
           {alignment: alignment, front: format, sep: format, back: format, formatter: 'a -> format} =
          PP.closedSequenceWithMap {
              align=alignment, left=front, sep=sep, back=right, fmt=formatter
            }

    (* alignedList : alignment -> ('a -> format) -> 'a list -> format *)
    fun 'a alignedList alignment (formatter : 'a -> format) (xs: 'a list) =
        formatClosedSeq
          {alignment=alignment, front = lbracket, back = rbracket, sep = comma, formatter = formatter}
          xs

    (* list : ('a -> format) -> 'a list -> format *)
    (* packed-style formatting of an 'a list *)
    fun 'a list (formatter : 'a -> format) (xs: 'a list) =
        formatClosedSeq
          {alignment=P, front = lbracket, back = rbracket, sep = comma, formatter = formatter}
          xs

    (* option : ('a -> format) -> 'a option -> format *)
    fun 'a option (formatter: 'a -> format) (xOp: 'a option) =
        case xOp
          of NONE => text "NONE"
           | SOME x => ccat (text "SOME", parens (formatter x))


(*** vertical formatting with headers ***)

(* header1 and header2 are header strings for the first item and subsequent items,
 * respectively. We vertically align the element formats, taking account of the possibly
 * that the two header strings have different sizes by left-padding the shorter header
 * string, using the padHeaders function, to equalize the sizes of the headers. *)

(* padleft : string * string -> string * string *)
(* pad the shorter string with spaces on the left to make them the same size.
 * ASSERT: (s1', s2') = pad (s1, s2) => size s1' = size s2' = max (size s1, size s2). *)
fun padHeaders (s1, s2) =
    let val maxsize = Int.max (size s1, size s2)
     in (StringCvt.padLeft #" " maxsize s1,
	 StringCvt.padLeft #" " maxsize s2)
    end

(* vHeaders : {header1 : string, header2 : string, formatter: 'a -> format} -> 'a list -> format *)
fun vHeaders {header1: string, header2: string, formatter: 'a -> format}
		    (elems: 'a list) =
    let val (header1, header2) = padHeaders (header1, header2)
     in case elems
	  of nil => empty
	   | elem :: rest =>
	       vblock
		 (hcat (text header1, formatter elem) ::
		  map (fn e => hcat (text header2, formatter e)) rest)
    end

(* vHeaderFormats : {header1 : string, header2 : string} -> format list -> format *)
fun vHeaderFormats {header1: string, header2: string} (elems: format list) =
    let val (header1, header2) = padHeaders (header1, header2)
     in case elems
	  of nil => empty
	   | elem :: rest =>
	       vblock
		 (hcat (text header1, elem) ::
		  map (fn fmt => hcat (text header2, fmt)) rest)
    end


(*** "indenting" formats ***)

(* When applied to EMPTY, produces EMPTY
 * The resulting format soft-indents n spaces (iff following a line break) *)
val indent : int -> format -> format = PP.indent

(*** functions for setting and accessing the line width ***)

(* NOTE: setLineWidthFun does not have an effect until actuall pretty printing occurs,
 *   so it does not make sense to call it within the modules defining particular pretty printers!
*)

local
  val defaultLocalLineWidth = 90
  val lineWidthRef : int ref = ref defaultLocalLineWidth
  fun getLocalLineWidth () : int = !lineWidthRef
  fun setLocalLineWidth (width: int) = lineWidthRef := width
  val lineWidthFunRef : (unit -> int) ref = ref (getLocalLineWidth)
  fun getLineWidthFun () : (unit -> int) = !lineWidthFunRef
in
  fun setLineWidthFun (lwf: unit -> int) =  lineWidthFunRef := lwf
  fun resetLineWidthFun () = lineWidthFunRef := getLocalLineWidth
  fun getLineWidth () = getLineWidthFun () ()
end


(*** printing (i.e., rendering) formats ***)

(* render : format * (string -> unit) * int -> unit *)
val render = R.render

(* printFormatLW : int -> format -> unit *)
fun printFormatLW lw format = R.render (appendNewLine format, print, lw)

(* printFormatLW' : format -> int -> unit *)
fun printFormatLW' format lw = R.render (appendNewLine format, print, lw)

(* printFormat : format -> unit *)
fun printFormat format = R.render (format, print, getLineWidth ())

(* printFormatNL : format -> unit *)
fun printFormatNL format = R.render (appendNewLine format, print, getLineWidth ())

end (* structure PrettyPrint *)

(* NOTES:

1. We have sequence formating funtions that act on lists of arbitrary values (of a given type),
   with a supplied formatting function for the element type, and other functions that act on lists of formats.

   The first sort can easily be simulated by translating the value list into a format list
   by mapping the formatter over the values.  This seems to be preferabel, so the former sequencing
   functions (formatSeq, formatClosedSeq, tuple, list, alignedList) can be viewed as redundant.
   [DBM: 2022.10.17]

   basicBlock and alignedBlock revised so that a block with a single format member reduces to
   that format.  This prevents trivial nesting of blocks nesting of blocks, e.g. block(block(block(...))).
   [DBM: 2022.10.24]

*)
