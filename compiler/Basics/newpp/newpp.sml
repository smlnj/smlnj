(* sml/Dev/pp/new/new7/newpp.sml *)

(* New Prettyprinter, main 
 * Version 7:
 *   -- prettyprinter factored into Format, Measure: MEASURE, Render : RENDER, and NewPP : NEW_PP structures
 *   -- memoized block flat measures
 *)

(* Defines:
 *   structure NewPP: NEW_PP
 *)

structure NewPP :> NEW_PP =
struct

local

  structure M = Measure
  structure R = Render

in		      

open Format  (* defines types format, element, separator, bindent *)

(*** the basic block building functions ***)

(* reduce : format list -> format list *)
fun reduce (formats: format list) =
    let fun notEmpty EMPTY = false
	  | notEmpty _ = true
     in List.filter notEmpty formats
    end

(* specialBlock : bindent -> element list -> format
 *   Construct an SBLOCK with explicit, possibly heterogeous, separators.
 *   Returns EMPTY if the element list is null. *)
fun specialBlock bindent elements =
    (case elements
       of nil => EMPTY
        | _ => SBLOCK {elements = elements, bindent = bindent, measure = M.measureElements elements})

(* alignedBlock : alignment -> bindent -> format list -> format *)
(* A block with no element formats reduces to EMPTY, regardless of alignment or indentation. *)
fun alignedBlock alignment bindent formats =
    let val sepsize = case alignment of C => 0 |  _ => 1
     in case formats
	  of nil => EMPTY
	   | _ => let val formats' = reduce formats
		   in ABLOCK {formats = formats', alignment = alignment, bindent = bindent,
	                      measure = M.measureFormats (sepsize, formats')}
		  end
     end


(*** block building functions for non-indenting blocks ***)

(* sblock : element list -> format *)
(* construct a block with explicit, possibly heterogeous, separators, NI bindent *)
fun sblock elements = specialBlock NI elements

(* xblock : format list -> format, for x = h, v, p *)
(* constructing nonindented alighned blocks *)
val hblock = alignedBlock H NI
val vblock = alignedBlock V NI
val pblock = alignedBlock P NI
val cblock = alignedBlock C NI

(* block building functions producing (for bindent =  HI, SI) indented blocks
 * For Version 7, we measure indented blocks the same as non-indented blocks *)  

(* siblock : bindent * element list -> format *)
(* construct a block with explicit, possibly heterogeous, separators *)
val siblock = specialBlock

(* xiblock : bindent -> format list -> format, for x = h, v, p, c
 *  for constructing aligned and possibly indented blocks.
 *  The alignment and indentation do not matter if applied to an empty list of formats;
 *  in this case the result is always EMPTY. *)
val hiblock = alignedBlock H
val viblock = alignedBlock V
val piblock = alignedBlock P
val ciblock = alignedBlock C

(* "conditional" formats *)

(* tryFlat : format -> format *)
fun tryFlat (fmt: format) = ALT (FLAT fmt, fmt)

(* alt : format * format -> format *)
val alt = ALT

(* hvblock : format list -> format *)
fun hvblock fmts = tryFlat (vblock fmts)


(*** format-building utility functions for some primitive types ***)

val empty : format = EMPTY

(* text : string -> format *)
val text : string -> format = TEXT

(* integer : int -> format *)
fun integer (i: int) : format = text (Int.toString i)

(* string : string -> format *)
fun string (s: string) = text (String.concat ["\"", s, "\""])  (* was using PrintUtil.formatString *)

(* char : char -> format *)
fun char (c: char) = cblock [text "#", string (Char.toString c)]

(* bool : bool -> format *)
fun bool (b: bool) = text (Bool.toString b)


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
fun pcat (leftFmt, rightFmt) = pblock [leftFmt, rightFmt]

(* hcat : format * format -> format
 * separate r, l with a space *)
fun hcat (leftFmt, rightFmt) = hblock [leftFmt, rightFmt]

(* vcat : format * format -> format
 * separate r, l with a hard line break *)
fun vcat (leftFmt, rightFmt) = vblock [leftFmt, rightFmt]

(* ccat : format * format -> format
 * concatenate left and right formats with no separater *)
fun ccat (left, right) = cblock [left, right]


(*** wrapping or closing formats, e.g. parenthesizing a format ***)

(* enclose : {front : format, back : format} -> format -> format *)
(* tight -- no space between front, back, and fmt *)
fun enclose {front: format, back: format} fmt =
    cblock [front, fmt, back]

(* parens : format -> format *)
val parens = enclose {front = lparen, back = rparen}

(* brackets : format -> format *)
val brackets = enclose {front = lbracket, back = rbracket}

(* braces : format -> format *)
val braces = enclose {front = lbrace, back = rbrace}

(* appendNewLine : format -> format *)
fun appendNewLine fmt = sblock [FMT fmt, SEP HardLine]

(* label : string -> format -> format *)
(* labeled formats, i.e. formats preceded by a string label, a commonly occurring pattern *)
fun label (str:string) (fmt: format) = hcat (text str, fmt)


(*** functions for formatting sequences of formats (format lists) ***)

(* sequence : alignement -> format -> format list -> format
 *  The second argument (sep: format) is a printable separator such as comma or semicolon *)
fun sequence (alignment: alignment) (sep: format) (formats: format list) =
    let val separate =
	    (case alignmentSeparator alignment
	       of NONE => (fn elems => FMT sep :: elems)  (* alignment = C *)
	        | SOME separator => (fn elems => FMT sep :: SEP separator :: elems))
	fun addSeps nil = nil
	  | addSeps fmts =  (* fmts non-null *)
	    let fun inter [fmt] = [FMT fmt]
		  | inter (fmt :: rest) =  (* rest non-null *)
		      FMT fmt :: (separate (inter rest))
		  | inter nil = nil (* won't happen *)
	     in inter fmts
	     end
      in sblock (addSeps formats)
     end

(* hsequence : format -> format list -> format *)
val hsequence = sequence H

(* psequence : format -> format list -> format *)
val psequence = sequence P

(* vsequence : format -> format list -> format *)
val vsequence = sequence V

(* csequence : format -> format list -> format *)
val csequence = sequence C

(* tupleFormats : format list -> format  -- parenthesized, comma separated, packed alignment sequence 
 *  not really restricted to actual "tuples", just "tuple-style" formatting. Constituent formats can represent
 *  values of heterogeneous types. *)
fun tupleFormats formats = parens (psequence comma formats)

(* listFormats : format list -> format  -- packed alignment
 * typically used for lists, but the constituent formats can represent values of heterogeneous types. *)
fun listFormats formats = brackets (psequence comma formats)

fun optionFormat (formatOp: format option) =
    case formatOp
      of NONE => text "NONE"
       | SOME fmt => ccat (text "SOME", parens fmt)


(*** functions for formatting sequences of values (of homogeneous types, i.e. 'a lists) ***)

(* formatSeq : {alignment: alignment, sep: format, formatter : 'a -> format} -> 'a list -> format *)
fun 'a formatSeq
       {alignment: alignment, sep: format, formatter: 'a -> format}
       (xs: 'a list) =
    let val separate =
	    (case alignmentSeparator alignment
	      of NONE => (fn elems => FMT sep :: elems)  (* alignment = C *)
	       | SOME separator => (fn elems => FMT sep :: SEP separator :: elems))
	val formats = map formatter xs
	fun addSeps nil = nil
	  | addSeps fmts =  (* fmts non-null *)
	    let fun inter [fmt] = [FMT fmt]
		  | inter (fmt :: rest) =  (* rest non-null *)
		      FMT fmt :: (separate (inter rest))
		  | inter nil = nil (* won't happen *)
	     in inter fmts
	     end
      in sblock (addSeps formats)
     end

(* formatClosedSeq :
     {alignment: alignment, front: format, sep: format, back: format, formatter: 'a -> format}
     -> 'a list
     -> format *)
fun 'a formatClosedSeq
       {alignment: alignment, front: format, sep: format, back: format, formatter: 'a -> format}
       (xs: 'a list) =
    enclose {front=front, back=back} (formatSeq {alignment=alignment, sep=sep, formatter=formatter} xs)

(* tuple : ('a -> format) -> 'a list -> format *)
(* packed-style formatting of a _homogeneous_ tuple of 'a values.
 * This is of limited value since most tuples are not homogeneous! *)
fun 'a tuple (formatter : 'a -> format) (xs: 'a list) =
    formatClosedSeq
      {alignment=P, front = lparen, back = rparen, sep = comma, formatter = formatter}
      xs

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


(*** "indenting" formats ***)

(* hardIndent : int -> format -> format *)
(* When applied to EMPTY, produces EMPTY *)
fun hardIndent (n: int) (fmt: format) =
    (case fmt
       of EMPTY => EMPTY
        | _ => hiblock (HI n) [fmt])

(* softIndent : int -> format -> format *)
(* When applied to EMPTY, produces EMPTY *)
fun softIndent (n: int) (fmt: format) =
    (case fmt
       of EMPTY => EMPTY
        | _ => hiblock (SI n) [fmt])


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

end (* top local *)
end (* structure NewPP *)

(* NOTES:

1. We have sequence formating funtions that act on lists of arbitrary values (of a given type),
   with a supplied formatting function for the element type, and other functions that act on lists of formats.

   The first sort can easily be simulated by translating the value list into a format list
   by mapping the formatter over the values.  This seems to be preferabel, so the former sequencing
   functions (formatSeq, formatClosedSeq, tuple, list, alignedList) can be viewed as redundant.
   [DBM: 2022.10.17].
*)    
