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

(* basic block building functions *)

(* specialBlock : bindent -> element list -> format
 *   construct a SBLOCK with explicit, possibly heterogeous, separators *)
fun specialBlock bindent elements =
    SBLOCK {elements = elements, bindent = bindent, measure = M.measureElements elements}

(* alignedBlock : alignment -> bindent -> format list -> format *)
fun alignedBlock alignment bindent formats =
    ABLOCK {formats = formats, alignment = alignment, bindent = bindent,
	    measure = M.measureFormats formats}

(* block building functions for non-indenting blocks *)

(* sblock : element list -> format *)
(* construct a block with explicit, possibly heterogeous, separators, NI bindent *)
fun sblock elements = specialBlock NI elements

(* xblock : format list -> format, for x = h, v, p *)
val hblock = alignedBlock H NI
val vblock = alignedBlock V NI
val pblock = alignedBlock P NI

(* block building functions producing (for bindent =  HI, SI) indented blocks
 * For Version 7, we measure indented blocks the same as non-indented blocks *)  

(* siblock : bindent * element list -> format *)
(* construct a block with explicit, possibly heterogeous, separators *)
val siblock = specialBlock

(* xiblock : bindent -> format list -> format, for x = h, v, p
 *  for constructing aligned and possibly indented blocks *)
val hiblock = alignedBlock H
val viblock = alignedBlock V
val piblock = alignedBlock P

(* "conditional" formats *)

(* tryFlat : format -> format *)
fun tryFlat (fmt: format) = ALT (FLAT fmt, fmt)

(* alt : format * format -> format *)
val alt = ALT

(* hvblock : format list -> format *)
fun hvblock fmts = tryFlat (vblock fmts)

(* some format-building utility functions *)

val empty : format = TEXT ""

(* text : string -> format *)
val text : string -> format = TEXT

(* integer : int -> format *)
fun integer (i: int) : format = text (Int.toString i)

(* string : string -> format *)
fun string (s: string) = text (concat ["\"", s, "\""])  (* was using PrintUtil.formatString *)

(* "punctuation" characters *)

val comma : format = text ","
val colon : format = text ":"
val semicolon : format = text ";"
val lparen = text "("
val rparen = text ")"
val lbracket = text "["		  
val rbracket = text "]"		  
val lbrace = text "{"		  
val rbrace = text "}"		  

(* xcat: binary versions of xblock functions (x = p, h, v) *)

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
fun ccat (left, right) = sblock [FMT left, FMT right]

(* NOT USED, could delete *)
(* separate : format list * separator -> element list *)
fun separate (fmts: format list, separator: separator) : element list = 
    let val elements = map FMT fmts (* translate the formats to SBLOCK elements *)
	fun addSeps nil = nil
	  | addSeps fmts =
	    let fun inter nil = nil
		  | inter [fmt] = [fmt]
		  | inter (fmt :: rest) =  (* rest non-null *)
		      fmt :: SEP separator :: inter rest
	      in inter fmts
	     end
      in addSeps elements
     end

(* concat : format list -> format
 * WARNING: be careful of multi-line formats *)				  
fun concat nil = empty
  | concat fmts = sblock (map FMT fmts)  (* no separators! *)

(* enclose : {front : format, back : format} -> format -> format *)
(* tight -- no space between front, back, and fmt *)
fun enclose {front: format, back: format} fmt =
    concat [front, fmt, back]

(* parens : format -> format *)
val parens = enclose {front = lparen, back = rparen}

(* brackets : format -> format *)
val brackets = enclose {front = text "[", back = text "]"}

(* braces : format -> format *)
val braces = enclose {front = text "{", back = text "}"}

(* sequence : {alignement: alignment, sep: format} -> format list -> format *)
fun sequence {alignment: alignment, sep: format} (formats: format list) =
    let val separator = alignmentSeparator alignment
	fun addSeps nil = nil
	  | addSeps fmts =  (* fmts non-null *)
	    let fun inter [fmt] = [FMT fmt]
		  | inter (fmt :: rest) =  (* rest non-null *)
		      FMT fmt :: FMT sep :: SEP separator :: inter rest
		  | inter nil = nil (* won't happen *)
	     in inter fmts
	     end
      in sblock (addSeps formats)
     end

(* tupleFormats : format list -> format *)
fun tupleFormats formats = parens (sequence {alignment=P, sep=comma} formats)

(* listFormats : format list -> format *)
fun listFormats formats = brackets (sequence {alignment=P, sep=comma} formats)


(* formatSeq : {alignment: alignment, sep: format, formatter : 'a -> format} -> 'a list -> format *)
fun 'a formatSeq
       {alignment: alignment, sep: format, formatter: 'a -> format}
       (xs: 'a list) =
    let val separator = alignmentSeparator alignment
	val formats = map formatter xs
	fun addSeps nil = nil
	  | addSeps fmts =  (* fmts non-null *)
	    let fun inter [fmt] = [FMT fmt]
		  | inter (fmt :: rest) =  (* rest non-null *)
		      FMT fmt :: FMT sep :: SEP separator :: inter rest
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

(* formatTuple : ('a -> format) -> 'a list -> format *)
(* packed-style formatting of a tuple *)
fun 'a formatTuple (formatter : 'a -> format) (xs: 'a list) =
    formatClosedSeq
      {alignment=P, front = lparen, back = rparen, sep = comma, formatter = formatter}
      xs

(* formatAlignedList : alignment -> ('a -> format) -> 'a list -> format *)
fun 'a formatAlignedList alignment (formatter : 'a -> format) (xs: 'a list) =
    formatClosedSeq
      {alignment=alignment, front = lbracket, back = rbracket, sep = comma, formatter = formatter}
      xs

(* formatList : ('a -> format) -> 'a list -> format *)
(* packed-style formatting of a list *)
fun 'a formatList (formatter : 'a -> format) (xs: 'a list) =
    formatClosedSeq
      {alignment=P, front = lbracket, back = rbracket, sep = comma, formatter = formatter}
      xs

(* appendNewLine : format -> format *)
fun appendNewLine fmt = sblock [FMT fmt, SEP HardLine]

(* "indenting" formats *)

(* hardIndent : int -> format -> format *)
fun hardIndent (n: int) (fmt: format) =
    hiblock (HI n) [fmt]

(* softIndent : int -> format -> format *)
fun softIndent (n: int) (fmt: format) =
    hiblock (SI n) [fmt]

(* functions for setting and accessing the line width
 * NOTE: setLineWidthFun does not have an effect until actuall pretty printing occurs,
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

(* printing (i.e., rendering) formats *)

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
