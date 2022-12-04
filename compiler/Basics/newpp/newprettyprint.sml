(* compiler/Basics/newpp/newprettyprint.sml *)

(* New Prettyprinter, main interface
 * Version 7:
 *   -- prettyprinter factored into Format, Measure: MEASURE, Render : RENDER,
        and NewPP : NEW_PP structures
 *   -- memoized block flat measures
 * Version 7.4:
 *   -- structure NewPP --> NewPrettyPrint
 *   -- signature NEW_PP --> NEW_PRETTYPRINT
 *   -- separator --> break, SEP --> BRK, SBLOCK --> BLOCK, sblock --> block, siblock --> iblock
 *   -- added: vHeaders and vHeaderFormats (moved from NewPPUtil)
 *   -- removed: tuple
 *)

(* Defines:
 *   structure NewPrettyPrint: NEW_PRETTYPRINT
 *)

structure NewPrettyPrint :> NEW_PRETTYPRINT =
struct

local

  structure M = Measure
  structure R = Render

in		      

open Format  (* defines types format, element, break, bindent *)

(*** the basic block building functions ***)

(* reduceFormats : format list -> format list *)
fun reduceFormats (formats: format list) =
    let fun notEmpty EMPTY = false
	  | notEmpty _ = true
     in List.filter notEmpty formats
    end

(* reduceElements : element list -> element list *)
(* eliminate EMPTY format elements *)
fun reduceElements (elements: element list) =
    let fun notEmpty (FMT EMPTY) = false
	  | notEmpty _ = true
     in List.filter notEmpty elements
    end

(* basicBlock : bindent -> element list -> format
 *   Construct an BLOCK with explicit, possibly heterogeous, breaks.
 *   Returns EMPTY if the element list is null. *)
fun basicBlock bindent elements =
    (case reduceElements elements
       of nil => EMPTY
	| [FMT fmt] => fmt  (* special blocks consisting of a single (FMT fmt) element, reduce to fmt *)
        | _ => BLOCK {elements = elements, bindent = bindent, measure = M.measureElements elements})

(* alignedBlock : alignment -> bindent -> format list -> format *)
(* A block with no element formats reduces to EMPTY, regardless of alignment or indentation. *)
fun alignedBlock alignment bindent formats =
    let val sepsize = case alignment of C => 0 |  _ => 1
     in case reduceFormats formats
	  of nil => EMPTY
	   | [fmt] => fmt  (* singleton aligned blocks reduce to their sole component format *)
	   | formats' =>
	       ABLOCK {formats = formats', alignment = alignment, bindent = bindent,
	               measure = M.measureFormats (sepsize, formats')}
    end


(*** block building functions for non-indenting blocks ***)

(* block : element list -> format *)
(* construct a block with explicit, possibly heterogeous, breaks, NI bindent *)
fun block elements = basicBlock NI elements

(* constructing nonindented aligned blocks *)
(* xblock : format list -> format, for x = h, v, p, c *)
val hblock = alignedBlock H NI
val vblock = alignedBlock V NI
val pblock = alignedBlock P NI
val cblock = alignedBlock C NI

(* block building functions producing (for bindent =  HI, SI) indented blocks
 * For Version 7, we measure indented blocks the same as non-indented blocks *)  

(* siblock : bindent * element list -> format *)
(* construct a block with explicit, possibly heterogeous, breaks *)
val iblock = basicBlock

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
fun appendNewLine fmt = block [FMT fmt, BRK HardLine]

(* label : string -> format -> format *)
(* labeled formats, i.e. formats preceded by a string label, a commonly occurring pattern *)
fun label (str:string) (fmt: format) = hcat (text str, fmt)


(*** functions for formatting sequences of formats (format lists) ***)

(* sequence : alignement -> format -> format list -> format
 *  The second argument (sep: format) is a printable break such as comma or semicolon *)
fun sequence (alignment: alignment) (sep: format) (formats: format list) =
    let val separate =
	    (case alignmentToBreak alignment
	       of NullBreak => (fn elems => FMT sep :: elems)  (* alignment = C *)
	        | break => (fn elems => FMT sep :: BRK break :: elems))
	fun addBreaks nil = nil
	  | addBreaks fmts =  (* fmts non-null *)
	      let fun inter [fmt] = [FMT fmt]
		    | inter (fmt :: rest) =  (* rest non-null *)
			FMT fmt :: (separate (inter rest))
		    | inter nil = nil (* won't happen *)
	       in inter fmts
	      end
      in block (addBreaks formats)
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
 *  typically used for lists, but the constituent formats can represent values of heterogeneous types. *)
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
	    (case alignmentToBreak alignment
	       of NullBreak => (fn elems => FMT sep :: elems)  (* alignment = C *)
	        | break => (fn elems => FMT sep :: BRK break :: elems))
	val formats = map formatter xs
	fun addBreaks nil = nil
	  | addBreaks fmts =  (* fmts non-null *)
	    let fun inter [fmt] = [FMT fmt]
		  | inter (fmt :: rest) =  (* rest non-null *)
		      FMT fmt :: (separate (inter rest))
		  | inter nil = nil (* won't happen *)
	     in inter fmts
	     end
     in block (addBreaks formats)
    end

(* formatClosedSeq :
     {alignment: alignment, front: format, sep: format, back: format, formatter: 'a -> format}
     -> 'a list
     -> format *)
fun 'a formatClosedSeq
       {alignment: alignment, front: format, sep: format, back: format, formatter: 'a -> format}
       (xs: 'a list) =
    enclose {front=front, back=back} (formatSeq {alignment=alignment, sep=sep, formatter=formatter} xs)

(* DROPPED!
(* tuple : ('a -> format) -> 'a list -> format *)
(* packed-style formatting of a _homogeneous_ tuple of 'a values.
 * This was of limited value since most tuples are not homogeneous! Use tupleFormats instead. *)
fun 'a tuple (formatter : 'a -> format) (xs: 'a list) =
    formatClosedSeq
      {alignment=P, front = lparen, back = rparen, sep = comma, formatter = formatter}
      xs
*)

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

(* vHeaders : {header1 : string, header2 : string, formatter: 'a -> format}
              -> 'a list -> format *)
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
end (* structure NewPrettyPrint *)

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
