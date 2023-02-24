(* smlnj-lib/PrettyPrint/src/prettyprint.sml *)

(* New Prettyprinter, main interface
 * Version 7:
 *   -- prettyprinter factored into Format, Measure: MEASURE, Render : RENDER,
        and NewPrettyPrint: NEW_PRETTYPRINT (NewPP : NEW_PP) structures
 *   -- memoized block flat measures
 *
 * Version 7.4:
 *   -- structure NewPP --> NewPrettyPrint
 *   -- signature NEW_PP --> NEW_PRETTYPRINT
 *   -- separator --> break, SEP --> BRK, SBLOCK --> BLOCK, sblock --> block, siblock --> iblock
 *   -- added: vHeaders and vHeaderFormats (moved from NewPPUtil)
 *   -- removed: tuple
 *
 * Version 8:
 *   -- bindent, xiblock, etc. eliminated; replaced by HINDENT, SINDENT format constructors
 *
 * Version 8.1 [2023.1.2]
 *   -- Merge HINDENT and SINDENT into a single INDENT constructor acting like SINDENT
 *   -- the breakIndent function replaces hardIndent (but breakIndent _unconditionally_ performs
 *      a line break before the indented format, so its behavior is different from hardIndent).
 *
 * Version 8.2 [2023.1.5]
 *   -- files newprettyprint.{sig,sml} renamed prettyprint.{sig,sml}
 *   -- Renamed
 *      NEW_PRETTYPRINT --> PRETTYPRINT
 *      NewPrettyPrint --> PrettyPrint
 *
 * Verion 8.3 [2023.1.6]
 *   -- Removed
 *      breakIndent
 *
 * Version 8.4 [2023.2.22]
 *   -- renamed:
 *      Hard -> Hard
 *      Soft -> Soft
 *      tupleFormat -> tuple
 *      list -> listMap
 *      formatSeq -> sequenceMap
 *      formatClosedSeq -> closedSequenceMap
 *      vHeaders -> vHeadersMap
 *      vHeaderFormats -> vHeaders
 *      hblock -> hcat
 *      pblock -> pcat
 *      vblock -> vcat
 *      cblock -> ccat
 *      hvblock -> hvcat
 *   -- removed:
 *      tuple [i.e. the function that should have been called tupleMap; tupleFormat becomes tuple]
 *      hcat [recycled as the name of the former hblock]
 *      pcat [recycled as the name of the former pblock]
 *      vcat [recycled as the name of the former vblock]
 *      ccat [recycled as the name of the former cblock]
 *)

(* Defines:
 *   structure PrettyPrint: PRETTYPRINT
 *)

structure PrettyPrint :> PRETTYPRINT =
struct

local

  structure M = Measure
  structure R = Render

in

open Format  (* import datatypes format, element, break; format exported as abstract in NEW_PRETTYPRINT *)

(*** the basic block building functions ***)

(* reduceFormats : format list -> format list *)
(*   filter out empty components formats *)
fun reduceFormats (formats: format list) =
    let fun notEmpty EMPTY = false
	  | notEmpty _ = true
     in List.filter notEmpty formats
    end

(* reduceElements : element list -> element list *)
(*   filter out FMT EMPTY elements *)
fun reduceElements (elements: element list) =
    let fun notEmpty (FMT EMPTY) = false
	  | notEmpty _ = true
     in List.filter notEmpty elements
    end

(* block : element list -> format
 *   Construct a BLOCK with explicit, possibly heterogeous, breaks.
 *   Returns EMPTY if the element list is null. *)
fun block elements =
    (case reduceElements elements
       of nil => EMPTY
	| [FMT fmt] => fmt  (* special blocks consisting of a single (FMT fmt) element, reduce to fmt *)
        | _ => BLOCK {elements = elements, measure = M.measureElements elements})

(* aBlock : alignment -> format list -> format *)
(* An aligned block with no component formats reduces to EMPTY, regardless of alignment. *)
fun aBlock alignment formats =
    let val breaksize = case alignment of C => 0 |  _ => 1
     in case reduceFormats formats
	  of nil => EMPTY
	   | [fmt] => fmt
	   | formats' =>
	       ABLOCK {formats = formats', alignment = alignment, measure = M.measureFormats (breaksize, formats')}
    end


(*** block building functions for non-indenting blocks ***)

(* constructing aligned blocks: common abbreviations *)
(* xcat : format list -> format, for x = h, v, p, c *)
val hcat = aBlock H
val vcat = aBlock V
val pcat = aBlock P
val ccat = aBlock C

(* "conditional" formats *)

(* tryFlat : format -> format *)
fun tryFlat (fmt: format) = ALT (FLAT fmt, fmt)

(* alt : format * format -> format *)
val alt = ALT

(* hvblock : format list -> format *)
fun hvcat fmts = tryFlat (vcat fmts)


(*** format-building utility functions for some primitive types ***)

val empty : format = EMPTY

(* text : string -> format *)
val text : string -> format = TEXT

(* integer : int -> format *)
fun integer (i: int) : format = text (Int.toString i)

(* string : string -> format *)
fun string (s: string) = text (String.concat ["\"", String.toString s, "\""])  (* was using PrintUtil.formatString *)

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
fun appendNewLine fmt = block [FMT fmt, BRK Hard]

(* label : string -> format -> format *)
(* labeled formats, i.e. formats preceded by a string label, a commonly occurring pattern *)
fun label (str:string) (fmt: format) = hcat [text str, fmt]


(*** functions for formatting sequences of formats (format lists) ***)

(* alignmentToBreak : alignment -> break
 * The virtual break associated with each alignment.
 * This is a utility function used in functions sequence and formatSeq *)
fun alignmentToBreak H = Space 1
  | alignmentToBreak V = Hard
  | alignmentToBreak P = Soft 1
  | alignmentToBreak C = NullBreak

(* sequence : alignement -> format -> format list -> format
 *  The second argument (sep: format) is normally a symbol (TEXT) such as comma or semicolon *)
fun sequence (alignment: alignment) (sep: format) (formats: format list) =
    let val separate =
	    (case alignment
	       of C => (fn elems => FMT sep :: elems)  (* alignment = C *)
	        | _ =>
		  let val break = alignmentToBreak alignment
		   in (fn elems => FMT sep :: BRK break :: elems)
		  end)
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

(* xsequence : [sep:]format -> format list -> format, x = h, v, p, c *)
val hsequence = sequence H
val psequence = sequence P
val vsequence = sequence V
val csequence = sequence C

(* tuple : format list -> format  -- parenthesized, comma separated, packed alignment sequence
 *  not really restricted to actual "tuples", just "tuple-style" formatting. Constituent formats can represent
 *  values of heterogeneous types. *)
fun tuple formats = parens (psequence comma formats)

(* list : format list -> format  -- packed alignment
 *  typically used for lists, but the constituent formats can represent values of heterogeneous types. *)
fun list formats = brackets (psequence comma formats)

fun option (formatOp: format option) =
    case formatOp
      of NONE => text "NONE"
       | SOME fmt => ccat [text "SOME", parens fmt]

(*** functions for formatting sequences of values (of homogeneous types, i.e. 'a lists) ***)

(* sequenceMap : {alignment: alignment, sep: format} -> ([formatter:] 'a -> format} -> 'a list -> format *)
fun 'a sequenceMap {alignment: alignment, sep: format}  (formatter: 'a -> format) (xs: 'a list) =
    let val separate =
	    (case alignment
	       of C => (fn elems => FMT sep :: elems)  (* alignment = C *)
	        | _ =>
		  let val break = alignmentToBreak alignment
		   in (fn elems => FMT sep :: BRK break :: elems)
		  end)
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

(* closedSequenceMap :
     {alignment: alignment, front: format, sep: format, back: format} 
     -> ([formatter:] 'a -> format}
     -> 'a list
     -> format *)
fun 'a closedSequenceMap
       {alignment: alignment, front: format, sep: format, back: format} (formatter: 'a -> format) (xs: 'a list) =
    enclose {front=front, back=back} (sequenceMap {alignment=alignment, sep=sep} formatter xs)

(* alignedListMap : alignment -> ('a -> format) -> 'a list -> format *)
fun 'a alignedListMap alignment (formatter : 'a -> format) (xs: 'a list) =
    closedSequenceMap
      {alignment=alignment, front = lbracket, back = rbracket, sep = comma} formatter xs

(* listMap : ('a -> format) -> 'a list -> format *)
(* packed-style formatting of an 'a list, given an 'a formatter function *)
fun 'a listMap (formatter : 'a -> format) (xs: 'a list) =
    closedSequenceMap
      {alignment=P, front = lbracket, back = rbracket, sep = comma} formatter xs

(* optionMap : ('a -> format) -> 'a option -> format *)
fun 'a optionMap (formatter: 'a -> format) (xOp: 'a option) =
    case xOp
      of NONE => text "NONE"
       | SOME x => ccat [text "SOME", parens (formatter x)]


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

(* vHeadersMap : {header1 : string, header2 : string} -> ([formatter:] 'a -> format) -> 'a list -> format *)
fun vHeadersMap {header1: string, header2: string} (formatter: 'a -> format) (elems: 'a list) =
    let val (header1, header2) = padHeaders (header1, header2)
     in case elems
	  of nil => empty
	   | elem :: rest =>
	       vblock
		 (hcat [text header1, formatter elem] ::
		  map (fn e => hcat [text header2, formatter e]) rest)
    end

(* vHeaders : {header1 : string, header2 : string} -> format list -> format *)
fun vHeaders {header1: string, header2: string} (elems: format list) =
    let val (header1, header2) = padHeaders (header1, header2)
     in case elems
	  of nil => empty
	   | elem :: rest =>
	       vblock
		 (hcat [text header1, elem] ::
		  map (fn fmt => hcat [text header2, fmt]) rest)
    end


(*** "indenting" formats ***)

(* indent : ([n:] int) -> format -> format *)
(* When applied to EMPTY, produces EMPTY
 * The resulting format is soft-indented n _additional_ spaces,
 *   i.e. indents an additional n spaces iff following a line break with its indentation. *)
fun indent (n: int) (fmt: format) =
    (case fmt
       of EMPTY => EMPTY
        | _ => INDENT (n, fmt))


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

   basicBlock -> block, alignedBlock -> aBlock, xblock becomes xcat (x = h, p, v, c) with same type,
   hvblock -> hvcat, other renamings: see Version 8.4 changes note at beginning of this file. Some of
   these changes suggested by JHR. Thinking about separating the render and printing functions into a
   separate structure and possilby parameterizing wrt a "device" record that would contain printing functions
   for strings, spaces, and newlines, and possibly the lineWidth parameter.

*)
