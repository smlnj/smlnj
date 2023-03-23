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
 *      NullBreak -> Null
 *      tupleFormats -> tuple
 *      list -> listMap (and removed)
 *      formatSeq -> sequenceMap  (and removed)
 *      formatClosedSeq -> closedSequenceMap (and removed)
 *      vHeaders -> vHeadersMap (and removed)
 *      vHeaderFormats -> vHeaders
 *   -- removed:
 *      tuple [i.e. the function that should have been called tupleMap; tupleFormat renamed as "tuple"]
 *      binary xcat functions, replaced by calls of corresponding xblock but with lists of 2 formats:
 *      hcat [hcat (f1, f2) -> hblock [f1,f2]]
 *      pcat [-> pblock]
 *      vcat [-> vblock]
 *      ccat [-> cblock]
 *  
 *      The map versions of various functions: (these are not used anywhere in SML/NJ?)
 *      sequenceMap
 *      closedSequenceMap
 *      listMap
 *      alignedListMap
 *      optionMap
 * 
 * Version 8.5
 *   render and printFormat functions moved to new PrintFormat structure
 *   signature PRETTYPRINT --> signature FORMATTING
 *   structure PrettyPrint --> structure Formatting
 * 
 * Version 9.1
 *   -- Added
 *      styled
 *)

(* Defines:
 *   structure Formatting: FORMATTING
 *)

structure Formatting :> FORMATTING =
struct

local

  structure S = Style
  structure F = Format
  structure M = Measure

in

(* types from the Format structure *)
type format = F.format
datatype alignment = datatype F.alignment
datatype element = datatype F.element
datatype break = datatype F.break

(* but we need a coercion back to Format.format so that we can pass abstract formats to
 * functions like Render.render that need the concrete type. formatRep is the identity. *)
fun formatRep (fmt : F.format) : F.format = fmt

(*** the basic block building functions ***)

(* reduceFormats : format list -> format list *)
(*   filter out empty components formats *)
fun reduceFormats (formats: format list) =
    let fun notEmpty F.EMPTY = false
	  | notEmpty _ = true
     in List.filter notEmpty formats
    end

(* reduceElements : element list -> element list *)
(*   filter out FMT EMPTY elements *)
fun reduceElements (elements: F.element list) =
    let fun notEmpty (F.FMT F.EMPTY) = false
	  | notEmpty _ = true
     in List.filter notEmpty elements
    end

(* block : element list -> format
 *   Construct a BLOCK with explicit, possibly heterogeous, breaks.
 *   Returns EMPTY if the element list is null. *)
fun block elements =
    (case reduceElements elements
       of nil => F.EMPTY
	| [F.FMT fmt] => fmt  (* special blocks consisting of a single (FMT fmt) element, reduce to fmt *)
        | _ => F.BLOCK {elements = elements, measure = M.measureElements elements})

(* aBlock : alignment -> format list -> format *)
(* An aligned block with no component formats reduces to EMPTY, regardless of alignment. *)
fun aBlock alignment formats =
    let val breaksize = case alignment of F.C => 0 |  _ => 1
     in case reduceFormats formats
	  of nil => F.EMPTY
	   | [fmt] => fmt
	   | formats' =>
	       F.ABLOCK {formats = formats', alignment = alignment, measure = M.measureFormats (breaksize, formats')}
    end


(*** block building functions for non-indenting blocks ***)

(* constructing aligned blocks: common abbreviations *)
(* xblock : format list -> format, for x = h, v, p, c *)
val hblock = aBlock F.H
val vblock = aBlock F.V
val pblock = aBlock F.P
val cblock = aBlock F.C

(* "conditional" formats *)

(* tryFlat : format -> format *)
fun tryFlat (fmt: format) = F.ALT (F.FLAT fmt, fmt)

(* alt : format * format -> format *)
val alt = F.ALT

(* hvblock : format list -> format *)
fun hvblock fmts = tryFlat (vblock fmts)


(*** format-building utility functions for some primitive types ***)

val empty : format = F.EMPTY

(* text : string -> format *)
val text : string -> F.format = F.TEXT

(* integer : int -> format *)
fun integer (i: int) : format = text (Int.toString i)

(* string : string -> format *)
fun string (s: string) : format =
    text (String.concat ["\"", String.toString s, "\""])  (* was using PrintUtil.formatString *)

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
fun appendNewLine fmt = block [F.FMT fmt, F.BRK F.Hard]

(* label : string -> format -> format *)
(* labeled formats, i.e. formats preceded by a string label, a commonly occurring pattern *)
fun label (str:string) (fmt: format) = hblock [text str, fmt]


(*** functions for formatting sequences of formats (format lists) ***)

(* alignmentToBreak : alignment -> break
 * The virtual break associated with each alignment.
 * This is a utility function used in functions sequence and formatSeq *)
fun alignmentToBreak F.H = F.Space 1
  | alignmentToBreak F.V = F.Hard
  | alignmentToBreak F.P = F.Soft 1
  | alignmentToBreak F.C = F.Null

(* sequence : alignement -> format -> format list -> format
 *  Format a sequence of formats, specifying alignment and separator format used between elements.
 *  The second argument (sep: format) is normally a symbol (TEXT) such as comma or semicolon *)
fun sequence (alignment: F.alignment) (sep: format) (formats: format list) =
    let val separate =
	    (case alignment
	       of C => (fn elems => F.FMT sep :: elems)  (* alignment = C *)
	        | _ =>
		  let val break = alignmentToBreak alignment
		   in (fn elems => F.FMT sep :: F.BRK break :: elems)
		  end)
	fun addBreaks nil = nil
	  | addBreaks fmts =  (* fmts non-null *)
	      let fun inter [fmt] = [F.FMT fmt]
		    | inter (fmt :: rest) =  (* rest non-null *)
			F.FMT fmt :: (separate (inter rest))
		    | inter nil = nil (* won't happen *)
	       in inter fmts
	      end
      in block (addBreaks formats)
     end

(* xsequence : [sep:]format -> format list -> format, x = h, v, p, c *)
val hsequence = sequence F.H
val psequence = sequence F.P
val vsequence = sequence F.V
val csequence = sequence F.C

(* tuple : format list -> format
 *  parenthesized, comma-separated, packed alignment sequence
 *  not really restricted to actual "tuples", just "tuple-style" formatting. Constituent formats can represent
 *  values of heterogeneous types. *)
fun tuple (formats: format list) = parens (psequence comma formats)

(* list : format list -> format
 *  bracketed, comma-separated, packed alignment
 *  typically used for lists, but the constituent formats can represent values of heterogeneous types. *)
fun list (formats: format list) = brackets (psequence comma formats)

fun option (formatOp: format option) =
    case formatOp
      of NONE => text "NONE"
       | SOME fmt => cblock [text "SOME", parens fmt]

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

(* vHeaders : {header1 : string, header2 : string} -> format list -> format *)
fun vHeaders {header1: string, header2: string} (elems: format list) =
    let val (header1, header2) = padHeaders (header1, header2)
     in case elems
	  of nil => empty
	   | elem :: rest =>
	       vblock
		 (hblock [text header1, elem] ::
		  map (fn fmt => hblock [text header2, fmt]) rest)
    end

(* DEPRICATED! *)
(* vHeadersMap : {header1 : string, header2 : string} -> ([formatter:] 'a -> format) -> 'a list -> format *)
fun vHeadersMap (headers as {header1: string, header2: string}) (formatter: 'a -> format) (xs: 'a list) =
    vHeaders headers (map formatter xs)


(*** "indenting" formats ***)

(* indent : ([n:] int) -> format -> format *)
(* When applied to EMPTY, produces EMPTY
 * The resulting format is soft-indented n _additional_ spaces,
 *   i.e. indents an additional n spaces iff following a line break with its indentation. *)
fun indent (n: int) (fmt: format) =
    (case fmt
       of F.EMPTY => F.EMPTY
        | _ => F.INDENT (n, fmt))

(* styled : S.style * format -> format *)
fun styled (style: S.style) (format: format) = F.STYLE (style, format)

end (* top local *)
end (* structure Formatting *)

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

   8.4: basicBlock -> block, alignedBlock -> aBlock, and other renamings: see Version 8.4
   changes note at beginning of this file. Some of these changes were suggested by
   JHR. Thinking about separating the render and printing functions into a separate
   structure and possilby parameterizing wrt a "device" record that would contain printing
   functions for strings, spaces, and newlines, and possibly the lineWidth parameter [See Version 9.1].
   [DBM: 2023.3.1]
*)
