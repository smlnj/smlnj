(* sml/Dev/pp/new/new71/format.sml *)

(* Version 7
 * -- added memoization for block measure to speed up the measure function
 * -- The prettyprinter is partitioned into a number of structures and signatures: Format, Measure, Render, NewPrettyPrint.
 * -- The Format structure defines the types, related to formats: format, separator, element, bindent. 
 *    These types defined as datatypes and are therefore concrete within the Prettyprinter modules, but format
 *    becomes abstract when it is re-exported from NewPP.
 *
 * Version 7.1
 * -- add alignment datatype (partially replacing separator)
 *
 * Version 7.4
 * -- name changes: "separator" --> "break", and SEP --> BRK (element constructor)
 * -- name change: SBLOCK --> BLOCK (format constructor)
 * -- add NullBreak break constructor (virtual break for C alignment)
 *
 * Version 8
 * -- eliminate bindent, replaced it with HINDENT and SINDENT format constructors
 *)

(* Abbreviations:
   WL = Hughes-Walder-Leijen pretty printer, e.g. Prettyprinter 1.7.1 in the Haskell Hackage library *)

 (* Format: no signature, exports only datatypes alignment, break, and format *)

structure Format = 
struct

(* ================================================================================
 *  BLOCKS, elements, separators
 * ================================================================================ *)

(* type break:
 * Breaks are used in two ways:
 *
 *  (1) as explicit separators (turned into "elements" by the BRK constructor) in basic blocks
 *      (BLOCK constructor);
 *  (2) to effect the "alignment" (horizontal, vertical, or packed) of _aligned_ blocks (ABLOCK constructor).
 *
 * There are four forms of breaks:
 *
 *  (1) SoftLine n: soft line breaks that translate to n spaces if not triggered;
 *  (2) HardLine: hard line breaks that are unconditional and are followed by indentation to the current blm;
 *  (3) Space n: translates to n spaces, without producing a newline+indent.
 *  (4) NullBreak: has no effect (no spaces, no newline+indent)
 *
 * In a "typical" basic block the breaks would occur only _between_ the FMT elements, and
 * only one break would be occur between any two FMT elements. If two FMT elements are adjacent,
 * the "do nothing" break NullBreak, in principle, be inserted between them.
 * For the "aligned" block constructor ABLOCK, only the format elements are specified,
 * while the alignment attribute determines an implicit break successive formats.
 * The NullBreak is not essential, since we can create a basic block containing no breaks
 * between format elements, but it is convenient since it can be used as the implicit break 
 * between formats in compact (alignment = C) aligned blocks (see cblock, ccat in NewPrettyPrint).
 *)

(* alignment: alignment mode for "aligned" blocks *)
 
datatype alignment  (* the alignment property of "aligned" blocks *)
  = H  (* Horizontal alignment, with implicit single space (Space 1) breaks between format components *)
  | V  (* Vertical alignment, with implicit hardline breaks (HardLine) between format components *)
  | P  (* Packed alignment, with implicit softline (SoftLine 1) breaks between format components *)	
  | C  (* Compact alignment, with implicit NullBreak between format components, hence also horizontal *)

(* breaks: used to separate format elements of a special block and categorize alignment in aligned blocks
 *   breaks are concerned only with "formatting" and do not contribute any content *)
datatype break
  = HardLine         (* hard line break; rendered as a newline + indent to current block left margin (blm) *)
  | SoftLine of int  (* soft line break (n >= 0); rendered as n spaces, if the line break is not triggered;
		      * if the line break is triggered because the following format does not fit in remaining
                      * line space, renders as newline + indent to current block left margin (blm) *)
  | Space of int     (* spaces (n >= 0); rendered as n spaces *)
  | NullBreak        (* The default break that does nothing, i.e. neither breaks a line nor inserts spaces.
		      * This is essentially equivalent to Space 0, but included for logical "completeness". *)
		
(* type format: the type representing "formats" (named "documents" or doc in Hughes-Wadler, WL)
 *  There is an atomic "EMPTY" format. This carries no content, and when rendered produces no
 *  output.  It is an identity format for the BLOCK compositions, meaning that it will be absorbed
 *  (i.e. dropped).  Also BLOCK [] and BLOCK [EMPTY] reduce to EMPTY during construction.
 *
 *  There are three basic forms of formats that carry the "contents" of a format:
 *
 *  -- TEXT: atomic formats containing a string;
 *  -- ABLOCK: _aligned_ compound blocks of formats, with implicit breaks separating the formats.
 *  -- BLOCK: _basic_ compound blocks of formats, possibly with explicit breaks among the formats.
 *
 *  TEXT formats can be thought of as atomic blocks.
 *  There are also two kinds of _compound_ blocks:
 *
 *  -- BLOCK: basic blocks, with arbitrary break elements interleaved with format elements.
 *     These are essentually "manually constructed" blocks that allow use of heterogeneous breaks.
 *
 *  -- ABLOCK: _aligned_ blocks with an alignment attribute. The alignment can be
 *       H: horizontal (Space break),
 *       V: vertical (HardLine),
 *       P: packed (SoftLine),
 *       C: compact (NullBreak).
 *
 *  Formats can be "indented" or not, with indented blocks constructed using the HINDENT or SINDENT format
 *  constructore.
 *  -- A "hard" indented format is formed by applying HINDENT (HINDENT (n, fmt)). It always starts on a new line
 *     (newline+indent), generating its own newline+indent if it does not immediately follow a newline+indent.
 *     The context block's blm is incremented by n to define the indentation of the format.
 *  -- A "soft" indented format is formed by applying SINDENT (SINDENT (n, fmt)). The argument format is indented
 *     only if it follows a newline+indent that "enables" it. If it does not follow a newline+indent, it is
 *     rendered as an ordinary, non-indented format starting, as usual, at the current column.
 *  -- An "indented format" is a format constructed with HINDENT or SINDENT.
 *  -- A non-indented format is rendered beginning at the current column (which defines its blm if it is a
 *     compound block) without introducing a newline+indent (though it may immediately follow a newline+indent).
 *  -- Format indentations (HINDENT or SINDENT) are the only things that change indentation, and they change
 *     indentation through indenting a whole format (establishing a new blm for formats that are blocks).
 *  -- HINT: If we want a format to start with a (hard) newline but no additional indentation (i.e. indented
 *     to the current blm), we can make it a "hard" indented format with 0 indentation, e.g., HINDENT (0, fmt).
 *  -- In "flat" rendering mode (function Render.flatRender), newlines and indentations are cancelled for
 *     indented formats.
 *  -- Blocks  have an implicit blm (block left margin) which is determined during rendering;
 *     it is defined as the cc (current column) at the point where the block is "entered".
 *
 *  There is also a FLAT format constructor. This is used to specify that a format should be rendered
 *  (and measured) _flat_, without newlines (as though rendered on an infinite line with hard line breaks
 *  rendered as spaces.
 *
 *  Finally, there is a "conditional" format constructor, ALT (format1, format2). This renders as format1
 *  if it fits, and otherwise renders format2.
 *
 *    Note: See the definition of the "tryFlat" function (in NewPrettyPrint) to see an example
 *    of how FLAT and ALT can be used together.
 *
 *    Note: ALT and FLAT are related to Wadler's Union documents and the FlatAlt constructor in recent
 *    Wadler-Leijen prettyprinters. In a format of the form ALT(format1, format2) it will usually be the
 *    case that the alternative formats format1 and format2 have the same content (text strings), but this
 *    is not enforced, and in some cases it may be desired that format2 is an abbreviated version of format1.
 *
 *  Normalization of formats [not implemented]:
 *
 *    (1) The list of elements or formats of an BLOCK or ABLOCK could be "normalized" by merging adjacent
 *    TEXT elements and inserting an "empty" TEXT element between adjacent BRKs and inserting null breaks
 *    of the form BRK (NullBreak) between adjacent FMTs, so that the list contains alternating FMT and SEP elements
 *    with single SEPs occurring as breaks between the FMT elements. Such a normalization would also have
 *    a policy for "merging" adjacent BRK elements in BLOCKs. This normalization is probably not needed.
 *
 *    (2) A BLOCK consisting of one format element could be replaced by that element's format without
 *    changing anything:
 *
 *        BLOCK {elements = [FMT format]}  ==>  format
 *
 *    Such reduction rules could also be used in the "normalization" of formats.
 *
 *    (3) We normalize ABLOCK {formats = nil, ...} to EMPTY.
 *
 *    (4) By convention, we also normalize HINDENT(n, EMPTY) and SINDENT(n, EMPTY) to EMPTY.
 *        (Although it might make sense not simplify these and let HINDENT (n, EMPTY) be rendered
 *        as a newline+indent.)
 *
 *    We assume that in a BLOCK with a single element, that element ought to be a FMT, not a BRK, although
 *    there are conceivable uses for BLOCKs containing only breaks, like two HardLines to produce
 *    a blank line in the layout (e.g. BLOCK {formats = [BRK HardLine, BRK HardLine],...}).
 *
 *  Memoized block measures.
 *    When blocks are constructed, their "flat" measure is memoized in an internal measure: int field.
 *    This avoids possible repeated calculation of a block's measure. The measure field is defined during
 *    the block construction, since its child blocks will have been measured. The measure of a TEXT
 *    block is just the size of the string, and the measure of a FLAT or ALT format is the measure 
 *    of its (first) argument. The calculation of the measure of a format is "static", in the sense that it
 *    is based only on the structure of the format. [Does this constitute a form of "dynamic programming"?]
 *
 * type element:
 *   The type of elements of a basic block (BLOCK), supports heterogeneous breaks interleaved in
 *   any way with formats. A BLOCK contains a list of "elements" instead of just a list of sub-formats
 *   to allow for the explicit specification of break elements, which normally will appear between
 *   the sub-formats in the block's elements list. But we allow the cases where all the elements to are formats
 *   or all the elements are breaks.
 *
 * The formats of an ABLOCK will be reduced, meaning that any EMPTY formats are eliminated. *)

datatype format (* aka "format" *)
  = EMPTY
      (* empty format; rendering this produces no output, identity for format compositions *)
  | TEXT of string
      (* unique form of atomic doc with content*)
  | BLOCK of {elements: element list, measure: int}
      (* "basic" or "ad hoc" blocks with explicit break (BRK) elements interleaved with format (FMT) elements *)
  | ABLOCK of {formats: format list, alignment: alignment, measure: int}
      (* "aligned" blocks, with implicit breaks between formats determined by the alignment *)

  | HINDENT of int * format  (* hard indent the format n spaces *)
  | SINDENT of int * format  (* soft indent the format n spaces *)

  | FLAT of format
      (* render (and measure) the format as flat *)

  | ALT of format * format
      (* render format1 if it fits, otherwise render format2. Note that formats not constrained to have same
       * content! But normally they should, or at least content fmt2 << content fmt1. *)

and element  (* components of special blocks *)
  = BRK of break
  | FMT of format

end (* structure Format *)

(* NOTES:

  (1) CONTEXT or CONTEXTUAL formats of WL -- not implemented

    WL has additional "document" constructors (Column, Next) supporting "render-time" conditional
    formatting, using functions over render-time variables like cc and blm (aka "current
    indentation level") to produce a specialized format to be rendered at this point, thus generating
    formats (documents) on the fly during rendering.

    The problem with these is that it not clear how to "statically" measure the a format
    whose layout is produced using render-time information, like the current column (cc),
    that is not available when measuring the format "statically" (i.e. it is not
    "structural" information intrinsic to the format itself).


  (2) Why don't we merge format and element into one type?

      * Breaks do not carry "content", whereas formats generally do (except for EMPTY).

      * On general principles, we want to use the type system to express differences (in usage, properties, ...)
	whenever possible. Here we want to express the fact that breaks are subsidiary to formats. They are
	elements that are used to construct formats, but they do not acts as formats on their own. Nevertheless,
	it is easy to _coerce_ a break to a format: break => BLOCK [BRK break], since we allow basic blocks that
	contain only formats.

 *)
