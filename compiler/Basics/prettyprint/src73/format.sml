(* sml/Dev/pp/new/new71/format.sml *)

(* Version 7
 * -- added memoization for block measure to speed up the measure function
 * -- The prettyprinter is partitioned into a number of structures and signatures: Format, Measure, Render, NewPP.
 * -- The Format structure defines the types, related to formats: format, separator, element, bindent. 
 *    These types defined as datatypes and are therefore concrete within the Prettyprinter modules, but format
 *    becomes abstract when it is re-exported from NewPP.
 * Version 7.1
 * -- add alignment datatype (partially replacing separator
 *)

(* Abbreviations:
   WL = Walder-Leijen pretty printer, e.g. Prettyprinter 1.7.1 in the Haskell Hackage library *)

 (* Format: no signature, exports only datatypes and the one value "alignmentSeparator", which
    could/should be defined in Render?  *)
structure Format = 
struct

(* ================================================================================
 *  BLOCKS, elements, separators
 * ================================================================================ *)

(* type separator:
 * Separators are used in two ways:
 *
 *  (1) as explicit separators (turned into "elements" by the SEP constructor) in special blocks
 *      (SBLOCK constructor);
 *  (2) to determine the "alignment" (horizontal, vertical, or packed) of _aligned_ blocks (BLOCK constructor).
 *
 * There are three forms of separators:
 *
 *  (1) SoftLine n: soft line breaks that translate to n spaces if not triggered;
 *  (2) HardLine: hard line breaks that are unconditional and are followed by indentation to the current blm;
 *  (3) Space n: translates to n spaces, without producing a newline+indent.
 *
 * In a "typical" special block the separators would occur only _between_ the FMT elements, and
 * only one separator would be occur between any two FMT elements. If two FMT elements are adjacent,
 * a "do nothing" separator like (Space 0) could, in principle, be inserted between them.
 * For the "aligned" block constructor BLOCK, only the format elements are specified,
 * as well as an implicit separator to be rendered between successive format elements.
 * A "null" separator (i.e. "no separator") can be represented by Space 0, but this is not really needed,
 * since we can create a special block with no separators at all, containing only formats. This allows us to
 * "tightly" concatenate a sequence of formats so that their layouts will abut one another (see the cblock and
 * ccat functions in NewPP).
 *)

(* alignment: alignment mode for "aligned" blocks *)

datatype alignment  (* the alignment property of "aligned" blocks *)
  = H  (* Horizontal alignment, with implicit single space separtors between format components *)
  | V  (* Vertical alignment, with implicit hardline separtors between format components *)
  | P  (* Packed alignment, with implicit softline separtors between format components *)	
  | C  (* Compact alignment, horizontal with no separators between format components *)

(* separators:  used to separate format elements of a special block and categorize alignment in aligned blocks
 *   separators are concerned only with "formatting" and do not contribute any content *)
datatype separator
  = HardLine         (* hard line break; rendered as a newline + indent to current block left margin (blm) *)
  | SoftLine of int  (* soft line break (n >= 0); rendered as n spaces, if the line break is not triggered;
		      * if the line break is triggered because the following format does not fit in remaining
                      * line space, renders as newline + indent to current block left margin (blm) *)
  | Space of int     (* spaces (n >= 0); rendered as n spaces *)
		
(* block indents: specify the indentation behavior on entering a block *)
datatype bindent
  = NI          (* No Indent: the block begins at the current column on the current line *)
  | HI of int	(* Hard Indent: always taken, supplying its own newline+indent if necessary; n >= 0 *)
  | SI of int	(* Soft Indent: taken only if the block is preceded by a newline+indent; n >= 0 *)

(* type format: the type representing "formats" (named "documents" or doc in Hughes-Wadler, WL)
 *  There is an atomic "EMPTY" format. This carries no content, and when rendered produces no
 *  output.  It is an identity format for the BLOCK compositions, meaning that it will be absorbed
 *  (i.e. dropped).  Also BLOCK [] and BLOCK [EMPTY] reduce to EMPTY during construction.
 *
 *  There are three basic forms of formats that carry the "contents" of a format:
 *
 *  -- TEXT: atomic formats containing a string;
 *  -- BLOCK: _aligned_ compound blocks of formats, with implicit "separators" between the formats.
 *  -- SBLOCK: _special_ compound blocks of formats, possibly with explicit "separators" among the formats.
 *
 *  There are two kinds of blocks:
 *
 *  -- BLOCK: _aligned_ blocks with separator and bindent attributes. The alignment can be horizontal
 *     (Space separator), vertical (HardLine), or packed (SoftLine).
 *
 *  -- SBLOCK: special blocks, with arbitrary separators between format elements, and an bindent attribute;
 *     These are essentually "manually constructed" blocks that allow use of heterogeneous separators.
 *
 *  Blocks can be "indented" or not, as specified by the bindent field of the block record.
 *  -- A "hard" indented block (with bindent - HI n) always starts on a new line (after a line break),
 *     generating its own newline+indent if necessary, i.e. if it does not immediately follow a newline+indent.
 *     The parent block's blm is incremented by n to define the blm of the indented block.
 *  -- A "soft" indented block (with bindent = SI n) is indented only if it follows a newline+indent
 *     that "enables" it. If it does not follow and newline+indent, it is rendered as an ordinary, non-indented
 *     block, with its blm defined by the current column.
 *  -- An "indented block" is a block with bindent = HI n or bindent = SI n (n >= 0).
 *  -- A non-indented block (with bindent = NI) is rendered at the current column (which defines its blm)
 *     without introducing a newline+indent (though it may immediately follow a newline+indent).
 *  -- Block indentations (HI n or SI n) are the only things that change indentation, and they change
 *     indentation through defining a new blm for the block.
 *  -- HINT: If we want a block to start with a (hard) newline but no additional indentation (i.e. indented
 *     to the current blm, we can make it a "hard" indented block with 0 indentation, e.g., hiblock (HI 0) fmts.
 *  -- In "flat" rendering mode (function Render.flatRender), newlines and indentations are cancelled for
 *     indented blocks.
 *  -- Blocks  have an implicit blm (block left margin) which is determined during rendering;
 *     it is the cc (current column) at the point where the block is "entered".
 *
 *  There is also a FLAT format constructor. This is used to specify that a format should be rendered
 *  (and measured) _flat_, without newlines (as though rendered on an infinite line with hard line breaks
 *  rendered as spaces.
 *
 *  Finally, there is a "conditional" format constructor, ALT (format1, format2). This renders as format1
 *  if it fits, and otherwise renders format2.
 *
 *    Note: See the definition of the "tryFlat" function (in NewPP) to see an example of how FLAT and ALT
 *    can be used together.
 *
 *    Note: ALT and FLAT are related to Wadler's Union documents and the FlatAlt constructor in recent
 *    Wadler-Leijen prettyprinters. In a format of the form ALT(format1, format2) it will usually be the
 *    case that the alternative formats format1 and format2 have the same content (text strings), but this
 *    is not enforced, and in some cases it may be desired that format2 is an abbreviated version of format1.
 *
 *  Normalization of formats [not implemented]:
 *
 *    (1) The list of elements or formats of an SBLOCK or BLOCK could be "normalized" by merging adjacent
 *    TEXT elements and inserting an "empty" TEXT element between adjacent SEPs and inserting "empty" separators
 *    of the form SEP (Space 0) between adjacent FMTs, so that the list contains alternating FMT and SEP elements
 *    with single SEPs occurring as separators between the FMT elements. Such a normalization would also have
 *    a policy for "merging" adjacent SEP elements in SBLOCKs. This normalization is probably not needed.
 *
 *    (2) A non-indenting BLOCK with only one element could be replaced by that element without changing anything:
 *
 *        SBLOCK {indent = NI, elements = [FMT format]}  ==>  format
 *
 *    Such reduction rules could also be used in the "normalization" of formats.
 *
 *    (3) If we add an EMPTY format constructor, representing an empty format that produces no output, we might
 *    normalize BLOCK {formats = nil, ...} to EMPTY. But it is not clear how an empty indented block should behave:
 *    should it produce the newline+indent even if its content is empty?
 *
 *    We assume that in a SBLOCK with a single element, that element ought to be a FMT, not a SEP, although
 *    there are conceivable uses for SBLOCKs containing only separators, like two HardLines to produce
 *    a blank line in the layout (e.g. SBLOCK {formats = [SEP HardLine, SEP HardLine],...}).
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
 *   The type of elements of a special block (SBLOCK), which can use heterogeneous separators interleaved in
 *   any way with formats. An SBLOCK contains a list of "elements" instead of just a list of sub-formats
 *   to allow for the explicit specification of separator elements, which normally will appear between
 *   the sub-formats in the block's elements list. But it is possible for the elements to consist only of
 *   formats, or even only of separators. *)

(* The formats of an ABLOCK will be reduced, meaning that there will be no empty formats. *)

datatype format (* aka "format" *)
  = EMPTY
      (* empty format; rendering this produces no output, identity for format compositions *)
  | TEXT  of string
      (* unique form of atomic doc with content*)
  | SBLOCK of {elements: element list, bindent: bindent, measure: int}
      (* "special" or "ad hoc" block with explicit separator (SEP) elements mixed with format (FMT) elements *)
  | ABLOCK of 
      {formats: format list, alignment: alignment, bindent: bindent, measure: int}
      (* "normal" blocks, where alignment (H, V, P) is determined by the choice of the separator *)
  | FLAT of format
      (* render (and measure) the format as flat *)
  | ALT of format * format
      (* render format1 if it fits, otherwise render format2. Note that formats not constrained to have same
       * content! But normally they should, or at least content fmt2 << content fmt1. *)

and element  (* components of special blocks *)
  = SEP of separator
  | FMT of format

(* alignmentSeparator : alignment -> separator option
 * -- only value exported from Format (internal use only, in Render, NewPP) *)
fun alignmentSeparator H = SOME (Space 1)
  | alignmentSeparator V = SOME (HardLine)
  | alignmentSeparator P = SOME (SoftLine 1)
  | alignmentSeparator C = NONE

end (* structure Format *)

(* NOTES:

  (1) CONTEXT or CONTEXTUAL formats (not implemented). 

    WL has "documenent" constructors (Column, Next) supporting "render-time" conditional
    formatting, using functions over render-time variables like cc and blm (or "current
    indentation level") to produce a specialized format to be rendered at this point. The
    problem with these is that it not clear how to "statically" measure the a format whose
    layout is produced using render-time information, like the current column (cc), that is
    not available when measuring the format "statically" (i.e. it is not "structural" information
    intrinsic to the format itself).

 *)
