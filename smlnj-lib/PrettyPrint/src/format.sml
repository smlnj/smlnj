(* smlnj-lib/PrettyPrint/src/format.sml *)

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
 * -- eliminate iblock and xiblock functions for building indented blocks. Indentation is
 *    performed solely with HINDENT and SINDENT (and functions in NewPrettyPrint derived from
 *    them: hardIndent and softIndent.
 *
 * Version 8.1 [2023.1.2]
 * -- HINDENT and SINDENT merged into single INDENT constructor that renders as SINDENT did.
 * 
 * Version 8.4 [2023.3.1]
 * -- rename Break constructor names: HardLine -> Line, SoftLine -> Soft, NullBreak -> Null
 *)

(* structure Format:
 *   no signature
 *   exports datatypes: alignment, break, element, and format
 *   imports Style
 *)

structure Format =
struct

(* IMPORTS: Style *)

(* datatype alignment: alignment modes for "aligned" blocks *)
datatype alignment  (* the alignment property of "aligned" blocks *)
  = H  (* Horizontal alignment, with implicit single space (Space 1) breaks between format components *)
  | V  (* Vertical alignment, with implicit hardline breaks (Hard) between format components *)
  | P  (* Packed alignment, with implicit softline (Soft 1) breaks between format components *)
  | C  (* Compact alignment, with implicit Null between format components, hence also horizontal *)

(* breaks: used to separate format elements of a special block and categorize alignment in aligned blocks
 *   breaks are concerned only with "formatting" and do not contribute any content *)
datatype break
  = Hard             (* hard line break; rendered as a newline + indent to current block left margin (blm) *)
  | Soft of int      (* soft line break (n >= 0); rendered as n spaces, if the line break is not triggered;
		      * if the line break is triggered because the following format does not fit in remaining
                      * line space, renders as newline + indent to current block left margin (blm) *)
  | Space of int     (* spaces (n >= 0); rendered as n spaces; Space 1 is the break for H alignment *)
  | Null             (* The default break that does nothing, i.e. it neither breaks a line nor inserts spaces.
		      * This is essentially equivalent to Space 0, but included for logical "completeness";
		      * It is the separator break for C alignment. *)

(* datatype format corresponds to doc (short for "document") type in Hughes-Wadler-Leijen *)
datatype format =

  (* format builders *)
    EMPTY
      (* empty format; rendering this produces no output, an identity for format compositions in blocks *)
  | TEXT of string
      (* unique form of atomic doc with content*)
  | BLOCK of {elements: element list, measure: int}
      (* "basic" or "ad hoc" blocks with explicit break (BRK) elements interleaved with format (FMT) elements *)
  | ABLOCK of {formats: format list, alignment: alignment, measure: int}
      (* "aligned" blocks, with implicit breaks between formats determined by the alignment *)

  (* format modifiers *)
  | INDENT of int * format
      (* soft indent the format n spaces, sinilar to Hughes's nest *)
  | FLAT of format
      (* render (and measure) the format as flat *)
  | STYLE of Style.style * format

  (* conditional choice of formats *)
  | ALT of format * format
      (* to render ALT (format1, format2): render format1 if it fits, otherwise render format2.
       * Note that the two formats are not constrained to have same content! But normally they should,
       * or at least content fmt2 should be an abbreviated or approximate version of content fmt1. *)

and element  (* components of special blocks *)
  = BRK of break
  | FMT of format

end (* structure Format *)
