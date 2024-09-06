(* format.sml
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Format =
  struct

    (* rendering styles are specified by application-specific atoms; these are
     * mapped to the device-specific styles by a user-defined mapping.
     *)
    datatype style = STY of string

    (* tokens are bits of text whose size is not determined by the length
     * of the text (e.g., UTF-8 characters; images; etc.)
     *)
    datatype token = TOK of {name : string, measure : int}

    (* datatype alignment: alignment modes for "aligned" blocks *)
    datatype alignment  (* the alignment property of "aligned" blocks *)
      = H  (* Horizontal alignment, with implicit single space (Space 1) breaks between format components *)
      | V  (* Vertical alignment, with implicit hardline breaks (HardLine) between format components *)
      | P  (* Packed alignment, with implicit softline (SoftLine 1) breaks between format components *)
      | C  (* Compact alignment, with implicit NullBreak between format components, hence also horizontal *)

    datatype break
      = Hard           (* hard line break *)
      | Soft of int    (* soft line break; rendered to n spaces when not
                         * triggered; n >= 0
                         *)
      | Space of int    (* non-breakable spaces; n >= 0; Space 0 == NullBreak *)
      | Null            (* A default break that does nothing, i.e., neither breaks
                         * a line nor inserts spaces. `NullBreak` is essentially
                         * equivalent to Space 0, but included for logical "completeness",
			 * and also eliminates the need for a break option in some
                         * places (alignmentToBreak).
                         *)

    (* datatype format corresponds to doc (short for "document") type in Hughes-Wadler-Leijen *)
    datatype format
      (* format builders *)
      = EMPTY
          (*< empty format; rendering this produces no output, identity for format compositions *)
      | TEXT of string
          (*< unique form of atomic doc with content*)
      | TOKEN of token
      | STYLE of style * format
      | BLOCK of {elements : element list, measure : int}
          (* blocks with explicit break (BRK) elements interleaved with format (FMT) elements *)
      | ABLOCK of {formats : format list, align : alignment, measure : int}
          (* "aligned" blocks, with implicit breaks between formats determined by the alignment *)

      (* format modifiers *)
      | INDENT of int * format
          (* soft indent the format n spaces, sinilar to Hughes's nest *)
      | FLAT of format
          (* render (and measure) the format as flat *)

      (* conditional choice of formats *)
      | ALT of format * format
          (* render format1 if it fits, otherwise render format2. Note that formats
           * not constrained to have same content! But normally they should, or at
           * least content fmt2 << content fmt1.
           *)

    and element  (* components of special blocks *)
      = BRK of break
      | FMT of format

  end (* structure Format *)
