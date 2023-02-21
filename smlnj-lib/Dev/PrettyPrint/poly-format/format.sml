(* format.sml
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Format =
  struct

    datatype 'sty token = TOK of {txt : Atom.atom, sz : int, style : 'sty}

    (* datatype alignment: alignment modes for "aligned" blocks *)
    datatype alignment  (* the alignment property of "aligned" blocks *)
      = H  (* Horizontal alignment, with implicit single space (Space 1) breaks between format components *)
      | V  (* Vertical alignment, with implicit hardline breaks (HardLine) between format components *)
      | P  (* Packed alignment, with implicit softline (SoftLine 1) breaks between format components *)
      | C  (* Compact alignment, with implicit NullBreak between format components, hence also horizontal *)

    datatype break
      = Newline         (* hard line break *)
      | Break of int    (* soft line break; rendered to n spaces when not
                         * triggered; n >= 0
                         *)
      | Space of int    (* non-breakable spaces; n >= 0; Space 0 == NullBreak *)
      | NullBreak       (* A default break that does nothing, i.e., neither breaks
                         * a line nor inserts spaces. `NullBreak` is essentially
                         * equivalent to Space 0, but included for logical "completeness",
			 * and also eliminates the need for a break option in some
                         * places (alignmentToBreak).
                         *)

    (* datatype format corresponds to doc (short for "document") type in Hughes-Wadler-Leijen *)
    datatype 'sty format
      (* format builders *)
      = EMPTY
          (* empty format; rendering this produces no output, identity for format compositions *)
      | TEXT of string
          (* unique form of atomic doc with content*)
      | TOKEN of 'sty token
      | STYLE of 'sty * 'sty format
      | BLOCK of {content : 'sty element list, sz : int}
          (* blocks with explicit break (BRK) elements interleaved with format (FMT) elements *)
      | ABLOCK of {content : 'sty format list, align : alignment, sz : int}
          (* "aligned" blocks, with implicit breaks between formats determined by the alignment *)

      (* format modifiers *)
      | INDENT of int * 'sty format
          (* soft indent the format n spaces, sinilar to Hughes's nest *)
      | FLAT of 'sty format
          (* render (and measure) the format as flat *)

      (* conditional choice of formats *)
      | ALT of 'sty format * 'sty format
          (* render format1 if it fits, otherwise render format2. Note that formats
           * not constrained to have same content! But normally they should, or at
           * least content fmt2 << content fmt1.
           *)

    and 'sty element  (* components of special blocks *)
      = BRK of break
      | FMT of 'sty format

  end (* structure Format *)
