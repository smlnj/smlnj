(* pretty-print.sig
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature PRETTY_PRINT =
  sig

  (* types *)

    (* rendering styles are specified by application-specific atoms; these are
     * mapped to the device-specific styles by a user-defined mapping.
     *)
    type style = Atom.atom

    (* tokens are bits of text whose size is not determined by the length
     * of the text (e.g., UTF-8 characters; images; etc.)
     *)
    datatype token = TOK of {txt : Atom.atom, sz : int}

    (* specifies a formated term; the type variable is an application-specific
     * `style`.
     *)
    type format

    (* the alignment property of "aligned" blocks *)
    datatype alignment
      = H  (* Horizontal alignment, with implicit single space breaks between
            * format components, unbreakable
            *)
      | V  (* Vertical alignment, with implicit hardline separators between
            * format components
            *)
      | P  (* Packed alignment, with implicit softline separators between
            * format components
            *)
      | C  (* compact, no separators between block format elements, unbreakable *)

    (* used to separate doc elements of a block; conditional and
     * unconditional line breaks
     *)
    datatype break
      = Hard            (* hard line break *)
      | Soft of int     (* soft line break; rendered to n spaces when not
                         * triggered; n >= 0
                         *)
      | Space of int    (* non-breakable spaces; n >= 0; Space 0 == Null *)
      | Null            (* A default break that does nothing, i.e., neither breaks
                         * a line nor inserts spaces. `Null` is essentially
                         * equivalent to Space 0, but included for logical "completeness",
			 * and also eliminates the need for a break option in some
                         * places (alignmentToBreak).
                         *)

    datatype element
      = BRK of break   (* breaks are atomic and do not contain content *)
      | FMT of format

  (* Basic formats and format building operations: *)

    (* the empty format, which is the identity for composition *)
    val empty : format
    (* format ASCII text *)
    val text  : string -> format
    (* format an abstract text item (e.g., UTF8 characters) *)
    val token : token -> format
    (* format with style *)
    val style : style -> format -> format
    (* lift a `toString` function to a function for formating values as `text`. *)
    val lift : ('a -> string) -> 'a -> format

    (* block-building functions *)

    (* format a block of text formed from a list of elements *)
    val block : element list -> format

    (* a block of text with a specific alignment *)
    val aBlock : alignment * format list -> format

    (* building blocks, basic and aligned; empty format args are absorbed and
     * empty argument list produces empty format
     *)

    val hBlock : format list -> format
    val vBlock : format list -> format
    val pBlock : format list -> format
    val cBlock : format list -> format

  (* wrapping or enclosing formats *)

    val enclose : {left: format, right: format} -> format -> format
        (* concatenates front and back to the front, respecively back, of the format *)

  (* composing lists of formats *)

    (* `sequence a break fmts`
     * inserts `break` between constituent `fmts` and aligns by `a`
     *)
    val sequence : {align : alignment, sep : format}
          -> format list
          -> format

    (* aligned sequence formatters, first argument is sep format *)
    val hSequence : format -> format list -> format  (* = sequence H *)
    val vSequence : format -> format list -> format  (* = sequence V *)
    val pSequence : format -> format list -> format  (* = sequence P *)
    val cSequence : format -> format list -> format  (* = sequence C *)

  (* formating of lists of values of arbitrary type *)

    (* `sequenceWithMap {align=algn, sep=sep, fmt=fmt} xs` is equivalent to
     * `sequence {align=algn, sep=sep} (map fmt xs)`
     *)
    val sequenceWithMap : {
            align : alignment,
            sep : format,
            fmt : 'a -> format
          } -> 'a list -> format

    (* `closedSequenceWithMap {align=algn, left=l, sep=sep, right=r, fmt=fmt} xs`
     * is equivalent to `enclose {left=l, right=r} (sequence {align=algn, sep=sep} (map fmt xs))`
     *)
    val closedSequenceWithMap : {
            align : alignment,
            left : format,
            sep : format,
            right : format,
            fmt : 'a -> format
          } -> 'a list -> format

  (** vertical lists with labels **)

    (* produces a vertical list where each line has a label prepended.  The string
     * `first` is used as the label for the first line and `rest` is used as the
     * label for subsequent lines.  If the labels have unequal sizes, then the shorter
     * label is padded on the left so that the labels right edge is aligned.
     *)
    val vLabeledList : {first : string, rest : string} -> format list -> format

    (* like `vLabeledList` except that if the labels have unequal sizes, then the shorter
     * label is padded on the right so that the labels are aligned on the left.
     *)
    val vLabeledListLAlign : {first : string, rest : string} -> format list -> format

    (* like `vLabeledList` except that if the labels have unequal sizes, then the shorter
     * label is padded on the left so that the labels are aligned on the right.
     *)
    val vLabeledListRAlign : {first : string, rest : string} -> format list -> format

  (* indenting formats *)

    (* indent n empty ==> empty; indent n fmt ==> INDENT (n, frmt) *)
    val indent : int -> format -> format

  (* Conditional formats: *)

    (* if the format fits flat, then render it flat, otherwise render it normally *)
    val tryFlat : format -> format

    (* if the first format fits flat, use it, otherwise render the second format,
     * NOTE: the two argument formats may not have the same content! But
     * usually they should!
     *)
    val alt : format * format -> format

    (* acts as hBlock if it fits, otherwise as vBlock *)
    val hvBlock : format list -> format

  end (* end PRETTY_PRINT *)
