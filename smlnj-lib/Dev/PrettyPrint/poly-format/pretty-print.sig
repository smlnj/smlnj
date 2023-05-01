(* pretty-print.sig
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature PRETTY_PRINT =
  sig

  (* types *)

    (* an abstract piece of text *)
    type 'sty token

    (* specifies a formated term; the type variable is an application-specific
     * `style`.
     *)
    type 'sty format

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

    datatype 'sty element
      = BRK of break   (* breaks are atomic and do not contain content *)
      | FMT of 'sty format

  (* Basic formats and format building operations: *)

    (* the empty format, which is the identity for composition *)
    val empty : 'sty format
    (* format ASCII text *)
    val text  : string -> 'sty format
    (* format an abstract text item (e.g., UTF8 characters) *)
    val token : 'sty token -> 'sty format
    (* format with style *)
    val style : 'sty -> 'sty format -> 'sty format
    (* lift a `toString` function to a function for formating values as `text`. *)
    val lift : ('a -> string) -> 'a -> 'sty format

    (* block-building functions *)

    (* format a block of text formed from a list of elements *)
    val block : 'sty element list -> 'sty format

    (* a block of text with a specific alignment *)
    val alignedBlock : alignment * 'sty format list -> 'sty format

    (* building blocks, basic and aligned; n-ary versions taking lists, empty format args are absorbed
     *   empty argument list produces empty format *)

    val hBlock : 'sty format list -> 'sty format
    val vBlock : 'sty format list -> 'sty format
    val pBlock : 'sty format list -> 'sty format
    val cBlock : 'sty format list -> 'sty format

  (* wrapping or enclosing formats, plus appending newlines and prepending labels *)

    val enclose : {left: 'sty format, right: 'sty format} -> 'sty format -> 'sty format
        (* concatenates front and back to the front, respecively back, of the format *)

  (* composing lists of formats *)

    (* `sequence a break fmts`
     * inserts `break` between constituent `fmts` and aligns by `a`
     *)
    val sequence : {align : alignment, sep : 'sty format}
          -> 'sty format list
          -> 'sty format

    (* aligned sequence formatters, first argument is sep format *)
    val hSequence : 'sty format -> 'sty format list -> 'sty format  (* = sequence H *)
    val vSequence : 'sty format -> 'sty format list -> 'sty format  (* = sequence V *)
    val pSequence : 'sty format -> 'sty format list -> 'sty format  (* = sequence P *)
    val cSequence : 'sty format -> 'sty format list -> 'sty format  (* = sequence C *)

  (* formating of lists of values of arbitrary type *)

    val sequenceWithMap : {
            align : alignment,
            sep : 'sty format,
            fmt : 'a -> 'sty format
          } -> 'a list -> 'sty format

    val closedSequenceWithMap : {
            align : alignment,
            left : 'sty format,
            sep : 'sty format,
            right : 'sty format,
            fmt : 'a -> 'sty format
          } -> 'a list -> 'sty format

  (* indenting formats *)

    val indent : int -> 'sty format -> 'sty format
        (* indent n empty ==> empty; indent n fmt ==> INDENT (n, frmt) *)

  (* Conditional formats: *)

    val tryFlat : 'sty format -> 'sty format
	(* if the format fits flat, then render it flat, otherwise render it normally *)

    val alt : 'sty format * 'sty format -> 'sty format
	(* if the first format fits flat, use it, otherwise render the second format,
	   NOTE: the two argument formats may not have the same content! But usually they should! *)

    val hvBlock : 'sty format list -> 'sty format
	(* acts as hblock if it fits, otherwise as vblock *)

  end (* end PRETTY_PRINT *)
