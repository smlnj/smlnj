(* formatting.sig
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * The main interface of the new Prettyprinter.
 *)

signature FORMATTING =
  sig

  (* types *)

    (* rendering styles are specified by application-specific strings; these are
     * mapped to the device-specific styles by a user-defined mapping.
     *)
    datatype style = STY of string

    (* make a style from a string; use this function instead of the `STY` constructor.
     * since we may want to change the representation of styles to use hashed
     * strings.
     *)
    val style : string -> style

    (* the default style, which is `style ""` *)
    val defaultStyle : style

    (* tokens are bits of text whose measure is not determined by the length
     * of the text (e.g., UTF-8 characters; images; etc.)
     *)
    datatype token = TOK of {name : string, measure : int}

    (* specifies a formated term *)
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
    (* format a token, which is a string whose measure is not determined
     * by the length of the text (e.g., UTF-8 characters; images; etc.)
     *)
    val token : token -> format
    (* format with style *)
    val styled : style -> format -> format
    (* lift a `toString` function to a function for formating values as `text`. *)
    val lift : ('a -> string) -> 'a -> format
    (* == `lift Int.toString` *)
    val int : int -> format
    (* as SML string literal
     * == `enclose {front=text "\"", back = "\""} o (lift String.toString)`
     *)
    val string  : string -> format
    (* as SML character literal *)
    val char    : char -> format
    (* == `lift Bool.toString` *)
    val bool    : bool -> format
    (* == `lift Atom.toString` *)
    val atom : Atom.atom -> format

    (* block-building functions, corresponding to SBLOCK and BLOCK data constructors *)

    (* format a block of text formed from a list of elements, which may include
     * explicit breaks.
     *)
    val block : element list -> format

    (* The alignement parameter determines the implicit break that occurs between
     * formats in the list:
     *
     *    - `H` -> `Space 1`
     *    - `P` -> `Soft 1`
     *    - `V` -> `Hard`
     *    - `C` -> `Null`
     *
     * empty argument list produces empty format and empty format elements
     * are dropped, so, for example `ablock [empty, empty]` ==> `empty`.
     *)
    val aBlock : alignment * format list -> format

    (* xBlock: functions (for x = p, h, v, c) for building aligned blocks
     * with a given alignment, the empty format acts like an identity element
     * for all these format concatenation operators, in that it does not
     * contribute anything to the result, and the associated implicit breaks
     * separating empty formats from other elements are also dropped. Also,
     * xBlock [fmt] ==> fmt.
     *)

    (* combines a list of formats in an H-aligned block, with an implicit
     * single space (`Space 1`) between them.
     *)
    val hBlock : format list -> format
    (* combines a list of formats into a P-aligned (packed) block, with an
     * implicit soft line break (`Soft 1`) between them
     *)
    val pBlock : format list -> format
    (* combines a list of formats in an V-aligned block, with an implicit
     * hard line break (`Hard`) between them.
     *)
    val vBlock : format list -> format
    (* combines a list of formats in a C-aligned block, with no break
     * (or, implicitly, `Null`) between them.
     *)
    val cBlock : format list -> format

    (* wrapping or enclosing formats *)

    val enclose : {front: format, back: format} -> format -> format
        (* concatenates front and back to the front, respecively back, of the format *)

    (* == `enclose {front=text "(", back=text ")"}` *)
    val parens : format -> format

    (* == `enclose {front=text "[", back=text "]"}` *)
    val brackets : format -> format
        (* like parens, but with lbracket and rbracket *)

    (* == `enclose {front=text "{", back=text "}"}` *)
    val braces : format -> format

    (* == `enclose {front=text "<", back=text ">"}` *)
    val angleBrackets : format -> format

    (* append a newline to the format -- normally used for "top-level" printing *)
    val appendNewLine : format -> format

  (* composing lists of formats *)

    (* `sequence a break fmts`
     * inserts `break` between constituent `fmts` and aligns by `a`
     *)
    val sequence : alignment -> format -> format list -> format

    (* aligned sequence formatters, first argument is separator *)
    val hSequence : format -> format list -> format  (* == `sequence H` *)
    val vSequence : format -> format list -> format  (* == `sequence V` *)
    val pSequence : format -> format list -> format  (* == `sequence P` *)
    val cSequence : format -> format list -> format  (* == `sequence C` *)

    (* formats as a SML tuple
     * == `parens o (pSequence (text ","))`
     *)
    val smlTuple : format list -> format

    (* formats as a SML list
     * == `brackets o (pSequence (text ","))`
     *)
    val smlList : format list -> format

    (* `smlOption NONE` == `text "NONE"`
     * `smlOption (SOME f)` == `cBlock[text "SOME(", f, text ")"]`
     *)
    val smlOption : format option -> format

  (* formating of lists of values of arbitrary type *)

    (* `sequenceWithMap {align=algn, sep=sep, fmt=fmt} xs` is equivalent to
     * `sequence {align=algn, sep=sep} (map fmt xs)`
     *)
    val sequenceWithMap : {
            align : alignment,
            sep : format,
            fmt : 'a -> format
          } -> 'a list -> format

    (* `closedSequenceWithMap {align=algn, front=l, sep=sep, back=r, fmt=fmt} xs`
     * is equivalent to `enclose {front=l, back=r} (sequence {align=algn, sep=sep} (map fmt xs))`
     *)
    val closedSequenceWithMap : {
            align : alignment,
            front : format,
            sep : format,
            back : format,
            fmt : 'a -> format
          } -> 'a list -> format

  (** vertical lists with labels **)

    (* `label lab fmt` prepends the string to the format as a label.  It is equivalent
     * to the expression `hBlock [text lab, fmt]`
     *)
    val label : string -> format -> format

    (* produces a vertical list where each line has a label prepended.  The string
     * `first` is used as the label for the first line and `rest` is used as the
     * label for subsequent lines.
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

    (* a few "punctuation" characters as formats *)
(** QUESTION: are these really worth the extra bloat to the signature? [JHR] *)
    val comma : format     (* text "," *)
    val colon : format     (* text ":" *)
    val semicolon : format (* text ";" *)
    val period : format    (* text "." *)
    val lparen : format    (* text "(" *)
    val rparen : format    (* text ")" *)
    val lbracket : format  (* text "[" *)
    val rbracket : format  (* text "]" *)
    val lbrace : format    (* text "{" *)
    val rbrace : format    (* text "}" *)
    val langle : format    (* text "<" *)
    val rangle : format    (* text ">" *)
    val equal : format     (* text "=", an honorary punctuation mark *)

  end (* end PRETTY_PRINT *)
