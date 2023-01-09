(* smlnj-lib/PRETTYPRINT/src/prettyprint.sig *)

(* Version 8.1 [2023.1.2]
 *  -- hardIndent replaced by breakIndent with a different behavior (uncondition line break preceeding indentation)
 *
 * Version 8.
 *  -- Eliminated bindent type, xiblock functions
 *  -- added HINDENT, SINDENT format constructors
 *
 * Version 7.
 *  -- The main interface of the new Prettyprinter.
 *  -- New: memoized measure for blocks (does not alter NEW_PP signature)
 *
 * Version 7.4
 *  -- signature NEW_PP --> NEW_PRETTYPRINT
 *  -- sblock --> block; siblock --> iblock; separator --> break; SEP --> BRK
 *  -- added: vHeaders, vHeaderFormats (from NEW_PPUTIL)
 *  -- removed: tuple
 *
 * Version 8.0
 *  -- added: new format (modifier) constructors HINDENT and SINDENT
 *  -- removed: bindent type and bindent fields in blocks
 *
 * Version 8.1
 *  -- Removed: hardIndent (conditionally breaks line before indented format)
 *  -- Added: breakIndent  (unconditionally breaks line before indented format)
 *
 * Version 8.2
 *  -- this file renamed prettyprint.sig
 *  -- NEW_PRETTYPRINT --> PRETTYPRINT
 *
 * Verion 8.3 [2023.1.6]
 *   -- Removed
 *      breakIndent  (didn't work, resets blm)
 *)

(* Defines: signature PRETTYPRINT *)

signature PRETTYPRINT =
sig

  (* types *)

    type format  (* abstract, defined in Format structure *)

    datatype alignment  (* the alignment property of "aligned" blocks *)
      = H  (* Horizontal alignment, with implicit single space breaks between format components, unbreakable *)
      | V  (* Vertical alignment, with implicit hardline separtors between format components *)
      | P  (* Packed alignment, with implicit softline separtors between format components *)
      | C  (* compact, no separators between block format elements, unbreakable *)

    datatype break  (* used to separate doc elements of a block; conditional and unconditional line breaks *)
      = HardLine         (* hard line break *)
      | SoftLine of int  (* soft line break; rendered to n spaces when not triggered; n >= 0 *)
      | Space of int     (* n spaces; n >= 0; Space 0 == NullBreak *)
      | NullBreak        (* A default break that does nothing, i.e. neither breaks a line nor inserts spaces.
			  * This is essentially equivalent to Space 0, but included for logical "completeness",
			  * and also eliminates the need for break option in some places (alignmentToBreak). *)

    datatype element
      = BRK of break   (* breaks are atomic and do not contain content *)
      | FMT of format

  (* Basic formats and format building operations: *)

    val empty   : format           (* == EMPTY, renders as empty string, composition identity *)
    val text    : string -> format (* == the TEXT format constructor *)
    val integer : int -> format    (* integer n renders as Int.toString n *)
    val string  : string -> format (* previously used PrintUtil.formatString, adds double quotes *)
    val char    : char -> format   (* c --> #"c" *)
    val bool    : bool -> format   (* true --> TEXT "true", false --> TEXT "false" *)

    (* block-building functions, corresponding to SBLOCK and BLOCK data constructors *)
    (* basicBlock -- the elements may include explicit separators *)

    val basicBlock : element list -> format
    val alignedBlock : alignment -> format list -> format

    (* building blocks, basic and aligned; n-ary versions taking lists, empty format args are absorbed
     *   empty argument list produces empty format *)

    val block  : element list -> format  (* = basicBlock NI *)
    val hblock : format list  -> format  (* = alignedBlock H NI *)
    val vblock : format list  -> format  (* = alignedBlock V NI *)
    val pblock : format list  -> format  (* = alignedBlock P NI *)
    val cblock : format list  -> format  (* = alignedBlock C NI *)

    (* a few "punctuation" characters as formats *)

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
    val equal : format     (* text "=", an honorary punctuation mark *)


  (* xcat: "binary versions" of the xblock functions (x = p, h, v, c), the empty format
   *  acts like an identy element for all these binary concatenation operators *)

    val pcat : format * format -> format
        (* combinds two formats in a P (packed) block, with an implicit soft line break
         * (SoftLine 1) between them *)

    val hcat : format * format -> format
        (* combinds two formats in an H block, with an implicit single space
         * (Space 1 break) between them *)

    val vcat : format * format -> format
        (* combinds two formats in an V block, with an implicit hard line break
         * (HardLine) between them *)

    val ccat : format * format -> format
        (* combinds two formats in a C block, with no separator (NullBreak) between them;
         * a binary version of cblock *)


  (* wrapping or enclosing formats, plus appending newlines and prepending labels *)

    val enclose : {front: format, back: format} -> format -> format
        (* concatenates front and back to the front, respecively back, of the format *)

    val parens : format -> format
        (* = enclose {front=lparen, back=rparen} format *)

    val brackets : format -> format
        (* like parens, but with lbracket and rbracket *)

    val braces : format -> format
        (* like parens, but with lbrace and rbrace *)

    val appendNewLine : format -> format
        (* append a newline to the format -- normally used for "top-level" printing *)

    val label : string -> format -> format


  (* composing lists of formats *)

    val sequence : alignment -> format -> format list -> format
        (* sequence a break fmts: inserts break between constituent fmts and aligns by a *)

    (* aligned sequence formatters, first argument is sep format, e.g. comma *)
    val hsequence : format -> format list -> format  (* = sequence H *)
    val psequence : format -> format list -> format  (* = sequence P *)
    val vsequence : format -> format list -> format  (* = sequence V *)
    val csequence : format -> format list -> format  (* = sequence C *)

    val tupleFormats : format list -> format  (* default packed alignment *)
        (* formats as a tuple *)

    val listFormats : format list -> format  (* default packed alignment *)
        (* formats as a list *)

    val optionFormat : format option -> format
        (* formats a format option by producing text "NONE" or wrapping "SOME(.)" around the format *)


  (* formating of lists (or options) of values of arbitrary type *)

    val formatSeq :
        {alignment: alignment, sep : format, formatter : 'a -> format}
	-> 'a list
	-> format

    val formatClosedSeq :
	{alignment: alignment, front: format, sep: format, back: format, formatter: 'a -> format}
	-> 'a list
	-> format

    val list : ('a -> format) -> 'a list -> format  (* default packed alignment P *)

    val alignedList : alignment -> ('a -> format) -> 'a list -> format

    val option : ('a -> format) -> 'a option -> format


  (* vertical alignment with header strings *)

    val vHeaders : {header1: string, header2: string, formatter: 'a -> format} -> 'a list -> format

    val vHeaderFormats : {header1: string, header2: string} -> format list -> format


  (* indenting formats *)

    val indent : int -> format -> format
        (* indent n EMPTY ==> EMPTY; indent n fmt ==> INDENT (n, frmt) *)


  (* Conditional formats: *)

    val tryFlat : format -> format
	(* if the format fits flat, then render it flat, otherwise render it normally *)

    val alt : format * format -> format
	(* if the first format fits flat, use it, otherwise render the second format,
	   NOTE: the two argument formats may not have the same content! But usually they should! *)

    val hvblock : format list -> format
	(* acts as hblock if it fits, otherwise as vblock *)


  (* functions used to define and access the line width *)

    val setLineWidthFun : (unit -> int) -> unit
	(* defines the function that returns the current lineWidth value *)

    val resetLineWidthFun : unit -> unit
	(* reset the lineWidthFun to the default lineWidthFun (which returns 90) *)

    val getLineWidth : unit -> int
	(* returns the current line width, the value returned by the current lineWidthFun *)


  (* Printing formats *)

    val render : format * (string -> unit) * int -> unit

    val printFormatLW  : int -> format -> unit
        (* printing to stdOut, with line width as first argument *)

    val printFormat : format -> unit
        (* print to stdOut with lineWidth = getLineWidth (), (typically = !Control.Print.lineWidth) *)

    val printFormatNL : format -> unit
	(* like printFormat, but with newline appened *)

end (* end PRETTYPRINT *)
