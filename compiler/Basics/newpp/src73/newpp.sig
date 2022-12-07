(* sml/Dev/pp/new/new7/newpp.sig *)

(* Version 7.
 * The main interface of the new Prettyprinter.
 * New: memoized measure for blocks (does not alter NEW_PP signature)
 *)

(* Defines: signature NEW_PP *)

signature NEW_PP =
sig

  (* types *)

    type format  (* abstract, defined in Format structure *)

    datatype alignment  (* the alignment property of "aligned" blocks *)
      = H  (* Horizontal alignment, with implicit single space separtors between format components, unbreakable *)
      | V  (* Vertical alignment, with implicit hardline separtors between format components *)
      | P  (* Packed alignment, with implicit softline separtors between format components *)
      | C  (* compact, no separators between block format elements, unbreakable *)

    datatype separator  (* used to separate doc elements of a block *)
      = HardLine         (* hard line break *)
      | SoftLine of int  (* soft line break; rendered to n spaces when not triggered; n >= 0 *)
      | Space of int     (* n spaces; n >= 0 *)

    datatype element
      = SEP of separator  (* separators are _not_, and do not contain, content *)
      | FMT of format

    (* block indents: specify the indentation behavior on entering a block *)
    datatype bindent
      = NI          (* No indent *)
      | HI of int   (* Hard Indent: always taken, supplying its own newline+indent if necessary *)
      | SI of int   (* Soft Indent: taken only if the block is preceded by a newline+indent *)


  (* Basic formats and format building operations: *)

    val empty : format           (* == EMPTY, renders as empty string, composition identity *)
    val text : string -> format  (* == the TEXT format constructor *)
    val integer : int -> format  (* integer n renders as Int.toString n *)
    val string : string -> format (* previously used PrintUtil.formatString, adds double quotes *)
    val char : char -> format    (* c --> #"c" *)
    val bool : bool -> format    (* true --> TEXT "true", false --> TEXT "false" *)

    (* basic block-building functions, corresponding to SBLOCK and BLOCK data constructors *)
    (* specialBlock -- the elements may include explicit separators *)

    val specialBlock : bindent -> element list -> format
    val alignedBlock : alignment -> bindent -> format list -> format

    (* nonindented aligned blocks: bindent = NI *)

    val sblock : element list -> format  (* = specialBlock NI *)
    val hblock : format list -> format   (* = alignedBlock H NI *)
    val vblock : format list -> format   (* = alignedBlock V NI *)
    val pblock : format list -> format   (* = alignedBlock P NI *)
    val cblock : format list -> format   (* = alignedBlock C NI *)

    (* (possibly) indented blocks -- bindent specified as curried first argument *)

    val siblock : bindent -> element list -> format  (* = specialBlock *)
    val hiblock : bindent -> format list -> format   (* = alignedBlock H *)
    val viblock : bindent -> format list -> format   (* = alignedBlock V *)
    val piblock : bindent -> format list -> format   (* = alignedBlock P *)
    val ciblock : bindent -> format list -> format   (* = alignedBlock C *)

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
         * (Space 1 separator) between them *)

    val vcat : format * format -> format
        (* combinds two formats in an V block, with an implicit hard line break
         * (HardLine separator) between them *)

    val ccat : format * format -> format
        (* combinds two formats in a C block, with no separator between them;
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
        (* sequence a sep fmts: inserts sep between constituent fmts and aligns by a *)

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

    val tuple : ('a -> format) -> 'a list -> format  (* default packed alignment P *)

    val list : ('a -> format) -> 'a list -> format  (* default packed alignment P *)

    val alignedList : alignment -> ('a -> format) -> 'a list -> format

    val option : ('a -> format) -> 'a option -> format


  (* indenting formats *)

    val hardIndent : int -> format -> format
    (* produces a hard indented HBLOCK containing the format as sole component *)

    val softIndent : int -> format -> format
    (* produces a soft indented HBLOCK containing the format as sole component *)


  (* Conditional formats: *)

    val tryFlat : format -> format
	(* if the format fits flat, then render it flat, otherwise render it normally *)

    val alt : format * format -> format
	(* if the first format fits, use it, otherwise render the second format,
	   NOTE: the two argument formats may not have the same content! But usually they should! *)

    val hvblock : format list -> format
	(* hblock if it fits, otherwise vblock *)


  (* functions used to define and access the line width *)

    val setLineWidthFun : (unit -> int) -> unit
	(* defines the function that returns the current lineWidth value *)

    val resetLineWidthFun : unit -> unit
	(* reset the lineWidthFun to the default lineWidthFun (which returns 90) *)

    val getLineWidth : unit -> int
	(* returns the current line width *)


  (* Printing formats *)

    val render : format * (string -> unit) * int -> unit

    val printFormatLW  : int -> format -> unit 
        (* printing to stdOut, with line width as first argument *)

    val printFormat : format -> unit
        (* print to stdOut with lineWidth = !Control.Print.lineWidth *)

    val printFormatNL : format -> unit
	(* like printFormat, but with newline appened *)

end (* end NEW_PP *)
