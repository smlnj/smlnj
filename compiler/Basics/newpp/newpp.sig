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
      = H  (* Horizontal alignment, with implicit single space separtors between format components *)
      | V  (* Vertical alignment, with implicit hardline separtors between format components *)
      | P  (* Packed alignment, with implicit softline separtors between format components *)

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

    val empty : format           (* == TEXT "", renders as empty string *)
    val text : string -> format  (* == the TEXT format constructor *)
    val integer : int -> format  (* integer n renders as Int.toString n *)
    val string : string -> format (* previously used PrintUtil.formatString *)

    (* basic block-building functions, corresponding to SBLOCK and BLOCK data constructors *)
    (* specialBlock -- the elements may include explicit separators *)

    val specialBlock : bindent -> element list -> format
    val alignedBlock : alignment -> bindent -> format list -> format

    (* nonindented aligned blocks: bindent = NI *)

    val sblock : element list -> format  (* = specialBlock NI *)
    val hblock : format list -> format   (* = alignedBlock H NI *)
    val vblock : format list -> format   (* = alignedBlock V NI *)
    val pblock : format list -> format   (* = alignedBlock P NI *)

    (* (possibly) indented blocks -- bindent specified as curried first argument *)

    val siblock : bindent -> element list -> format  (* = specialBlock *)
    val hiblock : bindent -> format list -> format   (* = alignedBlock H *)
    val viblock : bindent -> format list -> format   (* = alignedBlock V *)
    val piblock : bindent -> format list -> format   (* = alignedBlock P *)

    (* a few "punctuation" characters as formats *)

    val comma : format
    val colon : format
    val semicolon : format
    val lparen : format
    val rparen : format
    val lbracket : format  (* text "[" *)
    val rbracket : format  (* text "]" *)
    val lbrace : format    (* text "{" *)
    val rbrace : format    (* text "}" *)

    val concat : format list -> format
        (* concatenate a list of formats with no separators *)

    (* xcat: "binary versions" of the xblock functions (x = p, h, v) *)

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
        (* combinds two formats in a "compact" S block, with no separator between them;
         * a binary version of concat *)

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


  (* composing lists of formats *)

    val sequence : {alignment: alignment, sep: format} -> format list -> format
        (* inserts sep between constituent formats and aligns *)

    val tupleFormats : format list -> format  (* default packed alignment *)
        (* formats as a tuple *)

    val listFormats : format list -> format  (* default packed alignment *)
        (* formats as a list *)


  (* formating of lists of values of arbitrary type *)

    val formatSeq :
        {alignment: alignment, sep : format, formatter : 'a -> format}
	-> 'a list
	-> format

    val formatClosedSeq :
	{alignment: alignment, front: format, sep: format, back: format, formatter: 'a -> format}
	-> 'a list
	-> format

    val formatTuple : ('a -> format) -> 'a list -> format

    val formatAlignedList : alignment -> ('a -> format) -> 'a list -> format

    val formatList : ('a -> format) -> 'a list -> format  (* default packed alignment *)


  (* indenting formats *)

    val hardIndent : int * format -> format
    (* produces a hard indented HBLOCK containing the format as sole component *)

    val softIndent : int * format -> format
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

    val printFormatLW' : format -> int -> unit
        (* like printFormatLW, with arguments reversed *)

    val printFormat : format -> unit
        (* print to stdOut with lineWidth = !Control.Print.lineWidth *)

    val printFormatNL : format -> unit
	(* like printFormat, but with newline appened *)

end (* end NEW_PP *)