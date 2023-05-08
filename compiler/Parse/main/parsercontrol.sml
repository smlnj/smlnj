(* parsercontrol.sml
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature PARSER_CONTROL =
sig

    val primaryPrompt : string ref
    val secondaryPrompt : string ref

  (* turn on lazy keywords and lazy declaration processing *)
    val lazysml : bool ref		(* default false *)

  (* controls "overload" as keyword *)
    val overloadKW : bool ref

  (* controls backquote quotation *)
    val quotation : bool ref

  (* controls printing of Ast location info *)
    val astLineprint : bool ref			 

  (* controls printing of internal Ast info *)
    val astInternals : bool ref			 

  (* set/clear Successor ML mode *)
    val setSuccML : bool -> unit

end (* signature PARSER_CONTROL *)

structure ParserControl : sig

    include PARSER_CONTROL

  (* the following components are not part of the PARSER_CONTROL
   * signature, because we do not want it to be visibile in the REPL.
   *)

  (* controls Successor-ML features. *)
    val succML : bool ref

  (* raised to force a parser switch (e.g., from SML'97 to Succ ML) *)
    exception RESET_PARSER

  end =

struct

  val {newBool, newInt, newString, newStrings} = MakeControls.make {name = "Parser", priority = [1]}

  val primaryPrompt =
	newString ("primary-prompt", "primary prompt", "- ")

  val secondaryPrompt =
	newString ("secondary-prompt", "secondary prompt","= ")

  val lazysml =
	newBool ("lazy-keyword", "whether `lazy' is considered a keyword", false)

  val overloadKW =
	newBool ("overload", "whether (_)overload keyword is enabled", false)

  val quotation =
	newBool ("quotations", "whether (anti-)quotations are recognized", false)

  val astLineprint =
	newBool ("astLineprint", "printing of ast location info", false)

  val astInternals =
	newBool ("astInternals", "printing of ast internal info", false)

  val succML =
	newBool ("succ-ml", "whether Successor-ML extensions are recognized", false)

  exception RESET_PARSER

  (* set/clear Successor ML mode *)
  fun setSuccML flg =
	if (!succML <> flg)
	  then (succML := flg; raise RESET_PARSER)
	  else ()

end (* structure ParserControl *)
