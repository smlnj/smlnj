(* Admin/control/parser-control.sml
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure ParserControl : PARSER_CONTROL =
struct

  val primaryPrompt = ref "- " (* "primary prompt" *)

  val secondaryPrompt = ref "= " (* "secondary prompt" *)

  val lazysml = ref false (* "whether `lazy' is considered a keyword" *)

  val overloadKW = ref false (* "whether (_)overload keyword is enabled" *)

  val quotation = ref false (* "whether (anti-)quotations are recognized" *)

  val astLineprint = ref false (* "printing of ast location info" *)

  val astInternals = ref false (* "printing of ast internal info" *)

  val succML = ref false (* "whether Successor-ML extensions are recognized" *)

  exception RESET_PARSER

  (* set/clear Successor ML mode *)
  fun setSuccML flg =
	if (!succML <> flg)
	  then (succML := flg; raise RESET_PARSER)
	  else ()

end (* structure ParserControl *)
