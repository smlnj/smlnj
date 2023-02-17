(* Admin/control/parser-control.sig
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
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

  (* controls Successor-ML features. -- should this be exported, given setSuccML?*)
  val succML : bool ref

  (* raised to force a parser switch (e.g., from SML'97 to Succ ML) *)
  exception RESET_PARSER

  (* set/clear Successor ML mode *)
  val setSuccML : bool -> unit

end (* signature PARSER_CONTROL *)
