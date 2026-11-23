(* html-error-sig.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * This is the interface of the error functions supplied to the lexer
 * (and transitively, to HTMLElemnts).
 *)

signature HTML_ERROR =
  sig

    type context = {file : string option, line : int}

    val badStartTag : context -> string -> unit
	(* called on unrecognized start tags; the string is the tag name *)
    val badEndTag : context -> string -> unit
	(* called on unrecognized end tags, or end tags for empty elements;
	 * the string is the tag name.
	 *)
    val badAttrVal : context -> (string * string) -> unit
	(* called on ill-formed attribute values; the first string is the
	 * attribute name, and the second is the value.
	 *)
    val lexError : context -> string -> unit
	(* called on other lexical errors; the string is an error message. *)
    val syntaxError : context -> string -> unit
	(* called on syntax errors; the string is an error message. *)
    val missingAttrVal : context -> string -> unit
	(* called when an attribute name is given without a value *)
    val missingAttr : context -> string -> unit
	(* called on a missing required attribute; the string is the attribute
	 * name.
	 *)
    val unknownAttr : context -> string -> unit
	(* called on unknown attributes; the string is the attribute name. *)
    val unquotedAttrVal : context -> string -> unit
	(* called when the attribute value should have been quoted, but wasn't;
	 * the string is the attribute name.
	 *)

  end
