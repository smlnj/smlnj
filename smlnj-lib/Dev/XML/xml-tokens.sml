(* xml-tokens.sml
 *
 * COPYRIGHT (c) 2013 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure XMLTokens =
  struct

    datatype token
      = EOF
      | OPEN_START_TAG		(* "<" *)
      | OPEN_END_TAG		(* "</" *)
      | OPEN_XML_TAG		(* "<?xml" *)
      | OPEN_PI_TAG		(* "<?" *)
      | OPEN_DOCTYPE		(* <!DOCTYPE *)
      | CLOSE_TAG		(* ">" *)
      | CLOSE_EMPTY_TAG		(* "/>" *)
      | CLOSE_PI_TAG		(* "?>" also closes XML tags *)
      | SYM_EQ			(* "=" inside a tag *)
      | ID of string		(* element or attribute name *)
      | LIT of string		(* quoted attribute value *)
    (* the following tags are content *)
      | TEXT of string		(* non-whitespace/non-comment text *)
      | WS of string		(* whitespace *)
      | COM of string		(* XML comment; string does not include "<!--" and "-->" *)
      | CDATA of string		(* CDATA text; string does not include "<![CDATA[" and "]]>" *)
      | PUBLIC			(* "PUBLIC" in <!DOCTYPE ...> *)
      | SYSTEM			(* "SYSTEM" in <!DOCTYPE ...> *)

    fun toString tok = (case tok
	   of EOF => "EOF"
	    | OPEN_START_TAG => "<"
	    | OPEN_END_TAG => "</"
	    | OPEN_XML_TAG => "<?xml"
	    | OPEN_PI_TAG => "<?"
	    | OPEN_DOCTYPE => "<!DOCTYPE"
	    | CLOSE_TAG => ">"
	    | CLOSE_EMPTY_TAG => "/>"
	    | CLOSE_PI_TAG => "?>"
	    | SYM_EQ => "="
	    | ID s => s
	    | LIT _ => "LIT"
	    | TEXT _ => "TEXT"
	    | WS _ => "WS"
	    | COM _=> "COM"
	    | CDATA _ => "CDATA"
	    | PUBLIC => "PUBLIC"
	    | SYSTEM => "SYSTEM"
	  (* end case *))

  end
