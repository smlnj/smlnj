(* xml-schema-sig.sml
 *
 * COPYRIGHT (c) 2013 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Information about an XML schema (or DTD) that is used in the implementation of
 * a parser.
 *)

signature XML_SCHEMA =
  sig

    type element
    type attribute

  (* create an element; returns NONE if the element name is unrecognized *)
    val element : string -> element option

  (* If this function returns true for an element, then all whitespace in the
   * element's content is preserved.  Otherwise, whitespace between tags is
   * not preserved.  Note that if true, this property is inherited by any
   * nested elements.
   *)
    val preserveWS : element -> bool

  (* should comments be preserved *)
    val preserveComment : element -> bool

  (* equality test *)
    val same : element * element -> bool

  (* the string representation of the element (w/o the "<" and ">" brackets) *)
    val toString : element -> string

  (* create an attribute from a name/value pair *)
    val attribute : (string * string) -> attribute

  end
