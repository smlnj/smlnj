(* xml-tree-fn.sml
 *
 * COPYRIGHT (c) 2013 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

functor XMLTreeFn (Schema : XML_SCHEMA) : XML_TREE =
  struct

    structure Schema = Schema

  (* limited support for <!DOCTYPE ...> declarations.  Internal subsets are not
   * current supported.
   *)
    datatype doctype = DOCTYPE of string * external_id option

    and external_id
      = SYSTEM of string
      | PUBLIC of string * string
	
    datatype content
      = TEXT of string
      | CDATA of string
      | ELEMENT of {
	    name : Schema.element,
	    attrs : Schema.attribute list,
	    content : content list
	  }

    type tree = {
	xmlDecl : Schema.attribute list option,	(* NONE if there is no decl *)
	doctype : doctype option,
	content : content			(* will be an ELEMENT *)
      }

  end
