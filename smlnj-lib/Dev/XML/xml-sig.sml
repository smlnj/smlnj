(* xml-sig.sml
 *
 * COPYRIGHT (c) 2010 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature XML =
  sig

    type document
    type element
    type comment
    type text

  (* elements *)

  (* attributes *)
    datatype 'a attr_value
      = NO_ATTR			(* attribute not defined *)
      | WRONG_TYPE		(* attribute defined, but at a different type *)
      | VALUE of 'a		(* attribute value *)
    val getAttr : element * string -> string option
    val getIntAttr : element * string -> int attr_value
    val getRealAttr : element * string -> real attr_value

  (* navigation:
   *
   * Nodes are similar to TinyXML's handle type.  They are meant to be used
   * with the "//" function to simplify navigation through the tree.  For
   * example, consider the document:
   *
   *	<Document>
   *	  <Element attributeA = "valueA">
   *	    <Child attributeB = "value1" />
   *	    <Child attributeB = "value2" />
   *	  </Element>
   *	<Document>
   *
   * then one can extract the value of "attributeB" in the 2nd "Child" element
   * as follows:
   *
   *	let val root = documentNode doc
   *	    val elem = root // firstChildElem "Document"
   *	                    // firstChildElem "Element"
   *	                    // nextSiblingElem "Child"
   *	in
   *	  case toElement elem
   *	   of SOME elem => getAttr(elem, "attributeB")
   *	    | NONE => raise Fail "cannot find attribute"
   *	end
   *)
    type node
    val null : node
    val isNull : node -> bool
    val toElement : node -> element option
    val toText : node -> string option

    val // : node * (node -> node) -> node

    val firstChild : node -> node
    val firstChildElem : node -> node
    val firstChildName : string -> node -> node
    val child : int -> node -> node
    val childElem : int -> node -> node
    val childName : string -> int -> node -> node

  end

(* the XML_DOM interface supports the Document Object Model view of an XML
 * document, incuding editing.
 *)
signature XML_DOM =
  sig
    include XML

  (* element editing *)
    val addAttr : element * string * string -> unit
    val removeAttr : element * string -> unit

  end
