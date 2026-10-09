(* html-attrs.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * This is the interface to HTMLAttrs, which provides support for parsing
 * element start tags.
 *)

signature HTML_ATTRS =
  sig

    type context = {file : string option, line : int}

  (* support for building elements that have attributes *)
    datatype attr_val = datatype HTMLAttrVals.attr_val
    type attrs = (string * attr_val) list

    val mkISINDEX : (context * attrs) -> {prompt : HTML.cdata option}
    val mkBASE : (context * attrs) -> HTML.head_content
    val mkMETA : (context * attrs) -> HTML.head_content
    val mkLINK : (context * attrs) -> HTML.head_content
    val mkBODY : (context * attrs * HTML.block) -> HTML.body
    val mkHn : (int * context * attrs * HTML.text) -> HTML.block
    val mkP : (context * attrs * HTML.text) -> HTML.block
    val mkUL : (context * attrs * HTML.list_item list) -> HTML.block
    val mkOL : (context * attrs * HTML.list_item list) -> HTML.block
    val mkDIR : (context * attrs * HTML.list_item list) -> HTML.block
    val mkMENU : (context * attrs * HTML.list_item list) -> HTML.block
    val mkLI : (context * attrs * HTML.block) -> HTML.list_item
    val mkDL : (context * attrs * {dt : HTML.text list, dd : HTML.block} list)
	  -> HTML.block
    val mkPRE : (context * attrs * HTML.text) -> HTML.block
    val mkDIV : (context * attrs * HTML.block) -> HTML.block
    val mkFORM : (context * attrs * HTML.block) -> HTML.block
    val mkHR : (context * attrs) -> HTML.block
    val mkTABLE : (context * attrs * {
	    caption : HTML.caption option,
	    body : HTML.tr list
	  }) -> HTML.block
    val mkCAPTION : (context * attrs * HTML.text) -> HTML.caption
    val mkTR : (context * attrs * HTML.table_cell list) -> HTML.tr
    val mkTH : (context * attrs * HTML.block) -> HTML.table_cell
    val mkTD : (context * attrs * HTML.block) -> HTML.table_cell
    val mkA : (context * attrs * HTML.text) -> HTML.text
    val mkIMG : (context * attrs) -> HTML.text
    val mkAPPLET : (context * attrs * HTML.text) -> HTML.text
    val mkPARAM : (context * attrs) -> HTML.text
    val mkFONT : (context * attrs * HTML.text) -> HTML.text
    val mkBASEFONT : (context * attrs * HTML.text) -> HTML.text
    val mkBR : (context * attrs) -> HTML.text
    val mkMAP : (context * attrs * HTML.area list) -> HTML.text
    val mkINPUT : (context * attrs) -> HTML.text
    val mkSELECT : (context * attrs * HTML.select_option list) -> HTML.text
    val mkTEXTAREA : (context * attrs * HTML.pcdata) -> HTML.text
    val mkAREA : (context * attrs) -> HTML.area
    val mkOPTION : (context * attrs * HTML.pcdata) -> HTML.select_option

  end

