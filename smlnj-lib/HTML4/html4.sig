(* html4.sig
 *
 * COPYRIGHT (c) 2014 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Main signature for the HTML4 library.
 *)

signature HTML4 =
  sig

    type pcdata = string

    datatype cdata
      = PCDATA of pcdata
      | ENTITY of Atom.atom
      | CHAR of IntInf.int
      | COMMENT of string

    type attribute = Atom.atom * string option

    type attributes = attribute list

    type col = attributes (* COL element is empty *)

    datatype html = HTML of {
	version : string option,
	head : head_content list,
	content : body_or_frameset
      }

    and head_content
      = Head_TITLE of attributes * cdata list
      | Head_BASE of attributes
      | Head_SCRIPT of script
      | Head_STYLE of attributes * cdata list
      | Head_META of attributes
      | Head_LINK of attributes
      | Head_OBJECT of attributes * flow_or_param list

    and body_or_frameset
      = BodyOrFrameset_BODY of body
      | BodyOrFrameset_FRAMESET of frameset

    and body
      = BODY of attributes * block_or_script list

    and frameset
      = FRAMESET of attributes * frameset_or_frame list * noframes option

    and frameset_or_frame
      = FramesetOrFrame_FRAMESET of frameset
      | FRAME of attributes

    and noframes
      = NOFRAMES of attributes * body

    and flow 
      = Flow_BLOCK of block
      | Flow_INLINE of inline

    and block
      = P of attributes * inline list
      | H1 of attributes * inline list
      | H2 of attributes * inline list
      | H3 of attributes * inline list
      | H4 of attributes * inline list
      | H5 of attributes * inline list
      | H6 of attributes * inline list
      | UL of attributes * list_item list
      | OL of attributes * list_item list
      | DIR of attributes * list_item list (* Loose *)
      | MENU of attributes * list_item list (* Loose *)
      | PRE of attributes * inline list
      | DL of attributes * def_term_or_desc list
      | DIV of attributes * flow list
      | NOSCRIPT of attributes * block list
      | BLOCKQUOTE of attributes * block_or_script list
      | FORM of attributes * block_or_script list
      | HR of attributes
      | TABLE of attributes * table_data list
      | FIELDSET of attributes * legend option * flow list
      | ADDRESS of attributes * inline list
      | CENTER of attributes * flow list (* Loose *)
      | ISINDEX of attributes (* Loose *)

    and inline
      = TT of attributes * inline list
      | I of attributes * inline list
      | B of attributes * inline list
      | BIG of attributes * inline list
      | SMALL of attributes * inline list
      | U of attributes * inline list (* Loose *)
      | S of attributes * inline list(* Loose *)
      | STRIKE of attributes * inline list (* Loose *)
      | EM of attributes * inline list
      | STRONG of attributes * inline list
      | DFN of attributes * inline list
      | CODE of attributes * inline list
      | SAMP of attributes * inline list
      | KBD of attributes * inline list
      | VAR of attributes * inline list
      | CITE of attributes * inline list
      | ABBR of attributes * inline list
      | ACRONYM of attributes * inline list
      | A of attributes * inline list
      | IMG of attributes
      | OBJECT of attributes * flow_or_param list
      | BR of attributes
      | Inline_SCRIPT of script
      | MAP of attributes * block_or_area list
      | Q of attributes * inline list
      | SUB of attributes * inline list
      | SUP of attributes * inline list
      | SPAN of attributes * inline list
      | BDO of attributes * inline list
      | APPLET of attributes * flow_or_param list (* Loose *)
      | BASEFONT of attributes (* Loose *)
      | FONT of attributes * inline list (* Loose *)
      | IFRAME of attributes * flow list (* Loose *)
      | INPUT of attributes
      | SELECT of attributes * optgroup_or_option list
      | TEXTAREA of attributes * cdata list
      | LABEL of attributes * inline list
      | BUTTON of attributes * flow list
      | CDATA of cdata list

    and list_item = LI of attributes * flow list

    and script = SCRIPT of attributes * cdata list

    and param = PARAM of attributes

    and legend = LEGEND of attributes * inline list

    and def_term_or_desc
      = DT of attributes * inline list
      | DD of attributes * flow list

    and table_data
      = CAPTION of attributes * inline list
      | COL of col
      | COLGROUP of attributes * col list
      | THEAD of attributes * tr list
      | TFOOT of attributes * tr list
      | TBODY of attributes * tr list

    and tr
      = TR of attributes * th_or_td list

    and th_or_td
      = TH of attributes * flow list
      | TD of attributes * flow list

  (* either "<OPTGROUP><OPTION>...</OPTION> ... </OPTGROUP>" or "<OPTION>...</OPTION>" *)
    and optgroup_or_option
      = OPTGROUP of attributes * (attributes * cdata list) list
      | OPTION of attributes * cdata list

    and flow_or_param
      = FlowOrParam_FLOW of flow
      | FlowOrParam_PARAM of param

    and block_or_script
      = BlockOrScript_BLOCK of block
      | BlockOrScript_SCRIPT of script

    and block_or_area
      = BlockOrArea_BLOCK of block
      | AREA of attributes

end

(* ______________________________________________________________________
   End of html4.sig
   ______________________________________________________________________ *)
