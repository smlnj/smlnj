(* html-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * This file defines the abstract syntax of HTML documents.  The
 * AST follows the HTML 3.2 Proposed Standard.
 *)

signature HTML = 
  sig

    val htmlVersion : string	(* = "3.2" *)

  (* the HTML data representations (these are all string) *)
    type pcdata = string
    type cdata = string
    type url = string
    type pixels = cdata
    type name = string
    type id = string

  (* the different types of HTTP methods *)
    structure HttpMethod : sig
	eqtype method
	val get : method
	val put : method
	val toString : method -> string
	val fromString : string -> method option
      end

  (* the different types of INPUT elements *)
    structure InputType : sig
	eqtype ty
	val text : ty
	val password : ty
	val checkbox : ty
	val radio : ty
	val submit : ty
	val reset : ty
	val file : ty
	val hidden : ty
	val image : ty
	val toString : ty -> string
	val fromString : string -> ty option
      end

  (* alignment attributes for IMG, APPLET and INPUT elements *)
    structure IAlign : sig
	eqtype align
	val top : align
	val middle : align
	val bottom : align
	val left : align
	val right : align
	val toString : align -> string
	val fromString : string -> align option
      end

    structure HAlign : sig
	eqtype align
	val left : align
	val center : align
	val right : align
	val toString : align -> string
	val fromString : string -> align option
      end

    structure CellVAlign : sig
	eqtype align
	val top : align
	val middle : align
	val bottom : align
	val baseline : align
	val toString : align -> string
	val fromString : string -> align option
      end

    structure CaptionAlign : sig
	eqtype align
	val top : align
	val bottom : align
	val toString : align -> string
	val fromString : string -> align option
      end

    structure ULStyle : sig
	eqtype style
	val disc : style
	val square : style
	val circle : style
	val toString : style -> string
	val fromString : string -> style option
      end

    structure Shape : sig
	eqtype shape
	val rect : shape
	val circle : shape
	val poly : shape
	val default : shape
	val toString : shape -> string
	val fromString : string -> shape option
      end

    structure TextFlowCtl : sig
	eqtype control
	val left : control
	val right : control
	val all : control
	val none : control
	val toString : control -> string
	val fromString : string -> control option
      end

    datatype html = HTML of {
	version : cdata option,
	head : head_content list,
	body : body
      }

    and head_content
      = Head_TITLE of pcdata
      | Head_ISINDEX of {prompt : cdata option}
      | Head_BASE of {href : url}
      | Head_META of {
	    httpEquiv : name option,
	    name : name option,
	    content : cdata
	  }
      | Head_LINK of {
	    id : id option,
	    href : url option,
	    rel : cdata option,
	    rev : cdata option,
	    title : cdata option
	  }
    (* SCRIPT/STYLE elements are placeholders for the next version of HTML *)
      | Head_SCRIPT of pcdata
      | Head_STYLE of pcdata

    and body = BODY of {
	background : url option,
	bgcolor : cdata option,
	text : cdata option,
	link : cdata option,
	vlink : cdata option,
	alink : cdata option,
	content : block
      }

    and block
      = BlockList of block list
      | TextBlock of text
      | Hn of {
	    n : int,
	    align : HAlign.align option,
	    content : text
	  }
    (* NOTE: the content of an ADDRESS element is really (text | P)* *)
      | ADDRESS of block
      | P of {
	    align : HAlign.align option,
	    content : text
	  }
      | UL of {
	    ty : ULStyle.style option,
	    compact : bool,
	    content : list_item list
	  }
      | OL of {
	    ty : cdata option,
	    start : int option,
	    compact : bool,
	    content : list_item list
	  }
      | DIR of {
	    compact : bool,
	    content : list_item list
	  }
      | MENU of {
	    compact : bool,
	    content : list_item list
	  }
      | DL of {
	    compact : bool,
	    content : {dt : text list, dd : block} list
	  }
      | PRE of {
	    width : int option,
	    content : text
	  }
      | DIV of {
	    align : HAlign.align,
	    content : block
	  }
      | CENTER of block
      | BLOCKQUOTE of block
      | FORM of {
	    action : url option,
	    method : HttpMethod.method,
	    enctype : cdata option,
	    content : block		(* -(FORM) *)
	  }
      | ISINDEX of {prompt : cdata option}
      | HR of {
	    align : HAlign.align option,
	    noshade : bool,
	    size : pixels option,
	    width : cdata option
	  }
      | TABLE of {
	    align : HAlign.align option,
	    width : cdata option,
	    border : pixels option,
	    cellspacing : pixels option,
	    cellpadding : pixels option,
	    caption : caption option,
	    content : tr list
	  }

    and list_item = LI of {
	    ty : cdata option,
	    value : int option,
	    content : block
	  }

  (** table content **)
    and caption = CAPTION of {
	    align : CaptionAlign.align option,
	    content : text
	  }
    and tr = TR of {
	    align : HAlign.align option,
	    valign : CellVAlign.align option,
	    content : table_cell list
	  }
    and table_cell
      = TH of {
	    nowrap : bool,
	    rowspan : int option,
	    colspan : int option,
	    align : HAlign.align option,
	    valign : CellVAlign.align option,
	    width : pixels option,
	    height : pixels option,
	    content : block
	  }
      | TD of {
	    nowrap : bool,
	    rowspan : int option,
	    colspan : int option,
	    align : HAlign.align option,
	    valign : CellVAlign.align option,
	    width : pixels option,
	    height : pixels option,
	    content : block
	  }

  (** Text **)
    and text
      = TextList of text list
      | PCDATA of pcdata
      | TT of text
      | I of text
      | B of text
      | U of text
      | STRIKE of text
      | BIG of text
      | SMALL of text
      | SUB of text
      | SUP of text
      | EM of text
      | STRONG of text
      | DFN of text
      | CODE of text
      | SAMP of text
      | KBD of text
      | VAR of text
      | CITE of text
      | A of {
	    name : cdata option,
	    href : url option,
	    rel : cdata option,
	    rev : cdata option,
	    title : cdata option,
	    content : text		(* -(A) *)
	  }
      | IMG of {
	    src : url,
	    alt : cdata option,
	    align : IAlign.align option,
	    height : pixels option,
	    width : pixels option,
	    border : pixels option,
	    hspace : pixels option,
	    vspace : pixels option,
	    usemap : url option,
	    ismap : bool
	  }
      | APPLET of {
	    codebase : url option,
	    code : cdata,
	    name : cdata option,
	    alt : cdata option,
	    align : IAlign.align option,
	    height : pixels option,
	    width : pixels option,
	    hspace : pixels option,
	    vspace : pixels option,
	    content : text
	  }
      | PARAM of {		(* applet parameter *)
	    name : name,
	    value : cdata option
	  }
      | FONT of {
	    size : cdata option,
	    color : cdata option,
	    content : text
	  }
      | BASEFONT of {
	    size : cdata option,
	    content : text
	  }
      | BR of {
	    clear : TextFlowCtl.control option
	  }
      | MAP of {
	    name : cdata option,
	    content : area list
	  }
      | INPUT of {
	    ty : InputType.ty option,
	    name : cdata option,
	    value : cdata option,
	    checked : bool,
	    size : cdata option,
	    maxlength : int option,
	    src : url option,
	    align : IAlign.align option
	  }
      | SELECT of {
	    name : cdata,
	    size : int option,
	    content : select_option list
	  }
      | TEXTAREA of {
	    name : cdata,
	    rows : int,
	    cols : int,
	    content : pcdata
	  }
    (* SCRIPT elements are placeholders for the next version of HTML *)
      | SCRIPT of pcdata

  (* map areas *)
    and area = AREA of {
	    shape : Shape.shape option,
	    coords : cdata option,
	    href : url option,
	    nohref : bool,
	    alt : cdata
	  }

  (* SELECT options *)
    and select_option = OPTION of {
	    selected : bool,
	    value : cdata option,
	    content : pcdata
	  }

  end (* signature HTML *)

