(* check-html-fn.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * This implements a tree walk over an HTML file to check for
 * errors, such as violations of exclusions.
 *)

functor CheckHTMLFn (Err : HTML_ERROR) : sig

    type context = {file : string option, line : int}

    val check : context -> HTML.html -> unit

  end = struct

    type context = Err.context

    fun check context (HTML.HTML{body=HTML.BODY{content, ...}, ...}) = let
	  fun error (elem, ctx) =
		Err.syntaxError context
		  (Format.format "unexpected %s element in %s" [
		      Format.STR elem, Format.STR ctx
		    ])
	  fun contentError ctx =
		Err.syntaxError context
		  (Format.format "unexpected element in %s" [Format.STR ctx])
	  fun formError elem =
		Err.syntaxError context
		  (Format.format "unexpected %s element not in FORM" [
		      Format.STR elem
		    ])
	  fun attrError attr = Err.missingAttr context attr
	  fun checkBodyContent {inForm} b = (case b
		 of (HTML.Hn{n, align, content}) => checkText {
			inAnchor=false, inForm=inForm, inPre=false, inApplet=false
		      } content
		  | (HTML.ADDRESS block) => checkAddress {inForm=inForm} block
		  | (HTML.BlockList bl) =>
		      List.app (checkBodyContent {inForm=inForm}) bl
		  | block => checkBlock {inForm=inForm} block
		(* end case *))
	  and checkAddress {inForm} blk = (case blk
		 of (HTML.BlockList bl) =>
		      List.app (checkAddress {inForm=inForm}) bl
		  | (HTML.TextBlock txt) => checkText {
			inAnchor=false, inForm=inForm, inPre=false, inApplet = false
		      } txt
		  | (HTML.P{content, ...}) => checkText {
			inAnchor=false, inForm=inForm, inPre=false, inApplet = false
		      } content
		  | _ => contentError "ADDRESS"
		(* end case *))
	  and checkBlock {inForm} blk = (case blk
		 of (HTML.BlockList bl) =>
		      List.app (checkBlock {inForm=inForm}) bl
		  | (HTML.TextBlock txt) => checkText {
			inAnchor=false, inForm=inForm, inPre=false, inApplet = false
		      } txt
		  | (HTML.P{content, ...}) => checkText {
			inAnchor=false, inForm=inForm, inPre=false, inApplet = false
		      } content
		  | (HTML.UL{content, ...}) =>
		      checkItems {inForm=inForm, inDirOrMenu=false} content
		  | (HTML.OL{content, ...}) =>
		      checkItems {inForm=inForm, inDirOrMenu=false} content
		  | (HTML.DIR{content, ...}) =>
		      checkItems {inForm=inForm, inDirOrMenu=true} content
		  | (HTML.MENU{content, ...}) =>
		      checkItems {inForm=inForm, inDirOrMenu=true} content
		  | (HTML.DL{content, ...}) =>
		      checkDLItems {inForm=inForm} content
		  | (HTML.PRE{content, ...}) => checkText {
			inAnchor=false, inForm=inForm, inPre=true, inApplet = false
		      } content
		  | (HTML.DIV{content, ...}) =>
		      checkBodyContent {inForm=inForm} content
		  | (HTML.CENTER content) =>
		      checkBodyContent {inForm=inForm} content
		  | (HTML.BLOCKQUOTE content) =>
		      checkBodyContent {inForm=inForm} content
		  | (HTML.FORM{content, ...}) => (
		      if inForm then error("FORM", "FORM") else ();
		      checkBodyContent {inForm=true} content)
		  | (HTML.ISINDEX _) => ()
		  | (HTML.HR _) => ()
		  | (HTML.TABLE{
		      caption=SOME(HTML.CAPTION{content=caption, ...}),
		      content, ...
		    }) => (
		      checkText {
			  inAnchor=false, inForm=inForm, inPre=false,
			  inApplet = false
		        } caption;
		      checkRows {inForm=inForm} content)
		  | (HTML.TABLE{content, ...}) => checkRows {inForm=inForm} content
		  | (HTML.Hn _) => error ("Hn", "block")
		  | (HTML.ADDRESS _) => error ("ADDRESS", "block")
		(* end case *))
	  and checkItems {inForm, inDirOrMenu} items = let
		fun chkBlk (HTML.BlockList bl) = List.app chkBlk bl
		  | chkBlk (HTML.TextBlock txt) = ()
		  | chkBlk (HTML.P _) = ()
		  | chkBlk _ = error ("block", "DIR/MENU")
		val chk = if inDirOrMenu
		      then (fn (HTML.LI{content, ...}) => (
			chkBlk content; checkBlock {inForm=inForm} content))
		      else (fn (HTML.LI{content, ...}) => (
			checkBlock {inForm=inForm} content))
		in
		  List.app chk items
		end
	  and checkDLItems {inForm} items = let
		fun chk {dt, dd} = (
		      List.app
			(checkText {
			  inAnchor=false, inForm=inForm, inPre=false, inApplet=false
			})
			  dt;
		      checkBlock {inForm=inForm} dd)
		in
		  List.app chk items
		end
	  and checkRows {inForm} rows = let
		fun chkCell (HTML.TH{content, ...}) =
		      checkBodyContent {inForm=inForm} content
		  | chkCell (HTML.TD{content, ...}) =
		      checkBodyContent {inForm=inForm} content
		fun chkRow (HTML.TR{content, ...}) = List.app chkCell content
		in
		  List.app chkRow rows
		end
	  and checkText {inAnchor, inForm, inPre, inApplet} = let
		fun chk txt = (case txt
		       of (HTML.TextList tl) => List.app chk tl
			| (HTML.PCDATA _) => ()
			| (HTML.TT txt) => chk txt
			| (HTML.I txt) => chk txt
			| (HTML.B txt) => chk txt
			| (HTML.U txt) => chk txt
			| (HTML.STRIKE txt) => chk txt
			| (HTML.BIG txt) => (
			    if inPre then error("BIG", "PRE") else ();
			    chk txt)
			| (HTML.SMALL txt) => (
			    if inPre then error("SMALL", "PRE") else ();
			    chk txt)
			| (HTML.SUB txt) => (
			    if inPre then error("SUB", "PRE") else ();
			    chk txt)
			| (HTML.SUP txt) => (
			    if inPre then error("SUP", "PRE") else ();
			    chk txt)
			| (HTML.EM txt) => chk txt
			| (HTML.STRONG txt) => chk txt
			| (HTML.DFN txt) => chk txt
			| (HTML.CODE txt) => chk txt
			| (HTML.SAMP txt) => chk txt
			| (HTML.KBD txt) => chk txt
			| (HTML.VAR txt) => chk txt
			| (HTML.CITE txt) => chk txt
			| (HTML.A{content, ...}) => (
			    if (inAnchor) then error("anchor", "anchor") else ();
		 	    checkText {
				inAnchor=true, inForm=inForm, inPre=inPre,
				inApplet=inApplet
			      } content)
			| (HTML.IMG _) =>
			    if inPre then error("IMG", "PRE") else ()
			| (HTML.APPLET{content, ...}) => checkText {
			      inAnchor=false, inForm=inForm, inPre=inPre,
			      inApplet=true
			    } content
			| (HTML.PARAM _) =>
			    if inApplet then error ("param", "applet") else ()
			| (HTML.FONT{content, ...}) =>
			    if inPre then error("FONT", "PRE") else ()
			| (HTML.BASEFONT{content, ...}) =>
			    if inPre then error("BASEFONT", "PRE") else ()
			| (HTML.BR _) => ()
			| (HTML.MAP _) => ()
			| (HTML.INPUT{ty, name, value, ...}) => (
			    if (not inForm) then formError "INPUT" else ();
			    if ((name = NONE)
			    andalso (ty <> SOME(HTML.InputType.submit))
			    andalso (ty <> SOME(HTML.InputType.reset)))
			      then attrError "NAME"
			      else ();
			    if ((value = NONE)
			    andalso ((ty = SOME(HTML.InputType.radio))
			    orelse (ty = SOME(HTML.InputType.checkbox))))
			      then attrError "VALUE"
			      else ())
			| (HTML.SELECT _) =>
			    if (not inForm) then formError "SELECT" else ()
			| (HTML.TEXTAREA _) =>
			    if (not inForm) then formError "TEXTAREA" else ()
			| (HTML.SCRIPT _) => ()
		      (* end case *))
		in
		  chk
		end
	  in
	    checkBodyContent {inForm=false} content
	  end

  end
