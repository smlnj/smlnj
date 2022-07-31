(* pp-desc-fn.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This functor implements a declarative way to specify pretty-printing
 * (see pp-desc-sig.sml).
 *)

functor PPDescFn (S : PP_STREAM) : PP_DESC =
  struct

    structure PPS = S

    type token = PPS.token
    type style = PPS.style
    type indent = PPS.indent

  (* The pp_desc type is a concrete representation of a PP layout. *)
    datatype pp_desc
      = HBox of pp_desc list
      | VBox of (indent * pp_desc list)
      | HVBox of (indent * pp_desc list)
      | HOVBox of (indent * pp_desc list)
      | Box of (indent * pp_desc list)
      | Token of token
      | String of string
      | Style of (style * pp_desc list)
      | Break of {nsp : int, offset : int}
      | NewLine
      | NBSpace of int
      | Control of (PPS.device -> unit)

  (* pretty print a description *)
    fun description (strm, ppd) = let
	  fun pp (HBox l) = (PPS.openHBox strm; ppList l; PPS.closeBox strm)
	    | pp (VBox(i, l)) = (PPS.openVBox strm i; ppList l; PPS.closeBox strm)
	    | pp (HVBox(i, l)) = (PPS.openHVBox strm i; ppList l; PPS.closeBox strm)
	    | pp (HOVBox(i, l)) = (PPS.openHOVBox strm i; ppList l; PPS.closeBox strm)
	    | pp (Box(i, l)) = (PPS.openBox strm i; ppList l; PPS.closeBox strm)
	    | pp (Token tok) = PPS.token strm tok
	    | pp (String s) = PPS.string strm s
	    | pp (Style(sty, l)) = (
		PPS.pushStyle(strm, sty); ppList l; PPS.popStyle strm)
	    | pp (Break brk) = PPS.break strm brk
	    | pp NewLine = PPS.newline strm
	    | pp (NBSpace n) = PPS.nbSpace strm n
	    | pp (Control ctlFn) = PPS.control strm ctlFn
	  and ppList [] = ()
	    | ppList (item::r) = (pp item; ppList r)
	  in
	    pp ppd
	  end

  (* exported PP description constructors *)
    val hBox    = HBox
    val vBox    = VBox
    val hvBox   = HVBox
    val hovBox  = HOVBox
    val box     = Box
    val token   = Token
    val string  = String
    val style   = Style
    val break   = Break
    fun space n = Break{nsp = n, offset = 0}
    val cut     = Break{nsp = 0, offset = 0}
    val newline = NewLine
    val nbSpace = NBSpace
    val control = Control

  end;

