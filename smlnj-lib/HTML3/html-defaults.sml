(* html-defaults.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * Some HTML attributes have default values specified by the DTD; this
 * file defines values for these.
 *)

structure HTMLDefaults =
  struct

    val br_clear	= HTML.TextFlowCtl.none
    val area_shape	= HTML.Shape.rect
    val form_method	= HTML.HttpMethod.get
    val form_enctype	= "application/x-www-form-urlencoded"
    val input_type	= HTML.InputType.text
    val input_align	= HTML.IAlign.top
    val th_rowspan	= 1
    val th_colspan	= 1
    val td_rowspan	= 1
    val td_colspan	= 1

  end;

