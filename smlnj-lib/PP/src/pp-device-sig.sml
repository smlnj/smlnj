(* pp-device-sig.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * A pretty-printer device is an abstraction of an output stream.
 *)

signature PP_DEVICE =
  sig

    type device
	(* a device is an abstraction of an output stream. *)

    type style
	(* an abstraction of font and color information.  For devices that
	 * support styled text, they should maintain a stack of styles, with
	 * the top of stack being the "current" style. Implementers of this
	 * signature should extend it with functions for creating style values.
	 *)


  (***** Style operations *****)

    val sameStyle : (style * style) -> bool
	(* are two styles the same? *)

    val pushStyle : (device * style) -> unit
    val popStyle  : device -> unit
	(* push/pop a style from the devices style stack.  A pop on an
	 * empty style stack is a nop.
	 *)

    val defaultStyle : device -> style
	(* the default style for the device (this is the current style,
	 * if the style stack is empty).
	 *)


  (***** Device properties *****
   **
   ** Note that the pretty-printer stream may cache these values, so that
   ** changing them mid-flight may not affect existing pretty-printing
   ** streams.  Devices may also not support various features, in which
   ** case, the `set` functions are no-ops.
   **)

  (* the maximum printing depth (in number of open boxes); `NONE` is
   * interpreted as no limit.
   *)
    val maxDepth : device -> int option
    val setMaxDepth : device * int option -> unit

  (* the sized string to print in place of boxes when the maximum depth is reached. *)
    val ellipses : device -> string * int
    val setEllipses : device * string -> unit
    val setEllipsesWithSz : device * string * int -> unit

  (* the width of the line for the device; `NONE` is infinite *)
    val lineWidth : device -> int option
    val setLineWidth : device * int option -> unit

  (* the suggested maximum width of indentation; `NONE` is interpreted as no limit. *)
    val maxIndent : device -> int option
    val setMaxIndent : device * int option -> unit

  (* the suggested maximum width of text on a line (i.e., not counting indentation).
   * `NONE` is interpreted as no limit.
   * NOTE: the pretty printer currently ignores this value.
   *)
    val textWidth : device -> int option
    val setTextWidth : device * int option -> unit


  (***** Output operations *****)

    val indent : (device * int) -> unit
	(* output an indentation of the given width to the device *)

    val space : (device * int) -> unit
	(* output some number of spaces to the device *)

    val newline : device -> unit
	(* output a new-line to the device *)

    val string : (device * string) -> unit
    val char : (device * char) -> unit
	(* output a string/character in the current style to the device *)

    val flush : device -> unit
	(* if the device is buffered, then flush any buffered output *)

  end;
