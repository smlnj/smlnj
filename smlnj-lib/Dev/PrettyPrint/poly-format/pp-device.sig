(* pp-device.sig
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* This signature is the base signature for modules that implement
 * pretty-printing devices.  Such modules will define additional
 * operations for creating devices, etc.
 *)
signature PP_DEVICE =
  sig

    type 'sty device
	(* a device is an abstraction of an output stream.  Its type parameter
         * specifies the application-specific style type.
         *)

  (***** Style operations *****)

    val pushStyle : ('sty device * 'sty) -> unit
    val popStyle  : 'sty device -> unit
	(* push/pop a style from the devices style stack.  A pop on an
	 * empty style stack is a nop.
	 *)

    val defaultStyle : 'sty device -> 'sty
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
    val maxDepth : 'sty device -> int option
    val setMaxDepth : 'sty device * int option -> unit

  (* the sized string to print in place of boxes when the maximum depth is reached. *)
    val ellipses : 'sty device -> 'sty PrettyPrint.format
    val setEllipses : 'sty device * string -> unit
    val setEllipsesToken : 'sty device * 'sty PrettyPrint.token -> unit

  (* the width of the line for the device; `NONE` is infinite *)
    val lineWidth : 'sty device -> int option
    val setLineWidth : 'sty device * int option -> unit

  (* the suggested maximum width of indentation; `NONE` is interpreted as no limit. *)
    val maxIndent : 'sty device -> int option
    val setMaxIndent : 'sty device * int option -> unit

  (* the suggested maximum width of text on a line (i.e., not counting indentation).
   * `NONE` is interpreted as no limit.
   * NOTE: the pretty printer currently ignores this value.
   *)
    val textWidth : 'sty device -> int option
    val setTextWidth : 'sty device * int option -> unit


  (***** Output operations *****)

    val indent : ('sty device * int) -> unit
	(* output an indentation of the given width to the device *)

    val space : ('sty device * int) -> unit
	(* output some number of spaces to the device *)

    val newline : 'sty device -> unit
	(* output a new-line to the device *)

    val string : ('sty device * string) -> unit
    val char : ('sty device * char) -> unit
	(* output a string/character in the current style to the device *)

    val token : 'sty device * 'sty Token.token -> unit

    val flush : 'sty device -> unit
	(* if the device is buffered, then flush any buffered output *)

  end;
