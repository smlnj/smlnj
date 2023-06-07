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

    type style
	(* an abstraction of attributes such as font and color information.
         * For devices that support styled text, they should maintain a stack
         * of styles, with the top of stack being the "current" style.
         * Implementers of this signature should extend it with functions
         * for creating style values.
	 *)

    type token
        (* the device-specific representation of tokens *)

    type device
	(* a device is an abstraction of an output stream.  Its type parameter
         * specifies the application-specific style type.
         *)

  (***** Style operations *****)

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
	(* output a string in the current style to the device *)

    val token : device * token -> unit

    val flush : device -> unit
	(* if the device is buffered, then flush any buffered output *)

  end
