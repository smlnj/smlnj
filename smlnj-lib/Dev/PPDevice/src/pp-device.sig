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

    (* an abstraction of attributes such as font and color information.
     * For devices that support styled text, they should maintain a stack
     * of styles, with the top of stack being the "current" style.
     * Implementers of this signature should extend it with functions
     * for creating style values.
     *)
    type style

    (* the device-specific representation of tokens *)
    type token

    (* a device is an abstraction of an output stream.  Its type parameter
     * specifies the application-specific style type.
     *)
    type device

  (***** Style operations *****)

    (* push/pop a style from the devices style stack.  A pop on an
     * empty style stack is a nop.
     *)
    val pushStyle : (device * style) -> unit
    val popStyle  : device -> unit

    (* `withStyle (dev, sty, f)` switches the device's style to `sty` and then
     * executes `f()`, restores the device's style and returns the result of
     * applying `f`.  Note that if `f()` raises an exception, the original style
     * will **not** be restored.
     *)
    val withStyle : device * style * (unit -> 'a) -> 'a

    (* the default style for the device, which should be the current style,
     * if the style stack is empty.
     *)
    val defaultStyle : style

    (* a constant function that always returns `defaultStyle` *)
    val defaultStyleMap : 'a -> style

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

    (* the suggested maximum width of indentation; `NONE` is interpreted as no
     * limit and is the default.
     *)
    val maxIndent : device -> int option
    val setMaxIndent : device * int option -> unit


  (***** Output operations *****)

    (* output an indentation of the given width to the device *)
    val indent : (device * int) -> unit

    (* output some number of spaces to the device *)
    val space : (device * int) -> unit

    (* output a new-line to the device *)
    val newline : device -> unit

    (* output a string in the current style to the device *)
    val string : (device * string) -> unit

    (* out put a device-supported token *)
    val token : device * token -> unit

    (* if the device is buffered, then flush any buffered output *)
    val flush : device -> unit

  end
