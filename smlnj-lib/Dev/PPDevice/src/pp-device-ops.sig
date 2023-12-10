(* pp-device-ops.sig
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* This signature defines the core types and operations that implement
 * a pretty-printing device.  It is the parameter signature of the
 * `DefaultPPDeviceFn` functor.
 *)
signature PP_DEVICE_OPS =
  sig

    (* ===== Types ===== *)

    (* the container for the state of the device *)
    type t

    (* the representation of style attributes for the device *)
    type style

    (* the representation of tokens for the device *)
    type token

    (* ===== Style Operations ===== *)

    (* mark the beginning of a region of styled text by pushing it on
     * the style stack.
     *)
    val pushStyle    : (t * style) -> unit

    (* mark the end of a region of styled text by popping the style stack *)
    val popStyle     : t -> unit

    (* the default style for the device *)
    val defaultStyle : t -> style

    (* ===== Output Operations ===== *)

    (* output a leading indentation of the specified width *)
    val indent : (t * int) -> unit

    (* output non-indentation white space of the specified width *)
    val space : (t * int) -> unit

    (* output a new-line *)
    val newline : t -> unit

    (* output a string in the current style to the device *)
    val string : (t * string) -> unit

    (* output a token *)
    val token : (t * token) -> unit

    (* if the device is buffered, then flush any buffered output *)
    val flush : t -> unit

  end
