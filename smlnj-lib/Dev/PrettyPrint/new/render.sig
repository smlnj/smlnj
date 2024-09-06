(* render.sig
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

signature RENDER =
  sig

    structure Device : PP_DEVICE

    (* `render {dev, styleMap, tokenMap} fmt`
     * renders `fmt`, using the device `dev`.  The `styleMap` and `tokenMap`
     * are used to specify the interpretation of styles and tokens.
     *)
    val render : {
            dev : Device.device,
            styleMap : Formatting.style -> Device.style,
            tokenMap : Formatting.token -> Device.token option
          } -> Format.format -> unit

(** TODO
    (* compute the dimensions of the result of rendering a format *)
    val dimensions : PrettyPrint.format -> {
            nr : int,           (* number of rows (i.e., lines) in the render *)
            nc : int,           (* number of columns, including indentation,
                                 * in the render
                                 *)
            maxTextWid : int    (* width of longest line of text (not including
                                 * indentation) in the render
                                 *)
          }
**)

  end (* signature RENDER *)
