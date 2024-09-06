(* render.sig
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

signature RENDER =
  sig

    structure Device : PP_DEVICE

    (* `render {styleMap, tokenMap} dev fmt`
     * renders `fmt`, using the device `dev`.  The `styleMap` and `tokenMap`
     * are used to specify the interpretation of styles and tokens.
     *)
    val render : {
            styleMap : Formatting.style -> Device.style,
            tokenMap : Formatting.token -> Device.token option
          } -> Device.device -> Format.format -> unit

  end (* signature RENDER *)
