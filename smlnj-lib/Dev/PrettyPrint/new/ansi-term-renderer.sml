(* text-io-renderer.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure ANSITermRenderer : sig

    (* `render {styleMap, tokenMap, width} outS`
     * returns a function for rendering formats to the `TextIO` outstream `outS`.
     * The `width` argument specifies the line width, and the `tokenMap` and
     * `tokenMap` arguments specify the interpretation of styles and tokens
     * (respectively).
     *)
   val render : {
            styleMap : Formatting.style -> ANSITermDev.style,
            tokenMap : Formatting.token -> ANSITermDev.token option,
            width : int
          } -> TextIO.outstream -> Format.format -> unit

  end = struct

    structure D = ANSITermDev

    structure Render = RenderFn (D)

    fun render {styleMap, tokenMap, width} outS =
          Render.render
            {styleMap = styleMap, tokenMap = tokenMap}
            (D.openDev {dst = outS, wid = width})

  end
