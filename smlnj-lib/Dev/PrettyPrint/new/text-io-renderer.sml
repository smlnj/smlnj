(* text-io-renderer.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure TextIORenderer : sig

    (* `render {tokenMap, width} outS`
     * returns a function for rendering formats to the `TextIO` outstream `outS`.
     * The `width` argument specifies the line width and the `tokenMap` argument
     * specifies the interpretation of tokens.  There is no styling support.
     *)
    val render : {
            tokenMap : Formatting.token -> SimpleTextIODev.token option,
            width : int
          } -> TextIO.outstream -> Format.format -> unit

  end = struct

    structure D = SimpleTextIODev

    structure Render = RenderFn (D)

    fun render {tokenMap, width} outS =
          Render.render
            {styleMap = D.defaultStyleMap, tokenMap = tokenMap}
            (D.openDev {dst = outS, wid = width})

  end
