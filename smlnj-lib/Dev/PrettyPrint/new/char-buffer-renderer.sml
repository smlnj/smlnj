(* char-buffer-renderer.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure CharBufferRenderer : sig

    (* `render {tokenMap, width} buf`
     * returns a function for rendering formats to the character buffer `buf`.
     * The `width` argument specifies the line width and the `tokenMap` argument
     * specifies the interpretation of tokens.  There is no styling support.
     *)
    val render : {
            tokenMap : Formatting.token -> CharBufferDev.token option,
            width : int
          } -> CharBuffer.buf -> Format.format -> unit

  end = struct

    structure D = CharBufferDev

    structure Render = RenderFn (D)

    fun render {tokenMap, width} outS =
          Render.render
            {styleMap = D.defaultStyleMap, tokenMap = tokenMap}
            (D.openDev {dst = outS, wid = width})

  end
