(* char-buffer-renderer.sml
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * A generic renderer for simple pretty printing to a character buffer.
 *)

structure CharBufferRenderer : sig

    (* `render (fmt, buf, lw)` renders the pretty-printer format `fmt`
     * to the character buffer `buf`.  The `lw` parameter specifies the
     * line width used to control line breaks when rendering `fmt`.
     *)
    val render : PrettyPrint.format * CharBuffer.buf * int -> unit

  end = struct

    fun render (fmt, buf, lw) =
          TextRenderer.render (fmt, fn s => CharBuffer.addVec(buf, s), lw)

  end
