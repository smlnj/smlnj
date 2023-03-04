(* text-io-render.sml
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * A generic renderer for simple pretty printing to a output stream.
 *)

structure TextIORenderer : sig

    (* `render (fmt, outS, lw)` renders the pretty-printer format `fmt` to
     * the output stream `outS`.  The `lw` parameter specifies the
     * line width used to control line breaks when rendering `fmt`.
     *)
    val render : PrettyPrint.format * TextIO.outstream * int -> unit

  end = struct

    fun render (fmt, outS, lw) = (
          TextRenderer.render (fmt, fn s => TextIO.output(outS, s), lw);
          TextIO.flushOut outS)

  end
