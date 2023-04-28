(* text-render.sml
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * A generic renderer for simple pretty printing to a simple text consumer.
 *)

structure TextRenderer : sig

    (* `render (fmt, consumer, lw)` renders the pretty-printer format `fmt`
     * to simple text that is fed to the text `consumer`.  The `lw`
     * parameter specifies the line width used to control line breaks
     * when rendering `fmt`.
     *)
    val render : PrettyPrint.format * (string -> unit) * int -> unit

  end = struct

    structure TextDev =
      struct
        type style = unit
        type token = unit
        datatype device = Dev of {
            out : string -> unit,
            lw : int
          }
        fun pushStyle _ = ()
        fun popStyle _ = ()
        fun defaultStyle _ = ()
        fun lineWidth (Dev{lw, ...}) = SOME lw
        fun setLineWidth _ = raise Fail "impossible"
        fun maxIndent _ = NONE
        fun setMaxIndent _ = raise Fail "impossible"
        fun textWidth _ = NONE
        fun setTextWidth _ = raise Fail "impossible"
        (***** Output operations *****)
        fun space (Dev{out, ...}, n) = let
              val sp10 = "          "
              fun lp n = if (n <= 0) then ()
                    else if (n < 10) then out(substring(sp10, 0, n))
                    else (out sp10; lp (n-10))
              in
                lp n
              end
        val indent = space
        fun newline (Dev{out, ...}) = out "\n"
        fun string (Dev{out, ...}, s) = out s
        fun token _ = ()
        fun flush _ = ()
      end

    structure R = RenderFn (TextDev)

    fun render (fmt, consumer, lw) = R.render {
            dev = TextDev.Dev{out = consumer, lw = lw},
            styleMap = R.nullMap,
            tokenMap = R.nullMap
          } fmt

  end
