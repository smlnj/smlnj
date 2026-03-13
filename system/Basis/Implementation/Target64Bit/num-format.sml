(* num-format.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * The word to string conversion for the default word and int types
 * on 64-bit machines.
 *)

structure NumFormat : sig

    val fmtWord : StringCvt.radix -> word -> string
    val fmtInt  : StringCvt.radix -> int -> string

  end = struct

    fun fmtWord radix = let
          val fmtWord' = NumFormat64.fmtWord radix
          in
            fn w => fmtWord' (InlineT.Word.toWord64 w)
          end

    fun fmtInt radix = let
          val fmtInt' = NumFormat64.fmtInt radix
          in
            fn n => fmtInt' (InlineT.Int64.fromInt n)
          end

  end
