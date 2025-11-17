(* num-format32.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * The word to string conversion for the native word and int types
 * on 64-bit machines.
 *)

structure NumFormat32 : sig

    val fmtWord : StringCvt.radix -> Word32.word -> string
    val fmtInt  : StringCvt.radix -> Int32.int -> string

  end = struct

    fun fmtWord radix = let
          val fmtWord' = NumFormat64.fmtWord radix
          in
            fn w => fmtWord' (InlineT.Word32.toLarge w)
          end

    fun fmtInt radix = let
          val fmtInt' = NumFormat64.fmtInt radix
          in
            fn n => fmtInt' (InlineT.Int64.fromLarge (InlineT.Int32.toLarge n))
          end

  end
