(* w32-popcount.sml
 *
 * COPYRIGHT (c) 2016 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This structure implements population count (Basis Proposal 2016-001) for 32-bit
 * words and is used in the Word32 implementation, and in the Word and Word64
 * implementations on 32-bit targets.
 *)

structure W32PopCount =
  struct

    local

    structure W32 = InlineT.Word32
    val >> = W32.rshiftl

    in

    fun popCount w = let
        (* pop count of each 2 bits into those 2 bits *)
          val w = w - W32.andb(>>(w, 0w1), 0wx55555555)
        (* pop count of each 4 bits into those 4 bits *)
          val w = W32.andb(w, 0wx33333333) + W32.andb(>>(w, 0w2), 0wx33333333)
        (* pop count of each 8 bits into those 8 bits *)
          val w = W32.andb(w + >>(w, 0w4), 0wx0F0F0F0F)
          in
          (* return leftmost 8 bits of w + (w<<8) + (w<<16) + (w<<24) *)
            W32.toIntX (>>(w * 0wx01010101, 0w24))
          end

    end (* local *)

  end
