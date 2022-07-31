(* w64-popcount.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This structure implements population count (Basis Proposal 2016-001) for 64-bit
 * words and is used in the Word and Word64 implementations on 64-bit targets.
 *)

structure W64PopCount =
  struct

    local

    structure W64 = InlineT.Word64
    val >> = W64.rshiftl

    in

    fun popCount w = let
        (* pop count of each 2 bits into those 2 bits *)
          val w = w - W64.andb(>>(w, 0w1), 0wx5555555555555555)
        (* pop count of each 4 bits into those 4 bits *)
          val w = W64.andb(w, 0wx3333333333333333)
		+ W64.andb(>>(w, 0w2), 0wx3333333333333333)
        (* pop count of each 8 bits into those 8 bits *)
          val w = W64.andb(w + >>(w, 0w4), 0wx0F0F0F0F0F0F0F0F)
          in
          (* return leftmost 8 bits of w + (w<<8) + (w<<16) + (w<<24) *)
            W64.toIntX (>>(w * 0wx0101010101010101, 0w56))
          end

    end (* local *)

  end
