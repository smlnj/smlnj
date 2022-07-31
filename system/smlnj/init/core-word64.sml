(* core-word64.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This module defines multiplication, division, and modulo operations,
 * as well as impure conversions for 64-bit words on 32-bit targets.
 * The other Word64 operations are either directly supported by the
 * compiler or are implemented in the Target32Bit/int64.sml file.
 *)

structure CoreWord64 =
  struct

    local
      infix 7 *
      val op * : word32 * word32 -> word32 = InLine.word32_mul
      infix 6 + -
      val op + : word32 * word32 -> word32 = InLine.word32_add
      val op - : word32 * word32 -> word32 = InLine.word32_sub
      infix 5 << >>
      val op << : word32 * word -> word32 = InLine.word32_lshift
      val op >> : word32 * word -> word32 = InLine.word32_rshiftl
      infix 5 ++
      val op ++ : word32 * word32 -> word32 = InLine.word32_orb
      infix 5 &
      val op & : word32 * word32 -> word32 = InLine.word32_andb
      infix 4 > <>
      val op > = InLine.word32_gt
      val op <> = InLine.word32_neq

      val extern : word64 -> word32 * word32 = InLine.word64_to_pair
      val intern : word32 * word32 -> word64 = InLine.word64_from_pair

      val unsigned_word32_to_int : word32 -> int = InLine.unsigned_word32_to_int
      val signed_word32_to_int : word32 -> int = InLine.signed_word32_to_int
      val copy_word32_to_int32 : word32 -> int32 = InLine.copy_word32_to_int32

  (* from Hacker's Delight (Figure 8.1); this version does not use conditionals
   * and is about 7% faster.
   *)
      fun mul64 (a, b) = let
	    val (hi1, lo1) = extern a
	    val (hi2, lo2) = extern b
	    val a1 = (lo1 >> 0w16) val a0 = (lo1 & 0wxffff)
	    val a3 = (hi1 >> 0w16) val a2 = (hi1 & 0wxffff)
	    val b1 = (lo2 >> 0w16) val b0 = (lo2 & 0wxffff)
	    val b3 = (hi2 >> 0w16) val b2 = (hi2 & 0wxffff)
	    val t = a0 * b0;
	    val acc0 = (t & 0wxffff)
	    val t = a1 * b0 + (t >> 0w16)
	    val acc1 = (t & 0wxffff)
	    val t = a2 * b0 + (t >> 0w16)
	    val acc2 = (t & 0wxffff)
	    val t = a3 * b0 + (t >> 0w16)
	    val acc3 = (t & 0wxffff)
	    val t = a0 * b1 + acc1
	    val acc1 = (t & 0wxffff)
	    val t = a1 * b1 + acc2 + (t >> 0w16)
	    val acc2 = (t & 0wxffff)
	    val t = a2 * b1 + acc3 + (t >> 0w16)
	    val acc3 = (t & 0wxffff)
	    val t = a0 * b2 + acc2
	    val acc2 = (t & 0wxffff)
	    val t = a1 * b2 + acc3 + (t >> 0w16)
	    val acc3 = (t & 0wxffff)
	    val t = a0 * b3 + acc3
	    val acc3 = (t & 0wxffff)
	    in
	      intern((acc3 << 0w16) ++ acc2, (acc1 << 0w16) ++ acc0)
	    end

      local
	structure CII = CoreIntInf
	val up = CII.copy64Inf
        val dn = CII.truncInf64
      in
      (* This is even more inefficient than doing it the hard way,
       * but I am lazy... *)
      fun div64 (x, y) = dn (CII.div (up x, up y))
      fun mod64 (x, y) = dn (CII.mod (up x, up y))
      end

      fun lift2 f (x, y) = intern (f (extern x, extern y))

    in
(* QUESTION: should these functions take pairs of words as arguments? *)

    val op * = mul64
    val div = lift2 div64
    val mod = lift2 mod64

    fun toInt w = (case extern w
	   of (0w0, lo) => unsigned_word32_to_int lo
	    | _ => raise Assembly.Overflow
	  (* end case *))
    fun toIntX w = signed_word32_to_int (#2 (extern w))

  (* hook needed to support fused conversions to int32 *)
    fun toInt32 w = let
	  val (hi, lo) = extern w
	  in
	    if (hi <> 0w0) orelse (lo > 0wx7fffffff)
	      then raise Assembly.Overflow
	      else copy_word32_to_int32 lo
	  end
    fun toInt32X w = copy_word32_to_int32 (#2 (extern w))

    end (* local *)

  end (* structure CoreWord64 *)
