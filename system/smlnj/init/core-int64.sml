(* core-int64.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This module defines multiplication, division, and modulo operations
 * for 64-bit signed integers on 32-bit targets.  The other Int64 operations
 * are directly supported by the compiler or are implemented in the
 * Basis/Implementation/Target32Bit/int64.sml file.
 *)

structure CoreInt64 =
  struct

    local
      structure CII = CoreIntInf

      val extern : word64 -> word32 * word32 = InLine.word64_to_pair
      val intern : word32 * word32 -> word64 = InLine.word64_from_pair

      fun mul64 (x, y) = CII.testInf64 (CII.* (CII.extend64Inf x, CII.extend64Inf y))

      fun div64 (x, y) =
	    CII.testInf64 (CII.div (CII.extend64Inf x, CII.extend64Inf y))

      fun mod64 (x, y) =
	    CII.truncInf64 (CII.mod (CII.extend64Inf x, CII.extend64Inf y))

      fun quot64 (x, y) =
	    CII.testInf64 (CII.quot (CII.extend64Inf x, CII.extend64Inf y))

      fun rem64 (x, y) =
	    CII.truncInf64 (CII.rem (CII.extend64Inf x, CII.extend64Inf y))

      fun lift2 f (x, y) = intern (f (extern x, extern y))

      infix 5 &
      val op & : word32 * word32 -> word32 = InLine.word32_andb
      infix 4 > <>
      val op > = InLine.word32_gt
      val op <> = InLine.word32_neq

      val signed_word32_to_int : word32 -> int = InLine.signed_word32_to_int
      val trunc_word32_to_int : word32-> int = InLine.trunc_word32_to_int
      val copy_word32_to_int32 : word32 -> int32 = InLine.copy_word32_to_int32

    in

    val op * = lift2 mul64
    val op div = lift2 div64
    val op mod = lift2 mod64
    val quot = lift2 quot64
    val rem = lift2 rem64

    fun toInt n = (case extern n
	   of (0w0, lo) => signed_word32_to_int lo
	    | (0wxffffffff, lo) => if (lo > 0wx7fffffff)
		then raise Assembly.Overflow
		else trunc_word32_to_int lo
	    | _ => raise Assembly.Overflow
	  (* end case *))

  (* hook needed to support fused conversions to int32 *)
    fun toInt32 n = (case extern n
	   of (0w0, lo) => if (lo > 0wx7fffffff)
		then raise Assembly.Overflow
		else copy_word32_to_int32 lo
	    | (0wxffffffff, lo) => if (lo > 0wx7fffffff)
		then copy_word32_to_int32 lo
		else raise Assembly.Overflow
	    | _ => raise Assembly.Overflow
	  (* end case *))

    end (* local *)

  end
