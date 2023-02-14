(* object-desc.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * The encoding of object description headers.
 *
 * WARNING: this file must agree with the run-time system values and the
 * values used in the following files:
 *
 *	system/init/core.sml
 *	system/Basis/Implementation/Unsafe/object.sml
 *	runtime/include/tags.h
 *)

structure ObjectDesc :> OBJECT_DESC =
  struct

    structure II = IntInf

    type tag = II.int

  (* taken from runtime/tags.h *)
    val tagWidth = 0w7		(* 5 minor tag bits plus 2 major tag bits *)

    val bitsPerWord = Word.fromInt Target.mlValueSz

  (* one greater than the maximum length field value (sign should be 0) *)
    val maxLength = II.<<(1, bitsPerWord - (tagWidth+0w1))

    val powTagWidth = II.<<(1, tagWidth)

  (* tag values *)
    local
      fun mkTag t = II.orb(II.<<(t, 0w2), 2)
    in
    val tag_record	= mkTag 0
    val tag_vec_hdr	= mkTag 1
    val tag_vec_data	= tag_record
    val tag_arr_hdr	= mkTag 2
    val tag_arr_data	= mkTag 3
    val tag_ref		= tag_arr_data
    val tag_raw		= mkTag 4
    val tag_raw64	= mkTag 5
    val tag_special	= mkTag 6
    end (* local *)

  (* build a descriptor from a tag and length *)
    fun makeDesc (len, t) = II.orb(II.<<(len, tagWidth), t)
    fun makeDesc' (len, t) = II.orb(II.<<(II.fromInt len, tagWidth), t)

  (* array/vector header codes; note that sequences of tagged integers use
   * the next largest size (e.g., 31 ==> 32).
   *)
    val seq_poly : IntInf.int	= 0	(* elements have uniform representation *)
    val seq_word8 : IntInf.int	= 1	(* elements are 8-bit raw integers *)
    val seq_word16 : IntInf.int	= 2	(* elements are 16-bit raw integers *)
    val seq_word32 : IntInf.int	= 3	(* elements are 32-bit raw integers *)
    val seq_word64 : IntInf.int	= 4	(* elements are 64-bit raw integers *)
    val seq_real32 : IntInf.int	= 5	(* elements are 32-bit raw floats *)
    val seq_real64 : IntInf.int	= 6	(* elements are 64-bit raw floats *)

  (* fixed descriptors *)
    val desc_pair = makeDesc(2, tag_record)
    val desc_ref = makeDesc(1, tag_ref)
    val desc_real64 = makeDesc(2, tag_raw64)
    val desc_polyvec = makeDesc(seq_poly, tag_vec_hdr)
    val desc_polyarr = makeDesc(seq_poly, tag_arr_hdr)
    val desc_special = makeDesc(0, tag_special)

  (* length codes for special descriptors *)
    val special_unevaled_susp : IntInf.int	= 0
    val special_evaled_susp : IntInf.int	= 1
    val special_weak : IntInf.int		= 2
    val special_nulled_weak : IntInf.int	= 3

  end;
