(* object-desc.sig
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Abstract interface to the encoding of object description headers.
 * Note that we use IntInf.int to represent lengths and descriptors so
 * as to be able to support cross compilation from 32-bits to 64-bits.
 *)

signature OBJECT_DESC =
  sig

    type tag

    val tagWidth : word			(* number of bits to hold a tag *)
    val powTagWidth : IntInf.int	(* 2 ^ tagWidth *)
    val maxLength : IntInf.int		(* one greater than max length value *)

  (* tag values *)
    val tag_record : tag
    val tag_ref : tag
    val tag_vec_hdr : tag
    val tag_vec_data : tag
    val tag_arr_hdr : tag
    val tag_arr_data : tag
    val tag_raw : tag		(* word-aligned raw data *)
    val tag_raw64 : tag		(* 64-bit aligned raw data *)
    val tag_special : tag

  (* build a descriptor from a tag and length (in words) *)
    val makeDesc : (IntInf.int * tag) -> IntInf.int
    val makeDesc' : (int * tag) -> IntInf.int

  (* fixed descriptors *)
    val desc_pair : IntInf.int
    val desc_ref : IntInf.int
    val desc_real64 : IntInf.int
    val desc_polyvec : IntInf.int
    val desc_polyarr : IntInf.int
    val desc_special : IntInf.int	(* with 0 length *)

  (* length codes for special descriptors *)
    val special_evaled_susp : IntInf.int
    val special_unevaled_susp : IntInf.int
    val special_weak : IntInf.int
    val special_nulled_weak : IntInf.int

  end;


