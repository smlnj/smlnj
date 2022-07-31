(* rawmem.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Raw memory access primops and raw C calls.
 * (This is for use by ml-nlffi.)
 *
 * author: Matthias Blume (blume@research.bell-labs.com)
 *)

structure RawMemInlineT =
  struct

(* FIXME: we should use PrimTypes.c_pointer for the load/store
 * operations and a target-dependent offset type for the sub/update
 * operations.
 *)

    val w8l  : word32 -> word32           = InLine.raw_load_word8
    val i8l  : word32 -> int32            = InLine.raw_load_int8
    val w16l : word32 -> word32           = InLine.raw_load_word16
    val i16l : word32 -> int32            = InLine.raw_load_int16
    val w32l : word32 -> word32           = InLine.raw_load_word32
    val i32l : word32 -> int32            = InLine.raw_load_int32
    val f32l : word32 -> real             = InLine.raw_load_float32
    val f64l : word32 -> real             = InLine.raw_load_float64
    val w8s  : word32 * word32 -> unit    = InLine.raw_store_word8
    val i8s  : word32 * int32  -> unit    = InLine.raw_store_int8
    val w16s : word32 * word32 -> unit    = InLine.raw_store_word16
    val i16s : word32 * int32  -> unit    = InLine.raw_store_int16
    val w32s : word32 * word32 -> unit    = InLine.raw_store_word32
    val i32s : word32 * int32  -> unit    = InLine.raw_store_int32
    val f32s : word32 * real   -> unit    = InLine.raw_store_float32
    val f64s : word32 * real   -> unit    = InLine.raw_store_float64
    val rawccall : word32 * 'a * 'b -> 'c = InLine.raw_ccall

  (* Allen Leung's additions... *)
    val rawrecord : int -> 'a = InLine.raw_record
    val rawrecord64 : int -> 'a = InLine.raw_record64

    val subw8  : 'a * word32 -> word32 = InLine.raw_sub_word8
    val subi8  : 'a * word32 -> int32  = InLine.raw_sub_int8
    val subw16 : 'a * word32 -> word32 = InLine.raw_sub_word16
    val subi16 : 'a * word32 -> int32  = InLine.raw_sub_int16
    val subw32 : 'a * word32 -> word32 = InLine.raw_sub_word32
    val subi32 : 'a * word32 -> int32  = InLine.raw_sub_int32
    val subf32 : 'a * word32 -> real   = InLine.raw_sub_float32
    val subf64 : 'a * word32 -> real   = InLine.raw_sub_float64

    val updw8  : 'a * word32 * word32 -> unit = InLine.raw_update_word8
    val updi8  : 'a * word32 * int32  -> unit = InLine.raw_update_int8
    val updw16 : 'a * word32 * word32 -> unit = InLine.raw_update_word16
    val updi16 : 'a * word32 * int32  -> unit = InLine.raw_update_int16
    val updw32 : 'a * word32 * word32 -> unit = InLine.raw_update_word32
    val updi32 : 'a * word32 * int32  -> unit = InLine.raw_update_int32
    val updf32 : 'a * word32 * real   -> unit = InLine.raw_update_float32
    val updf64 : 'a * word32 * real   -> unit = InLine.raw_update_float64

  end (* structure RawMemInlineT *)
