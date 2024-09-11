(* target32-rawmem.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Raw memory access primops and raw C calls for 32-bit targets.
 * (This module is for use by ml-nlffi.)
 *
 * author: Matthias Blume (blume@research.bell-labs.com)
 *)

structure RawMemInlineT =
  struct

(* FIXME: we should use PrimTypes.c_pointer for the load/store
 * operations and a target-dependent offset type for the sub/update
 * operations.
 *)
    type c_pointer = word32

    val w8l  : c_pointer -> word32        = InLine.raw_load_word8
    val i8l  : c_pointer -> int32         = InLine.raw_load_int8
    val w16l : c_pointer -> word32        = InLine.raw_load_word16
    val i16l : c_pointer -> int32         = InLine.raw_load_int16
    val w32l : c_pointer -> word32        = InLine.raw_load_word32
    val i32l : c_pointer -> int32         = InLine.raw_load_int32
    val f32l : c_pointer -> real          = InLine.raw_load_float32
    val f64l : c_pointer -> real          = InLine.raw_load_float64
    val w8s  : c_pointer * word32 -> unit = InLine.raw_store_word8
    val i8s  : c_pointer * int32  -> unit = InLine.raw_store_int8
    val w16s : c_pointer * word32 -> unit = InLine.raw_store_word16
    val i16s : c_pointer * int32  -> unit = InLine.raw_store_int16
    val w32s : c_pointer * word32 -> unit = InLine.raw_store_word32
    val i32s : c_pointer * int32  -> unit = InLine.raw_store_int32
    val f32s : c_pointer * real   -> unit = InLine.raw_store_float32
    val f64s : c_pointer * real   -> unit = InLine.raw_store_float64

    (* `rawccall (addr, args, dummy)` specifies a raw C call to the
     * function with address `addr`.  The `args` and `dummy` arguments
     * must have monomorphic type, where `args` are the arguments to
     * the C function and `dummy` is an empty list constrained to
     * a monomorphic type `ty list`, where `ty` specifies the SML type
     * of the C function.  For example, if `cfun` is the address of a
     * C function with prototype `void cfun (int)`, then we can call it
     * as follows:
     *
     *  let
     *  val rcc : c_pointer * Int32.int * (Int32.int -> unit) list -> unit
     *        = rawccall
     *  in
     *    fn arg => rcc(cfun, arg, [])
     *  end
     *
     *)
    val rawccall : c_pointer * 'a * 'b -> 'c = InLine.raw_ccall

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
