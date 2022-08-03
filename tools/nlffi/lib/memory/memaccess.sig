(* memaccess.sig
 *
 *   Primitives for "raw" memory access.
 *
 * Copyright (c) 2004 by The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)
signature CMEMACCESS = sig

    eqtype addr

    val null : addr
    val isNull : addr -> bool
    val ++ : addr * int -> addr
    val -- : addr * addr -> int
    val compare : addr * addr -> order
    val bcopy : { from: addr, to: addr, bytes: word } -> unit

    (* actual sizes of C types (not their ML representations) in bytes *)
    val addr_size : word
    val char_size : word
    val short_size : word
    val int_size : word
    val long_size : word
    val longlong_size : word
    val float_size : word
    val double_size : word

    (* fetching from memory *)
    val load_addr : addr -> addr
    val load_schar : addr -> MLRep.Signed.int
    val load_uchar : addr -> MLRep.Unsigned.word
    val load_sshort : addr-> MLRep.Signed.int
    val load_ushort : addr-> MLRep.Unsigned.word
    val load_sint : addr -> MLRep.Signed.int
    val load_uint : addr -> MLRep.Unsigned.word
    val load_slong : addr -> MLRep.Signed.int
    val load_ulong : addr -> MLRep.Unsigned.word
    val load_slonglong : addr -> MLRep.LongLongSigned.int
    val load_ulonglong : addr -> MLRep.LongLongUnsigned.word
    val load_float : addr -> MLRep.Real.real
    val load_double : addr -> MLRep.Real.real

    (* storing into memory *)
    val store_addr : addr * addr -> unit
    val store_schar : addr * MLRep.Signed.int -> unit
    val store_uchar : addr * MLRep.Unsigned.word -> unit
    val store_sshort : addr * MLRep.Signed.int -> unit
    val store_ushort : addr * MLRep.Unsigned.word -> unit
    val store_sint : addr * MLRep.Signed.int -> unit
    val store_uint : addr * MLRep.Unsigned.word -> unit
    val store_slong : addr * MLRep.Signed.int -> unit
    val store_ulong : addr * MLRep.Unsigned.word -> unit
    val store_slonglong : addr * MLRep.LongLongSigned.int -> unit
    val store_ulonglong : addr * MLRep.LongLongUnsigned.word -> unit
    val store_float : addr * MLRep.Real.real -> unit
    val store_double : addr * MLRep.Real.real -> unit

    val int_bits : word

    (* types used in C calling convention *)
    type cc_addr
    type cc_schar
    type cc_uchar
    type cc_sint
    type cc_uint
    type cc_sshort
    type cc_ushort
    type cc_slong
    type cc_ulong
    type cc_slonglong
    type cc_ulonglong
    type cc_float
    type cc_double

    (* wrapping and unwrapping for cc types *)
    val wrap_addr : addr -> cc_addr
    val wrap_schar : MLRep.Signed.int -> cc_schar
    val wrap_uchar : MLRep.Unsigned.word -> cc_uchar
    val wrap_sint : MLRep.Signed.int -> cc_sint
    val wrap_uint : MLRep.Unsigned.word -> cc_uint
    val wrap_sshort : MLRep.Signed.int -> cc_sshort
    val wrap_ushort : MLRep.Unsigned.word -> cc_ushort
    val wrap_slong : MLRep.Signed.int -> cc_slong
    val wrap_ulong : MLRep.Unsigned.word -> cc_ulong
    val wrap_slonglong : MLRep.LongLongSigned.int -> cc_slonglong
    val wrap_ulonglong : MLRep.LongLongUnsigned.word -> cc_ulonglong
    val wrap_float : MLRep.Real.real -> cc_float
    val wrap_double : MLRep.Real.real -> cc_double

    val unwrap_addr : cc_addr -> addr
    val unwrap_schar : cc_schar -> MLRep.Signed.int
    val unwrap_uchar : cc_uchar -> MLRep.Unsigned.word
    val unwrap_sint : cc_sint -> MLRep.Signed.int
    val unwrap_uint : cc_uint -> MLRep.Unsigned.word
    val unwrap_sshort : cc_sshort -> MLRep.Signed.int
    val unwrap_ushort : cc_ushort -> MLRep.Unsigned.word
    val unwrap_slong : cc_slong -> MLRep.Signed.int
    val unwrap_ulong : cc_ulong -> MLRep.Unsigned.word
    val unwrap_slonglong : cc_slonglong -> MLRep.LongLongSigned.int
    val unwrap_ulonglong : cc_ulonglong -> MLRep.LongLongUnsigned.word
    val unwrap_float : cc_float -> MLRep.Real.real
    val unwrap_double : cc_double -> MLRep.Real.real

    (* unsafe pointer <-> int conversion *)
    val p2i : addr -> MLRep.Unsigned.word
    val i2p : MLRep.Unsigned.word -> addr
end
