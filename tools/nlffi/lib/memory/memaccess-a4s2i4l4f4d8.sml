(* memaccess-a4s2i4l4f4d8.sml
 *
 *   Primitives for "raw" memory access.
 *
 *   x86/Sparc/PPC version:
 *       addr char short  int  long float double
 *       4    1    2      4    4    4     8       (bytes)
 *
 *   (C) 2004 The Fellowship of SML/NJ
 *
 * author: Matthias Blume (blume@tti-c.org)
 *)
structure CMemAccess : CMEMACCESS = struct

    type addr = Word32.word
    val null = 0w0 : addr
    fun isNull a = a = null
    infix ++ --
    (* rely on 2's-complement for the following... *)
    fun (a: addr) ++ i = a + Word32.fromInt i
    val compare = Word32.compare
    fun a1 -- a2 = Word32.toIntX (a1 - a2)

    val addr_size = 0w4
    val char_size = 0w1
    val short_size = 0w2
    val int_size = 0w4
    val long_size = 0w4
    val longlong_size = 0w8
    val float_size = 0w4
    val double_size = 0w8

    val load_addr = RawMemInlineT.w32l
    val load_uchar = RawMemInlineT.w8l
    val load_schar = RawMemInlineT.i8l
    val load_ushort = RawMemInlineT.w16l
    val load_sshort = RawMemInlineT.i16l
    val load_uint = RawMemInlineT.w32l
    val load_sint = RawMemInlineT.i32l
    val load_ulong = RawMemInlineT.w32l
    val load_slong = RawMemInlineT.i32l
    val load_ulonglong = InlineT.Word64.intern o MemAccess64.load2
    val load_slonglong = InlineT.Int64.intern o MemAccess64.load2
    val load_float = RawMemInlineT.f32l
    val load_double = RawMemInlineT.f64l

    val store_addr = RawMemInlineT.w32s
    val store_uchar = RawMemInlineT.w8s
    val store_schar = RawMemInlineT.i8s
    val store_ushort = RawMemInlineT.w16s
    val store_sshort = RawMemInlineT.i16s
    val store_uint = RawMemInlineT.w32s
    val store_sint = RawMemInlineT.i32s
    val store_ulong = RawMemInlineT.w32s
    val store_slong = RawMemInlineT.i32s
    fun store_ulonglong (a, x) = MemAccess64.store2 (a, InlineT.Word64.extern x)
    fun store_slonglong (a, x) = MemAccess64.store2 (a, InlineT.Int64.extern x)
    val store_float = RawMemInlineT.f32s
    val store_double = RawMemInlineT.f64s

    val int_bits = Word.fromInt Word32.wordSize

    (* this needs to be severely optimized... *)
    fun bcopy { from: addr, to: addr, bytes: word } =
	if bytes > 0w0 then
	    (store_uchar (to, load_uchar from);
	     bcopy { from = from + 0w1, to = to + 0w1, bytes = bytes - 0w1 })
	else ()

    (* types used in C calling convention *)
    type cc_addr = Word32.word
    type cc_schar = Int32.int
    type cc_uchar = Word32.word
    type cc_sint = Int32.int
    type cc_uint = Word32.word
    type cc_sshort = Int32.int
    type cc_ushort = Word32.word
    type cc_slong = Int32.int
    type cc_ulong = Word32.word
    type cc_slonglong = Int64.int
    type cc_ulonglong = Word64.word
    type cc_float = Real.real
    type cc_double = Real.real

    (* wrapping and unwrapping for cc types *)
    fun wrap_addr (x : addr) = x : cc_addr
    fun wrap_schar (x : MLRep.Signed.int) = x : cc_schar
    fun wrap_uchar (x : MLRep.Unsigned.word) = x : cc_uchar
    fun wrap_sint (x : MLRep.Signed.int) = x : cc_sint
    fun wrap_uint (x : MLRep.Unsigned.word) = x : cc_uint
    fun wrap_sshort (x : MLRep.Signed.int) = x : cc_sshort
    fun wrap_ushort (x : MLRep.Unsigned.word) = x : cc_ushort
    fun wrap_slong (x : MLRep.Signed.int) = x : cc_slong
    fun wrap_ulong (x : MLRep.Unsigned.word) = x : cc_ulong
    fun wrap_slonglong (x: MLRep.LongLongSigned.int) = x : cc_slonglong
    fun wrap_ulonglong (x: MLRep.LongLongUnsigned.word) = x : cc_ulonglong
    fun wrap_float (x : MLRep.Real.real) = x : cc_float
    fun wrap_double (x : MLRep.Real.real) = x : cc_double

    fun unwrap_addr (x : cc_addr) = x : addr
    fun unwrap_schar (x : cc_schar) = x : MLRep.Signed.int
    fun unwrap_uchar (x : cc_uchar) = x : MLRep.Unsigned.word
    fun unwrap_sint (x : cc_sint) = x : MLRep.Signed.int
    fun unwrap_uint (x : cc_uint) = x : MLRep.Unsigned.word
    fun unwrap_sshort (x : cc_sshort) = x : MLRep.Signed.int
    fun unwrap_ushort (x : cc_ushort) = x : MLRep.Unsigned.word
    fun unwrap_slong (x : cc_slong) = x : MLRep.Signed.int
    fun unwrap_ulong (x : cc_ulong) = x : MLRep.Unsigned.word
    fun unwrap_slonglong (x : cc_slonglong) = x : MLRep.LongLongSigned.int
    fun unwrap_ulonglong (x : cc_ulonglong) = x : MLRep.LongLongUnsigned.word
    fun unwrap_float (x : cc_float) = x : MLRep.Real.real
    fun unwrap_double (x : cc_double) = x : MLRep.Real.real

    fun p2i (x : addr) = x : MLRep.Unsigned.word
    fun i2p (x : MLRep.Unsigned.word) = x : addr
end
