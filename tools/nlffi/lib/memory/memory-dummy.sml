(*
 * Primitives for "raw" memory access.
 *   Normally, this module would be machine-dependent.
 *   The code you are looking at is just a placeholder, a fake.
 *
 *   (C) 2000, Lucent Technologies, Bell Laboratories
 *
 * author: Matthias Blume (blume@research.bell-labs.com)
 *)
structure CMemory : CMEMORY = struct
    exception OutOfMemory

    type addr = Word32.word
    val null = 0w0 : addr
    fun isNull a = a = null
    infix ++ --
    (* rely on 2's-complement for the following... *)
    fun (a: addr) ++ i = a + Word32.fromInt i
    fun compare (a1, a2) = Word32.compare (a1, a2)
    fun a1 -- a2 = Word32.toIntX (a1 - a2)
    fun bcopy { from: addr, to: addr, bytes: word } = ()
    fun alloc (bytes: word) : addr = raise OutOfMemory
    fun free (a: addr) = ()

    (* most of these types are represented using a bigger size
     * (for lack of the "right" size *)
    type uchar = Word32.word
    type schar = Int32.int
    type ushort = Word32.word
    type sshort = Int32.int
    type uint = Word32.word
    type sint = Int32.int
    type ulong = Word32.word
    type slong = Int32.int
    type float = Real.real
    type double = Real.real

    val addr_size = 0w4
    val char_size = 0w1
    val short_size = 0w2
    val int_size = 0w4
    val long_size = 0w4
    val float_size = 0w4
    val double_size = 0w8

    type 'a load_instr = addr -> 'a
    type 'a store_instr = addr * 'a -> unit

    fun load_addr (a: addr) = 0w0 : addr
    fun load_uchar (a: addr) = 0w0 : uchar
    fun load_ushort (a: addr) = 0w0 : ushort
    fun load_uint (a: addr) = 0w0 : uint
    fun load_ulong (a: addr) = 0w0 : ulong
    fun load_float (a: addr) = 0.0 : float
    fun load_double (a: addr) = 0.0 : double

    fun store_addr (a: addr, x: addr) = ()
    fun store_uchar (a: addr, c: uchar) = ()
    fun store_ushort (a: addr, s: ushort) = ()
    fun store_uint (a: addr, i: uint) = ()
    fun store_ulong (a: addr, l: ulong) = ()
    fun store_float (a: addr, f: float) = ()
    fun store_double (a: addr, d: double) = ()

    local
	fun u2s (mid, u) = let
	    val i = Word32.toLargeIntX u
	in
	    if i >= mid then i - 2 * mid else i
	end
    in
        fun char_u2s (c: uchar) = u2s (128, c)
	fun short_u2s (s: ushort) = u2s (32768, s)
	fun int_u2s (i: uint) = Word32.toLargeIntX i
	fun long_u2s (l: ulong) = Word32.toLargeIntX l
    end

    fun char_s2u (c: schar) = Word32.andb (Word32.fromLargeInt c, 0w255)
    fun short_s2u (s: sshort) = Word32.andb (Word32.fromLargeInt s, 0w65535)
    fun int_s2u (i: sint) = Word32.fromLargeInt i
    fun long_s2u (l: slong) = Word32.fromLargeInt l

    val >> = Word32.>>
    val << = Word32.<<
    val andb = Word32.andb
    val orb = Word32.orb
    val notb = Word32.notb

    fun sext (value, mask) =
	int_u2s (if andb (value, mask) = 0w0 then value else orb (value, mask))

    fun p2i x = x
    fun i2p x = x
end
