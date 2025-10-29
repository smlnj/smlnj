(* leb128.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * LEB128 encoding/decoding for various integer/word types.  See
 *
 *      https://en.wikipedia.org/wiki/LEB128
 *
 * for a description of the encoding.  This file contains an implementation
 * that has been specialized for 64-bit machine-word sizes.
 *)

structure LEB128 : LEB128 = struct

    structure W = Word
    structure W8 = Word8
    structure NW = NativeWord   (* = Word64 *)
    structure W64 = Word64
    structure NInt = NativeInt  (* = Int64 *)

    type ('ty, 'src) decoder =
          (Word8.word, 'src) StringCvt.reader -> 'src -> ('ty * 'src) option
    type ('ty, 'dst) encoder = ('dst * Word8.word -> 'dst) -> ('dst * 'ty) -> 'dst

    val mask : W8.word = 0wx7f
    val signBit : W8.word = 0wx40
    val contBit : W8.word = 0wx80
    val wBits : word = 0w63     (* number of bits in the default word type *)
    val nwBits : word = 0w64    (* number of bits in the native word type *)

    (* conversions to/from native (32-bit) words and bytes *)
    fun byteToNW b = NW.fromLarge(W8.toLarge b)
    fun byteFromNW b = W8.fromLarge(NW.toLarge b)

    (* conversions to/from 64-bit words and bytes *)
    fun byteToW64 b = W64.fromLarge(W8.toLarge b)
    fun byteFromW64 b = W8.fromLarge(W64.toLarge b)

(* TODO: need to figure out overflow and sign extension
    fun decodeNativeInt getB = let
          fun lp (inS, n, shift) = (case getB inS
                 of SOME(b, rest) => let
                      val slice = NW.andb(byteToNW b, 0wx7f)
                      val n = n + NInt.fromLarge(NW.toLargeIntX(W64.<<(slice, shift)))
                      in
                        if b >= contBit
                          then lp (rest, n, shift + 0w7)
                        else if (shift < 0w64) andalso (W8.andb(b, signBit) <> 0w0)
                          (* TODO: sign extend the result *)
                          then SOME(??)
                          else SOME(n, rest)
                      end
                  | NONE => NONE
                (* end case *))
          in
            fn inS => lp (inS, 0w0, 0w0)
          end

    fun decodeInt getB inS = NInt.toInt (decodeNativeInt getB inS)

    val decodeInt64 = decodeNativeInt
*)

    fun decodeIntInf getB = let
          fun lp (inS, n, shift) = (case getB inS
                 of SOME(b, rest) => let
                      val slice = W8.toLargeInt (W8.andb (b, mask))
                      val n = IntInf.<<(slice, shift) + n
                      in
                        if (W8.andb(b, contBit) <> 0w0)
                          then lp (rest, n, shift + 0w7)
                        else if (W8.andb(b, signBit) = 0w0)
                          then SOME(n, rest)
                          (* sign extend the result *)
                          else SOME(n - IntInf.<<(1, shift + 0w7), rest)
                      end
                  | NONE => NONE
                (* end case *))
          in
            fn inS => lp (inS, 0, 0w0)
          end

(* temporary implementation *)
fun decodeInt getB inS = (case decodeIntInf getB inS
       of SOME(n, inS') => SOME(Int.fromLarge n, inS')
        | NONE => NONE
      (* end case *))
fun decodeNativeInt getB inS = (case decodeIntInf getB inS
       of SOME(n, inS') => SOME(NInt.fromLarge n, inS')
        | NONE => NONE
      (* end case *))
val decodeInt64 = decodeNativeInt

    fun decodeWord getB = let
          (* check for too-large inputs *)
          fun chkOverflow (shift, slice) =
                if (shift < wBits-0w1) then ()
                else if (shift = wBits-0w1)
                  then if (W.>>(W.<<(slice, shift), shift) <> slice)
                    then raise Overflow
                    else ()
                else if (slice <> 0w0)
                  then raise Overflow
                  else ()
          fun lp (inS, w, shift) = (case getB inS
                 of SOME(b, rest) => let
                      val slice = W.andb(W.fromLarge(Word8.toLarge b), 0wx7f)
                      val w = W.orb(w, W.<<(slice, shift))
                      in
                        chkOverflow (shift, slice);
                        if b >= contBit
                          then lp (rest, w, shift + 0w7)
                          else SOME(w, rest)
                      end
                  | NONE => NONE
                (* end case *))
          in
            fn inS => lp (inS, 0w0, 0w0)
          end

    fun decodeWord64 getB = let
          (* check for too-large inputs *)
          fun chkOverflow (shift, slice) =
                if (shift < 0w63) then ()
                else if (shift = 0w63)
                  then if (W64.>>(W64.<<(slice, shift), shift) <> slice)
                    then raise Overflow
                    else ()
                else if (slice <> 0w0)
                  then raise Overflow
                  else ()
          fun lp (inS, w, shift) = (case getB inS
                 of SOME(b, rest) => let
                      val slice = W64.andb(byteToW64 b, 0wx7f)
                      val w = W64.orb(w, W64.<<(slice, shift))
                      in
                        chkOverflow (shift, slice);
                        if b >= contBit
                          then lp (rest, w, shift + 0w7)
                          else SOME(w, rest)
                      end
                  | NONE => NONE
                (* end case *))
          in
            fn inS => lp (inS, 0w0, 0w0)
          end

    val decodeNativeWord = decodeWord64

    fun decodeUIntInf getB = let
          fun lp (inS, n, shift) = (case getB inS
                 of SOME(b, rest) => let
                      val slice = W8.toLargeInt (W8.andb (b, mask))
                      val n = IntInf.<<(slice, shift) + n
                      in
                        if (W8.andb(b, contBit) <> 0w0)
                          then lp (rest, n, shift + 0w7)
                          else SOME(n, rest)
                      end
                  | NONE => NONE
                (* end case *))
          in
            fn inS => lp (inS, 0, 0w0)
          end


    fun sizeOfInt64 (n : Int64.int) = let
          val value = Word64.fromLargeInt(Int64.toLarge n)
          val sign = if (n < 0) then Word64.fromInt ~1 else 0w0
          fun lp (value, sz) = let
                val b = Word64.andb(value, 0wx7f)
                val value = Word64.~>>(value, 0w7)
                in
                  if (value <> sign)
                  orelse (Word64.andb(Word64.xorb(b, sign), 0wx40) <> 0w0)
                    then lp (value, sz + 1)
                    else sz
                end
          in
            lp (value, 1)
          end

    fun sizeOfInt (n : Int.int) = sizeOfInt64 (Int64.fromInt n)
    val sizeOfNativeInt = sizeOfInt64

    fun sizeOfIntInf (n : IntInf.int) = let
          val sign : IntInf.int = if (n < 0) then ~1 else 0
          fun lp (value, sz) = let
                val b = IntInf.andb(value, 0x7f)
                val value = IntInf.~>>(value, 0w7)
                in
                  if (value <> sign)
                  orelse (IntInf.andb(IntInf.xorb(b, sign), 0x40) <> 0)
                    then lp (value, sz + 1)
                    else sz
                end
          in
            lp (n, 1)
          end

    fun sizeOfWord64 (w : Word64.word) = let
          fun lp (0w0, sz) = sz
            | lp (w, sz) = lp (Word64.>>(w, 0w7), sz+1)
          in
            lp (Word64.>>(w, 0w7), 1)
          end

    fun sizeOfWord w = sizeOfWord64 (Word64.fromLarge (Word.toLarge w))

    val sizeOfNativeWord = sizeOfWord64

    fun sizeOfUIntInf (n : IntInf.int) = if (n < 0)
          then raise Domain
          else let
            fun lp (0, sz) = sz
              | lp (n, sz) = lp (IntInf.~>>(n, 0w7), sz+1)
            in
              lp (IntInf.~>>(n, 0w7), 1)
            end

    fun encodeNativeInt putB (outS, n : NInt.int) = let
          fun encode (n, outS) = let
                val b = byteFromNW(NW.andb(n, 0wx7f))
                val n = NW.~>>(n, 0w7)
                val noMore = ((n = 0w0) andalso (W8.andb(b, 0wx40) = 0w0))
                      orelse ((n = NW.notb 0w0) andalso (W8.andb(b, signBit) <> 0w0))
                val b = if noMore then b else W8.orb(b, contBit)
                val outS = putB (outS, b)
                in
                  if noMore then outS else encode (n, outS)
                end
          in
            encode (NW.fromLargeInt(NInt.toLarge n), outS)
          end

    fun encodeInt putB (outS, n : Int.int) = encodeNativeInt putB (outS, NInt.fromInt n)
    val encodeInt64 = encodeNativeInt

    fun encodeIntInf putB (outS, n : IntInf.int) = let
          fun encode (n, outS) = let
                val b = Word8.fromLargeInt(IntInf.andb(n, 0x7f))
                val n = IntInf.~>>(n, 0w7)
                val noMore = ((n = 0) andalso (W8.andb(b, 0wx40) = 0w0))
                      orelse ((n = ~1) andalso (W8.andb(b, signBit) <> 0w0))
                val b = if noMore then b else W8.orb(b, contBit)
                val outS = putB (outS, b)
                in
                  if noMore then outS else encode (n, outS)
                end
          in
            encode (n, outS)
          end

    fun encodeNativeWord putB (outS, w : NW.word) = let
          fun encode (n, outS) = let
                val b = byteFromNW(NW.andb(n, 0wx7f))
                val n = NW.>>(n, 0w7)
                in
                  if (n = 0w0)
                    then putB(outS, b)
                    else encode (n, putB (outS, W8.orb(b, contBit)))
                end
          in
            encode (w, outS)
          end

    fun encodeWord putB (outS, w : word) =
          encodeNativeWord putB (outS, NW.fromLarge (Word.toLarge w))
    val encodeWord64 = encodeNativeWord

    fun encodeUIntInf putB (outS, n : IntInf.int) = let
          fun encode (n, outS) = let
                val b = Word8.fromLargeInt(IntInf.andb(n, 0x7f))
                val n = IntInf.~>>(n, 0w7)
                in
                  if (n = 0)
                    then putB(outS, b)
                    else encode (n, putB (outS, W8.orb(b, contBit)))
                end
          in
            if (n < 0) then raise Domain else encode (n, outS)
          end

    (* encode as bytes *)
    local
      fun toBytes (sizeOf : 'ty -> int, encoder : ('ty, int) encoder) (n : 'ty) = let
            val sz = sizeOf n
            val buf = Unsafe.Word8Vector.create sz
            fun putB (idx, b) = (Unsafe.Word8Vector.update(buf, idx, b); idx+1)
            in
              ignore (encoder putB (0, n)); buf
            end
    in
    val intToBytes = toBytes (sizeOfInt, encodeInt)
    val nativeIntToBytes = toBytes (sizeOfNativeInt, encodeNativeInt)
    val int64ToBytes = toBytes (sizeOfInt64, encodeInt64)
    val intInfToBytes = toBytes (sizeOfIntInf, encodeIntInf)
    val wordToBytes = toBytes (sizeOfWord, encodeWord)
    val nativeWordToBytes = toBytes (sizeOfNativeWord, encodeNativeWord)
    val word64ToBytes = toBytes (sizeOfWord64, encodeWord64)
    val uIntInfToBytes = toBytes (sizeOfUIntInf, encodeUIntInf)
    end (* local *)

  end
