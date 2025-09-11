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
 * that has been specialized for 32-bit machine-word sizes.
 *)

structure LEB128 : LEB128 = struct

    structure W = Word
    structure W8 = Word8
    structure NW = NativeWord   (* = Word32 *)
    structure W64 = Word64

    type ('ty, 'src) decoder =
          (Word8.word, 'src) StringCvt.reader -> 'src -> ('ty * 'src) option
    type 'ty encoder = 'ty -> Word8Vector.vector

    val mask : W8.word = 0wx7f
    val signBit : W8.word = 0wx40
    val contBit : W8.word = 0wx80

    (* conversions to/from native (32-bit) words and bytes *)
    fun byteToNW b = NW.fromLarge(W8.toLarge b)
    fun byteFromNW b = W8.fromLarge(NW.toLarge b)

    (* conversions to/from 64-bit words and bytes *)
    fun byteToW64 b = W64.fromLarge(W8.toLarge b)
    fun byteFromW64 b = W8.fromLarge(W64.toLarge b)

(* TODO: need to figure out overflow and sign extension
    fun decodeInt getB = ??

    fun decodeInt64 getB = let
          fun toInt64 w = Int64.fromLarge(Word64.toIntX w)
          fun lp (inS, n, shift) = (case getB inS
                 of SOME(b, rest) => let
                      val slice = W64.andb(byteToW64 b, 0wx7f)
                      val n = n + Int64.fromLarge(W64.toLargeIntX(W64.<<(slice, shift)))
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
       of SOME(n, inS') => SOME(NativeInt.fromLarge n, inS')
        | NONE => NONE
      (* end case *))
fun decodeInt64 getB inS = (case decodeIntInf getB inS
       of SOME(n, inS') => SOME(Int64.fromLarge n, inS')
        | NONE => NONE
      (* end case *))

    fun decodeWord getB = let
          (* check for too-large inputs *)
          fun chkOverflow (shift, slice) =
                if (shift < 0w63) then ()
                else if (shift = 0w63)
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

    fun decodeNativeWord getB = ??

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

    fun decodeUIntInf getB = let
          fun lp (inS, w, shift) = (case getB inS
                 of SOME(b, rest) => let
                      val slice = W8.toLargeInt (W8.andb (b, mask))
                      val n = IntInf.<<(slice, shift) + n
                      in
                        if (W8.andb(b, contBit) <> 0w0)
                          then lp (rest, w, shift + 0w7)
                          else SOME(w, rest)
                      end
                  | NONE => NONE
                (* end case *))
          in
            fn inS => lp (inS, 0, 0w0)
          end

    fun sizeOfNativeInt (n : Int.int) = let
          val value = NW.fromLargeInt(NativeInt.toLarge n)
          val sign = if (n < 0) then NW.fromInt ~1 else 0w0
          fun lp (value, sz) = let
                val b = NW.andb(value, 0wx7f)
                val value = NW.~>>(value, 0w7)
                in
                  if (value <> sign)
                  orelse (NW.andb(NW.xorb(b, sign), 0wx40) <> 0w0)
                    then lp (value, sz + 1)
                    else sz
                end
          in
            lp (value, 1)
          end

    fun sizeOfInt (n : Int.int) = sizeOfNativeInt (NativeInt.fromInt n)

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

    fun sizeOfNativeWord (w : NW.word) = let
          fun lp (0w0, sz) = sz
            | lp (w, sz) = lp (NW.>>(w, 0w7), sz+1)
          in
            lp (NW.>>(w, 0w7), 1)
          end

    fun sizeOfWord w = sizeOfNativeWord (NW.fromLarge (Word.toLarge w))

    fun sizeOfWord64 (w : Word64.word) = let
          fun lp (0w0, sz) = sz
            | lp (w, sz) = lp (Word64.>>(w, 0w7), sz+1)
          in
            lp (Word64.>>(w, 0w7), 1)
          end

    fun sizeOfUIntInf (n : IntInf.int) = if (n < 0)
          then raise Domain
          else let
            fun lp (0, sz) = sz
              | lp (n, sz) = lp (IntInf.~>>(n, 0w7), sz+1)
            in
              lp (IntInf.~>>(n, 0w7), 1)
            end

    fun encodeInt64 (n : Int64.int) = let
          val sz = sizeOfInt64 n
          val buf = Unsafe.Word8Vector.create sz
          fun encode (n, idx) = let
                val b = byteFromW64(W64.andb(n, 0wx7f))
                val n = W64.~>>(n, 0w7)
                val noMore = ((n = 0w0) andalso (W8.andb(b, 0wx40) = 0w0))
                      orelse ((n = W64.notb 0w0) andalso (W8.andb(b, signBit) <> 0w0))
                val b = if noMore then b else W8.orb(b, contBit)
                in
                  Unsafe.Word8Vector.update(buf, idx, b);
                  if noMore then buf else encode (n, idx+1)
                end
          in
            encode (W64.fromLargeInt(Int64.toLarge n), 0)
          end

    fun encodeInt (n : Int.int) = encodeInt64 (Int64.fromInt n)

    fun encodeIntInf (n : IntInf.int) = let
          val sz = sizeOfIntInf n
          val buf = Unsafe.Word8Vector.create sz
          fun encode (n, idx) = let
                val b = Word8.fromLargeInt(IntInf.andb(n, 0x7f))
                val n = IntInf.~>>(n, 0w7)
                val noMore = ((n = 0) andalso (W8.andb(b, 0wx40) = 0w0))
                      orelse ((n = ~1) andalso (W8.andb(b, signBit) <> 0w0))
                val b = if noMore then b else W8.orb(b, contBit)
                in
                  Unsafe.Word8Vector.update(buf, idx, b);
                  if noMore then buf else encode (n, idx+1)
                end
          in
            encode (n, 0)
          end

    fun encodeNativeWord (w : NW.word) = let
          val sz = sizeOfNativeWord w
          val buf = Unsafe.Word8Vector.create sz
          fun encode (n, idx) = let
                val b = byteFromW64(W64.andb(n, 0wx7f))
                val n = W64.>>(n, 0w7)
                in
                  if (n = 0w0)
                    then (Unsafe.Word8Vector.update(buf, idx, b); buf)
                    else (
                      Unsafe.Word8Vector.update(buf, idx, W8.orb(b, contBit));
                      encode (n, idx+1))
                end
          in
            encode (w, 0)
          end

    fun encodeWord (w : word) = encodeWord64 (W64.fromLarge (Word.toLarge w))

    fun encodeWord64 (w : W64.word) = let
          val sz = sizeOfWord64 w
          val buf = Unsafe.Word8Vector.create sz
          fun encode (n, idx) = let
                val b = byteFromW64(W64.andb(n, 0wx7f))
                val n = W64.>>(n, 0w7)
                in
                  if (n = 0w0)
                    then (Unsafe.Word8Vector.update(buf, idx, b); buf)
                    else (
                      Unsafe.Word8Vector.update(buf, idx, W8.orb(b, contBit));
                      encode (n, idx+1))
                end
          in
            encode (w, 0)
          end

    fun encodeUIntInf (n : IntInf.int) = let
          (* note that `sizeOfUIntInf` raises `Domain` if `n < 0` *)
          val sz = sizeOfUIntInf n
          val buf = Unsafe.Word8Vector.create sz
          fun encode (n, idx) = let
                val b = Word8.fromLargeInt(IntInf.andb(n, 0x7f))
                val n = IntInf.~>>(n, 0w7)
                in
                  if (n = 0w0)
                    then (Unsafe.Word8Vector.update(buf, idx, b); buf)
                    else (
                      Unsafe.Word8Vector.update(buf, idx, W8.orb(b, contBit));
                      encode (n, idx+1))
                end
          in
            encode (n, 0)
          end

  end
