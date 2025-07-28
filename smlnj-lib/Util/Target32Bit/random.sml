(* random.sml
 *
 * Stateful pseudo-random generation using the 32-bit Mersenne Twister
 * algorithm.
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This code is derived from the 32-bit C version that can be found at
 *
 *      http://www.math.sci.hiroshima-u.ac.jp/m-mat/MT/emt.html
 *
 * That code is covered by the following Copyright and license:
 *
 * Copyright (C) 2004, 2014, Makoto Matsumoto and Takuji Nishimura,
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *
 *   3. The names of its contributors may not be used to endorse or promote
 *      products derived from this software without specific prior written
 *      permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)

structure Random :> RANDOM =
  struct

    structure W32 = Word32
    structure W32A : MONO_ARRAY = struct
        open Array
        type array = W32.word array
        type elem = W32.word
        type vector = W32.word vector
      end

    val && = W32.andb
    val || = W32.orb
    val ^^ = W32.xorb
    val >> = W32.>>
    val << = W32.<<

    (* conversions to default types *)
    fun toWord w = Word.fromLarge(W32.toLarge w)
    fun toInt w = Word.toIntX(Word.fromLarge(W32.toLarge w))
    fun toNat w = Word.toIntX(Word.>>(Word.fromLarge(W32.toLarge w), 0w1))

    infix 0 << >>
    infix 1 || ^^
    infix 2 &&

    val kN = 624
    val kM = 397
    val kMatrixA : W32.word = 0wx9908b0df
    val kUMask : W32.word = 0wx80000000         (* most significant w-r bits *)
    val kLMask : W32.word = 0wx7FFFFFFF         (* least significant r bits *)

    datatype rand = RandState of {
        mt : W32A.array,
        mti : int ref
      }

    fun error (f, msg) = LibBase.failure {module="MTRandom", func=f, msg=msg}

    val bytesPerWord = 4
    val magic = Byte.stringToBytes "MT32"
    val bufLen = 4 + 2 + kN * bytesPerWord (* magic + mti + words *)

    fun w32ToByte w = Word8.fromLarge(W32.toLarge w)
    fun byteToW32 b = W32.fromLarge(Word8.toLarge b)

    fun toBytes (RandState{mti, mt}) = let
          val buf = Word8Buffer.new bufLen
          fun w32ToByte w = Word8.fromLarge(W32.toLarge w)
          val mti' = W32.fromInt(!mti)
          (* add a 32-bit word to the buffer in little-endian order *)
          fun addW32 w = let
                fun lp (i, w) = if (i < bytesPerWord)
                      then (
                        Word8Buffer.add1 (buf, w32ToByte(w && 0wxFF));
                        lp (i+1, w >> 0w8))
                      else ()
                in
                  lp (0, w)
                end
          in
            (* add the magic tag to the front *)
            Word8Buffer.addVec (buf, magic);
            (* then the `mti` value as two bytes in little-endian order*)
            Word8Buffer.add1 (buf, w32ToByte(mti' && 0wxFF));
            Word8Buffer.add1 (buf, w32ToByte(mti' >> 0w8));
            (* add the words in the buffer *)
            W32A.app addW32 mt;
            (* extract the result *)
            Word8Buffer.contents buf
          end

    fun fromBytes vec = if (Word8Vector.length vec <> bufLen)
          then error ("fromBytes", "wrong number of bytes")
          else let
            val SOME(magic', rest) = Word8VectorSlice.getVec(Word8VectorSlice.full vec, 4)
            fun get i = byteToW32(Word8VectorSlice.sub(rest, i))
            val arr = W32A.array(kN, 0w0)
            in
              if (magic' <> magic)
                then error ("fromBytes", "invalid tag")
                else let
                  val mti = W32.toInt(get 0 || (get 1 << 0w8))
                  fun getLp (i, bi) = if (i < kN)
                        then let
                          fun getW32 (j, w) = if (0 <= j)
                                then getW32 (j-1, (w << 0w8) || get (bi + j))
                                else w
                          in
                            W32A.update (arr, i, getW32(bytesPerWord-1, 0w0));
                            getLp (i+1, bi+4)
                          end
                        else ()
                  in
                    if (mti <= kN)
                      then (
                        getLp (0, 2);
                        RandState{mti = ref mti, mt = arr})
                      else error ("fromBytes", "invalid index")
                  end
            end

    (* use Base64 for string encoding *)
    val toString = Base64.encode o toBytes
    fun fromString s = ((fromBytes (Base64.decode s))
          handle Base64.Incomplete => error ("fromString", "incomplete string")
               | Base64.Invalid _ => error ("fromString", "invalid string")
               | ex => raise ex)

    fun init seed = let
          val arr = W32A.array(kN, 0w0)
          fun lp (i, prev) = if (i < kN)
                then let
                  val next = 0w1812433253 * (prev ^^ (prev >> 0w30))
                        + W32.fromInt i
                  in
                    W32A.update(arr, i, next);
                    lp (i+1, next)
                  end
                else ()
          in
            W32A.update(arr, 0, seed);
            lp (1, seed);
            RandState{mt = arr, mti = ref kN}
          end

    fun fromList ws = let
          val rs as RandState{mt, mti} = init 0w19650218
          fun mtAt i = W32A.sub(mt, i)
          (* process `kN` elements taken from repeated `ws` *)
          fun lp1 (_, i, _, 0) = i
            | lp1 ([], i, j, k) = (* restart iteration over `ws` *)
                lp1 (ws, i, 0, k)
            | lp1 (w::wr, i, j, k) = let
                val mt_im1 = mtAt(i-1)
                val x = (mtAt i ^^ ((mt_im1 ^^ (mt_im1 >> 0w30)) * 0w1664525))
                      + w + W32.fromInt j
                val _ = W32A.update(mt, i, x)
                val i = if (i >= kN-1)
                      then (W32A.update(mt, 0, mtAt(kN-1)); 1)
                      else i+1
                in
                  lp1 (wr, i, j+1, k-1)
                end
          val i = if null ws
                then 1
                else lp1 (ws, 1, 0, Int.max(kN, List.length ws))
          (* another pass over the array *)
          fun lp2 (i, 0) = ()
            | lp2 (i, k) = let
                val mt_im1 = mtAt(i-1)
                val x = (mtAt i ^^ ((mt_im1 ^^ (mt_im1 >> 0w30)) * 0w1566083941))
                      - W32.fromInt i
                val _ = W32A.update(mt, i, x)
                val i = if (i >= kN-1)
                      then (W32A.update(mt, 0, mtAt(kN-1)); 1)
                      else i+1
                in
                  lp2 (i, k-1)
                end
          val _ = lp2 (i, kN-1)
          in
            W32A.update(mt, 0, 0wx80000000);  (* MSB is 1; assuring non-zero initial array *)
            rs
          end

    fun randNativeWord (RandState{mt, mti}) = let
          fun mtAt i = W32A.sub(mt, i)
          fun mag01Xor (w, x) = if ((x && 0w1) = 0w0)
                then w
                else w ^^ kMatrixA
          (* for-loop combinator *)
          fun for (init, bnd) body = let
                fun lp i = if (i < bnd) then (body i; lp(i+1)) else i
                in
                  lp init
                end
          val _ = if (!mti >= kN)
                  then let (* generate fresh words for array *)
                    fun update offset i = let
                          val x = (mtAt i && kUMask) || (mtAt(i+1) && kLMask)
                          val y = mag01Xor(mtAt(i+offset) ^^ (x >> 0w1), x)
                          in
                            W32A.update(mt, i, y)
                          end
                    val i = for (0, kN-kM) (update kM)
                    val _ = for (i, kN-1) (update (kM-kN))
                    val x = (mtAt(kN-1) && kUMask) || (mtAt 0 && kLMask)
                    val y = mag01Xor(mtAt(kM-1) ^^ (x >> 0w1), x)
                    in
                      W32A.update(mt, kN-1, y);
                      mti := 0
                    end
                  else ()
          val y = mtAt(!mti)
          val _ = (mti := !mti + 1)
          (* Tempering *)
          val y = y ^^ (y >> 0w11)
          val y = y ^^ ((y << 0w7) && 0wx9d2c5680)
          val y = y ^^ ((y << 0w15) && 0wxefc60000)
          val y = y ^^ (y >> 0w18)
          in
            y
          end

    fun rand (a, b) = fromList [W32.fromInt a, W32.fromInt b]

    fun randNativeInt rs = Int32.fromLarge(W32.toLargeIntX(W32.>>(randNativeWord rs, 0w1)))

  (***** old Random functions *****)

    fun randInt rs = toInt (randNativeWord rs)

    fun randNat rs = toNat (randNativeWord rs)

    fun randWord rs = toWord (randNativeWord rs)

    fun randReal rs = let
          val w = randNativeWord rs
          val r = Real.fromLargeInt(W32.toLargeInt w)
          in
            r * (1.0/4294967296.0)
          end

    fun randRange (i, j) = if j < i
          then error ("randRange", "hi < lo")
          else let
            (* use IntInf arithmetic to avoid overflow *)
            val n = W32.fromLargeInt(IntInf.fromInt j - IntInf.fromInt i + 1)
            in
              fn rs => i + W32.toInt(W32.mod(randNativeWord rs, n))
            end

  end
