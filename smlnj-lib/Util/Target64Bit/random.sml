(* random.sml
 *
 * Stateful pseudo-random generation using the 64-bit Mersenne Twister
 * algorithm.
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This code is derived from the 64-bit C version that can be found at
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

    structure W64 = Word64

    (* the following structure should be replaced with a packed
     * implementation once it is available.
     *)
    structure W64A : MONO_ARRAY = struct
        open Array
        type array = W64.word array
        type elem = W64.word
        type vector = W64.word vector
      end

    val && = W64.andb
    val || = W64.orb
    val ^^ = W64.xorb
    val >> = W64.>>
    val << = W64.<<

    (* conversions to default types *)
    fun toWord w = Word.fromLarge(W64.toLarge w)
    fun toInt w = Word.toIntX(Word.fromLarge(W64.toLarge w))
    fun toNat w = Word.toIntX(Word.>>(Word.fromLarge(W64.toLarge w), 0w1))

    infix 0 << >>
    infix 1 || ^^
    infix 2 &&

    val kNN = 312
    val kMM = 156
    val kMatrixA : W64.word = 0wxB5026F5AA96619E9
    val kUMask : W64.word = 0wxFFFFFFFF80000000 (* Most significant 33 bits *)
    val kLMask : W64.word = 0wx7FFFFFFF         (* Least significant 31 bits *)

    datatype rand = RandState of {
        mt : W64A.array,
        mti : int ref
      }

    fun error (f, msg) = LibBase.failure {module="MTRandom", func=f, msg=msg}

    val bytesPerWord = 8
    val magic = Byte.stringToBytes "MT64"
    val bufLen = 4 + 2 + kNN * bytesPerWord (* magic + mti + words *)

    fun toBytes (RandState{mti, mt}) = let
          val buf = Word8Buffer.new bufLen
          fun w64ToByte w = Word8.fromLarge w
          val mti' = W64.fromInt(!mti)
          (* add a 64-bit word to the buffer in little-endian order *)
          fun addW64 w = let
                fun lp (i, w) = if (i < bytesPerWord)
                      then (
                        Word8Buffer.add1 (buf, w64ToByte(w && 0wxFF));
                        lp (i+1, w >> 0w8))
                      else ()
                in
                  lp (0, w)
                end
          in
            (* add the magic tag to the front *)
            Word8Buffer.addVec (buf, magic);
            (* then the `mti` value as two bytes in little-endian order*)
            Word8Buffer.add1 (buf, w64ToByte(mti' && 0wxFF));
            Word8Buffer.add1 (buf, w64ToByte(mti' >> 0w8));
            (* add the words in the buffer *)
            W64A.app addW64 mt;
            (* extract the result *)
            Word8Buffer.contents buf
          end

    fun fromBytes vec = if (Word8Vector.length vec <> bufLen)
          then error ("fromBytes", "wrong number of bytes")
          else let
            val SOME(magic', rest) = Word8VectorSlice.getVec(Word8VectorSlice.full vec, 4)
            fun get i = Word8.toLarge(Word8VectorSlice.sub(rest, i))
            val arr = W64A.array(kNN, 0w0)
            in
              if (magic' <> magic)
                then error ("fromBytes", "invalid tag")
                else let
                  val mti = W64.toInt(get 0 || (get 1 << 0w8))
                  fun getLp (i, bi) = if (i < kNN)
                        then let
                          fun getW64 (j, w) = if (0 <= j)
                                then getW64 (j-1, (w << 0w8) || get (bi + j))
                                else w
                          in
                            W64A.update (arr, i, getW64(bytesPerWord-1, 0w0));
                            getLp (i+1, bi+8)
                          end
                        else ()
                  in
                    if (mti <= kNN)
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

    fun copy {src, dst} = let
          val RandState{mti=srcMTI, mt=srcMT} = src
          val RandState{mti=dstMTI, mt=dstMT} = dst
          in
            dstMTI := !srcMTI;
            W64A.copy{src = srcMT, dst = dstMT, di = 0}
          end

    fun init seed = let
          val arr = W64A.array(kNN, 0w0)
          fun lp (i, prev) = if (i < kNN)
                then let
                  val next = 0w6364136223846793005 * (prev ^^ (prev >> 0w62))
                        + W64.fromInt i
                  in
                    W64A.update(arr, i, next);
                    lp (i+1, next)
                  end
                else ()
          in
            W64A.update(arr, 0, seed);
            lp (1, seed);
            RandState{mt = arr, mti = ref kNN}
          end

    fun fromList ws = let
          val rs as RandState{mt, mti} = init 0w19650218
          fun mtAt i = W64A.sub(mt, i)
          (* process `kNN` elements taken from repeated `ws` *)
          fun lp1 (_, i, _, 0) = i
            | lp1 ([], i, j, k) = (* restart iteration over `ws` *)
                lp1 (ws, i, 0, k)
            | lp1 (w::wr, i, j, k) = let
                val mt_im1 = mtAt(i-1)
                val x = (mtAt i ^^ ((mt_im1 ^^ (mt_im1 >> 0w62)) * 0w3935559000370003845))
                      + w + W64.fromInt j
                val _ = W64A.update(mt, i, x)
                val i = if (i >= kNN-1)
                      then (W64A.update(mt, 0, mtAt(kNN-1)); 1)
                      else i+1
                in
                  lp1 (wr, i, j+1, k-1)
                end
          val i = if null ws
                then 1
                else lp1 (ws, 1, 0, Int.max(kNN, List.length ws))
          (* another pass over the array *)
          fun lp2 (i, 0) = ()
            | lp2 (i, k) = let
                val mt_im1 = mtAt(i-1)
                val x = (mtAt i ^^ ((mt_im1 ^^ (mt_im1 >> 0w62)) * 0w2862933555777941757))
                      - W64.fromInt i
                val _ = W64A.update(mt, i, x)
                val i = if (i >= kNN-1)
                      then (W64A.update(mt, 0, mtAt(kNN-1)); 1)
                      else i+1
                in
                  lp2 (i, k-1)
                end
          val _ = lp2 (i, kNN-1)
          in
            W64A.update(mt, 0, 0wx8000000000000000);  (* MSB is 1; assuring non-zero initial array *)
            rs
          end

    fun rand (a, b) = fromList [W64.fromInt a, W64.fromInt b]

    fun randNativeWord (RandState{mt, mti}) = let
          fun mtAt i = W64A.sub(mt, i)
          fun mag01Xor (w, x) = if ((x && 0w1) = 0w0)
                then w
                else w ^^ kMatrixA
          (* for-loop combinator *)
          fun for (init, bnd) body = let
                fun lp i = if (i < bnd) then (body i; lp(i+1)) else i
                in
                  lp init
                end
          val _ = if (!mti >= kNN)
                  then let (* generate fresh words for array *)
                    fun update offset i = let
                          val x = (mtAt i && kUMask) || (mtAt(i+1) && kLMask)
                          val y = mag01Xor(mtAt(i+offset) ^^ (x >> 0w1), x)
                          in
                            W64A.update(mt, i, y)
                          end
                    val i = for (0, kNN-kMM) (update kMM)
                    val _ = for (i, kNN-1) (update (kMM-kNN))
                    val x = (mtAt(kNN-1) && kUMask) || (mtAt 0 && kLMask)
                    val y = mag01Xor(mtAt(kMM-1) ^^ (x >> 0w1), x)
                    in
                      W64A.update(mt, kNN-1, y);
                      mti := 0
                    end
                  else ()
          val x = mtAt(!mti)
          val _ = (mti := !mti + 1)
          val x = x ^^ ((x >> 0w29) && 0wx5555555555555555)
          val x = x ^^ ((x << 0w17) && 0wx71D67FFFEDA60000)
          val x = x ^^ ((x << 0w37) && 0wxFFF7EEE000000000)
          val x = x ^^ (x >> 0w43)
          in
            x
          end

    fun randNativeInt rs = Int64.fromLarge(W64.toLargeIntX(W64.>>(randNativeWord rs, 0w1)))

  (***** old Random functions *****)

    fun randInt rs = toInt (randNativeWord rs)

    fun randNat rs = toNat (randNativeWord rs)

    fun randWord rs = toWord (randNativeWord rs)

    fun randReal rs = let
          val w = randNativeWord rs
          val r = Real.fromLargeInt(W64.toLargeIntX(W64.>>(w, 0w11)))
          in
            r * (1.0/9007199254740992.0)
          end

    fun randRange (i, j) = if j < i
          then error ("randRange", "hi < lo")
          else let
            (* use IntInf arithmetic to avoid overflow *)
            val n = W64.fromLargeInt(IntInf.fromInt j - IntInf.fromInt i + 1)
            in
              fn rs => i + W64.toInt(W64.mod(randNativeWord rs, n))
            end

  end
