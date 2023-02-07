(* target64-random-fn.sml
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This functor wraps a stateful pseudo-random generator to match the
 * SML/NJ Library's `RANDOM` signature.  This file contains the 64-bit
 * implementation of the functor.
 *)

functor RandomFn (Gen : RANDOM_ENGINE) : RANDOM =
  struct

    structure W = Word64

    type rand = Gen.state

    fun error (f, msg) = LibBase.failure {module="RandomFn", func=f, msg=msg}

    fun rand (a, b) = Gen.initFromList [W.fromInt a, W.fromInt b]

    val toBytes = Gen.toBytes
    val fromBytes = Gen.fromBytes

    val toString = Base64.encode o Gen.toBytes
    fun fromString s = ((Gen.fromBytes (Base64.decode s))
          handle Base64.Incomplete => error ("fromString", "incomplete string")
               | Base64.Invalid _ => error ("fromString", "invalid string")
               | ex => raise ex)

    fun randInt rs = let
          val w = Gen.genWord rs
          in
            W.toIntX(W.~>>(w, 0w1))
          end

    fun randNat rs = let
          val w = Gen.genWord rs
          in
            W.toIntX(W.>>(w, 0w2))
          end

    fun randWord rs = let
          val w = Gen.genWord rs
          in
            Word.fromLargeWord(W.>>(w, 0w1))
          end

    fun randReal rs = let
          val w = Gen.genWord rs
          val r = real(Int.fromLarge(W.toLargeIntX(W.>>(w, 0w11))))
          in
            r * (1.0/9007199254740992.0)
          end

    fun randRange (i, j) = if j < i
          then error ("randRange", "hi < lo")
          else let
            (* use IntInf arithmetic to avoid overflow *)
            val n = Word64.fromLargeInt(IntInf.fromInt j - IntInf.fromInt i)
            in
              fn rs => i + Word64.toInt(Word64.mod(Gen.genWord rs, n))
            end

  end
