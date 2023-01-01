(* random-sig.sml
 *
 * An interface to stateful pseudo-random number generators.
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature RANDOM =
  sig

    (* the internal state of a random number generator *)
    type rand

    (* create rand from initial seed *)
    val rand : (int * int) -> rand

    (* create a random state from a list of seeds *)
    val fromList : NativeWord.word list -> rand

    val toBytes : rand -> Word8Vector.vector
    val fromBytes : Word8Vector.vector -> rand
        (* convert state to and from byte vectors.
         * fromBytes raises Fail if its argument
         * does not have the proper form.
         *)

    val toString : rand -> string
    val fromString : string -> rand
        (* convert state to and from string
         * fromString raises Fail if its argument
         * does not have the proper form.
         *)

    val randNativeInt : rand -> NativeInt.int
	(* generate ints uniformly in [0,NativeInt.maxInt] *)

    val randNativeWord : rand -> NativeWord.word
	(* generate ints uniformly in [0w0,maxWord] *)

    val randInt : rand -> int
	(* generate ints uniformly in [minInt,NativeInt.maxInt] *)

    val randWord : rand -> word
	(* generate ints uniformly in [0w0,maxWord] *)

    val randNat : rand -> int
	(* generate ints uniformly in [0w0,NativeInt.maxInt] *)

    val randReal : rand -> real
	(* generate reals uniformly in [0.0,1.0) *)

    val randRange : (int * int) -> rand -> int
	(* randRange (lo,hi) generates integers uniformly [lo,hi].
	 * Raises Fail if hi < lo.
	 *)

  end; (* RANDOM *)
