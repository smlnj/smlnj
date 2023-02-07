(* random-engine-sig.sml
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * A generic interface for a stateful pseudo-random generator.
 *)

signature RANDOM_ENGINE =
  sig

    type state

    (* initialize the state from a single *)
    val init : NativeWord.word -> state

    (* initialize the state from a list of words *)
    val initFromList : NativeWord.word list -> state

    (* generate the next pseudo-random number in the
     * range [0..2^n-1], where n is `NativeWord.wordSize`.
     *)
    val genWord : state -> NativeWord.word

    (* dump the state to a byte vector *)
    val toBytes : state -> Word8Vector.vector

    (* load the state from a byte vector; this will raise `Fail`
     * if the input is invalid.
     *)
    val fromBytes : Word8Vector.vector -> state

    (* copy the source state (`src`) into the destination (`dst`) *)
    val copy : { src : state, dst : state } -> unit

  end
