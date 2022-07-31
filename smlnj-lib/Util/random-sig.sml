(* random-sig.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature RANDOM =
  sig

    type rand
	(* the internal state of a random number generator *)

    val rand : (int * int) -> rand
	(* create rand from initial seed *)

    val toString : rand -> string
    val fromString : string -> rand
        (* convert state to and from string
         * fromString raises Fail if its argument
         * does not have the proper form.
         *)

    val randInt : rand -> int
	(* generate ints uniformly in [minInt,maxInt] *)

    val randNat : rand -> int
	(* generate ints uniformly in [0,maxInt] *)

    val randReal : rand -> real
	(* generate reals uniformly in [0.0,1.0) *)

    val randRange : (int * int) -> rand -> int
	(* randRange (lo,hi) generates integers uniformly [lo,hi].
	 * Raises Fail if hi < lo.
	 *)

  end; (* RANDOM *)

