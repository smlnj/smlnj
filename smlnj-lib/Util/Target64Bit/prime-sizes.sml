(* target64-prim-sizes.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * A list of prime numbers for sizing hash tables, etc. on 64-bit targets.
 *)

structure PrimeSizes : sig

    val pick : int -> int

  end = struct

  (* This is a sequence of prime numbers; each number is approx. sqrt(2)
   * larger than the previous one in the series.  The list is organized
   * into sublists to make searches faster.
   *)
    val primes = [
	    (47,	[11, 13, 17, 23, 37, 47]),
	    (367,	[67, 97, 131, 191, 257, 367]),
	    (2897,	[521, 727, 1031, 1451, 2053, 2897]),
	    (23173,	[4099, 5801, 8209, 11587, 16411, 23173]),
	    (185369,	[32771, 46349, 65537, 92683, 131101, 185369]),
	    (1482919,	[262147, 370759, 524309, 741457, 1048583, 1482919]),
	    (2097169,	[2097169])
	  ]

    fun pick i = let
	  fun f [] = raise Fail "PrimeSizes.pick: out of sequences"
	    | f [(p, _)] = p
	    | f ((hi, l)::r) = if (i < hi) then g l else f r
	  and g [] = raise Fail "PrimeSizes.pick: out of primes in sequence"
	    | g [p] = p
	    | g (p::r) = if (i < p) then p else g r
	  in
	    f primes
	  end

  end
