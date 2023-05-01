(* swb-random.sml
 *
 * Stateful pseudo-random generation using the 32-bit subtract-with-borrow
 * algorithm from
 *
 *      A New Class of Random Number Generators,
 *      by Marsaglia and Zaman,
 *      Ann. Applied Prob. 1(3), 1991, pp. 462-480.
 *
 * The SWB generator is a 31-bit generator with lags 48 and 8. It has period
 * (2^1487 - 2^247)/105 or about 10^445. In general, these generators are
 * excellent. However, they act locally like a lagged Fibonacci generator
 * and thus have troubles with the birthday test. Thus, we combine this SWB
 * generator with the linear congruential generator (48271*a)mod(2^31-1).
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure SWBRandom : RANDOM_ENGINE =
  struct

(* TODO *)

  end
