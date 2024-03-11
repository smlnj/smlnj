(* time-pt-set.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature SET =
  sig

    type set

    val empty : set
    val add : set * word -> set
    val numItems : set -> int

  end


functor TimeSetFn (S : SET) : sig

    val randSrc : unit -> Random.rand

    val timeAdd : Random.rand -> {sz : int, nIters : int} -> Time.time * int

  end = struct

    structure R = Random

    fun randSrc () = Random.rand (17, 42)

    fun timeAdd r {sz, nIters} = let
          fun lp (i, nOps) = if i < nIters
                then let
                  fun lp2 (s, nOps) = if (S.numItems s < sz)
                        then let
                          val item = R.randWord r
                          val s' = S.add (s, item)
                          in
                            lp2 (s', nOps+1)
                          end
                        else nOps
                  in
                    lp(i+1, lp2 (S.empty, nOps))
                  end
                else nOps
          val t0 = Time.now()
          val nOps = lp (0, 0)
          val t1 = Time.now()
          in
            (Time.-(t1, t0), nOps)
          end

  end

structure TimePTSet = TimeSetFn (PTSet)

structure TimeRBSet = TimeSetFn (WordRedBlackSet)
