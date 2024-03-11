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
    val union : set * set -> set
    val numItems : set -> int

  end


functor TimeSetFn (S : SET) : sig

    val randSrc : unit -> Random.rand

    val timeAdd : Random.rand -> {sz : int, nIters : int} -> Time.time * int

    val timeUnion : Random.rand -> {n : int, nIters : int} -> Time.time

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

    (* time union operations by first creating a list of 2^n sets of 10 elements each
     * and then doing merging adjacent sets in the list until we have just one set.
     *)
    fun timeUnion r {n, nIters} = let
          val nSets = Word.toIntX(Word.<<(0w1, Word.fromInt n))
          fun mkSet (k, s) = if (k < 10) then mkSet (k+1, S.add(s, R.randWord r)) else s
          fun mkSets () = List.tabulate (nSets, fn _ => mkSet (0, S.empty))
          fun merge [s] = s
            | merge ss = let
                fun mergeAdj (s1::s2::r) = S.union(s1, s2) :: mergeAdj r
                  | mergeAdj ss = ss
                in
                  merge (mergeAdj ss)
                end
          fun lp i = if i < nIters
                then let
                  val ss = mkSets ()
                  val s = merge ss
                  in
                    lp (i+1)
                  end
                else ()
          val t0 = Time.now()
          val () = lp 0
          val t1 = Time.now()
          in
            Time.-(t1, t0)
          end

  end

structure TimePTSet = TimeSetFn (PTSet)

structure TimeRBSet = TimeSetFn (WordRedBlackSet)
