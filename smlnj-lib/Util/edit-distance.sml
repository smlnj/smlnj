(* edit-distance.sml
 *
 * Compute the "optimal string alignment" (or  Levenshtein) distance.  We
 * allow four kinds of edits: deletion, insertion, replacement, and
 * transposition of adjacent characters.
 *
 * The implementation is based on the pseudocode for "OSA" distance at
 *
 *      https://en.wikipedia.org/wiki/Damerau–Levenshtein_distance
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure EditDistance : sig

    val editDistance : string * string -> int

  end = struct

(* safe version
    val stringSub = String.sub
    val arraySub = Array.sub
    val arrayUpdate = Array.update
*)
   (* unsafe string operations *)
    val stringSub = Unsafe.CharVector.sub
    val arraySub = Unsafe.Array.sub
    val arrayUpdate = Unsafe.Array.update

    fun min3 (a : int, b, c) = if (a <= b)
          then if (a <= c) then a else c
          else if (b <= c) then b else c

    fun editDistance ("", b) = size b
      | editDistance (a, "") = size a
      | editDistance (a, b) = let
          val na = size a
          val nb = size b
          (* [0..na]x[0..nb] array of costs *)
          val d = Array.array((na+1)*(nb+1), ~1)
          fun get (i, j) = arraySub(d, i*nb + j)
          fun set (i, j, k) = arrayUpdate(d, i*nb + j, k)
          (* compute min cost for position [i,j], where `cost` is the cost
           * of replacement at position [i,j].  Transposition cost is handled
           * separately.
           *)
          fun editCost (i, j, cost) = min3(
                get(i-1, j) + 1,        (* deletion cost *)
                get(i, j-1) + 1,        (* insertion cost *)
                get(i-1, j-1) + cost)   (* substitution cost *)
          (* initialization for d[-, 0] and d[0, -] *)
          val _ = let
                fun init1 i = if (i <= na) then (set(i, 0, i); init1(i+1)) else ()
                fun init2 j = if (j <= nb) then (set(0, j, j); init2(j+1)) else ()
                in
                  init1 0; init2 0
                end
          (* for the first character position in either string, transposition is
           * not an option, so we handle those separately.
           *)
          val a1 = stringSub(a, 0)
          val b1 = stringSub(b, 0)
          val _ = let
                fun update (i, j, cost) = set (i, j, editCost (i, j, cost))
                (* loop for i=1 *)
                fun lp1 j = if (j <= nb)
                      then (
                        if (a1 = stringSub(b, j-1))
                          then update (1, j, 0)
                          else update (1, j, 1);
                        lp1 (j+1))
                      else ()
                (* loop for j=1 *)
                fun lp2 i = if (i <= na)
                      then (
                        if (stringSub(a, i-1) = b1)
                          then update (i, 1, 0)
                          else update (i, 1, 1);
                        lp2 (i+1))
                      else ()
                in
                  lp1 1;  lp2 1
                end
          (* loop for i = 2..na *)
          fun lpi (i, aim1) = if (i <= na)
                then let
                  val ai = stringSub(a, i-1)
                  (* loop for j = 2..nb *)
                  fun lpj (j, bjm1) = if (j <= nb)
                        then let
                          val bj = stringSub(b, j-1)
                          fun body cost = let
                                val dij = editCost(i, j, cost)
                                in
                                  (* check for transposition *)
                                  if (aim1 = bj) andalso (bjm1 = ai)
                                    then set(i, j, Int.min(dij, get(i-2, j-2)))
                                    else set(i, j, dij)
                                end
                          in
                            if (ai = bj) then body 0 else body 1;
                            lpj (j+1, bj)
                          end
                        else ()
                  in
                    lpj (2, b1); lpi (i+1, ai)
                  end
                else ()
          in
            lpi (2, a1);
            (* get the distance *)
            get(na, nb)
          end

  end
