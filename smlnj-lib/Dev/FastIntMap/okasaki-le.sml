(* okasaki-le.sml
 *
 * COPYRIGHT (c) 2023 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * This is the "little-endian" Patricia-tree implementation
 * from the "Fast Mergeable Integer Maps" paper by Chris Okasaki
 * and Andrew Gill.
 *)

structure OkasakiLittle :> INT_MAP =
  struct

    datatype 'a tree
      = Empty
      | Lf of word * 'a
      | Br of word * word * 'a tree * 'a tree

    (* Utility functions *)
    fun mask (k, m) = Word.andb(k, m-0w1)
    fun matchPrefix (k, p, m) = (mask (k, m) = p)
    fun zeroBit (k, m) = (Word.andb(k, m) = 0w0)

    (* return the k = (1 << i), where bit i of w is set, but bits
     * 0..i-1 are zero.
     *)
    fun leastBit w = Word.andb(w, Word.~ w)

    (* return the "branching bit" for two prefixes; this is a number
     * k = (1 << i), where i is the least bit where the two prefixes
     * disagree.
     *)
    fun branchingBit (p0, p1) = leastBit (Word.xorb(p0, p1))

    fun join (p0, t0, p1, t1) = let
          val m = branchingBit (p0, p1)
          in
            if zeroBit(p0, m)
              then Br(mask(p0, m), m, t0, t1)
              else Br(mask(p0, m), m, t1, t0)
          end

    datatype 'a t = MAP of word * 'a tree

    fun numItems (MAP(n, _)) = Word.toIntX n

    val empty = MAP(0w0, Empty)

    fun isEmpty (MAP(0w0, _)) = true
      | isEmpty _ = false

    fun find (MAP(_, t), key) = let
          fun find' Empty = NONE
            | find' (Lf(k, x)) = if (key = k) then SOME x else NONE
            | find' (Br(p, m, t0, t1)) =
                if not(matchPrefix(key, p, m)) then NONE
                else if zeroBit(key, m) then find' t0
                  else find' t1
          in
            find' t
          end

    fun singleton (key, v) = MAP(0w1, Lf(key, v))

    fun insert (MAP(n, t), key, v) = let
          val n' = ref n
          fun ins Empty = (n' := n+0w1; Lf(key, v))
            | ins (t as Lf(k, v')) = if (key = k)
                then Lf(key, v)
                else (n' := n+0w1; join(key, Lf(key, v), k, t))
            | ins (t as Br(p, m, t0, t1)) =
                if matchPrefix(key, p, m)
                  then if zeroBit(key, m)
                    then Br(p, m, ins t0, t1)
                    else Br(p, m, t0, ins t1)
                  else (n' := n+0w1; join (key, Lf(key, v), p, t))
          val t' = ins t
          in
            MAP(!n', t')
          end

    fun listItems (MAP(_, t)) = let
          fun toList (Empty, l) = l
            | toList (Lf(_, v), l) = v::l
            | toList (Br(_, _, t0, t1), l) = toList(t0, toList(t1, l))
          in
            toList (t, [])
          end

    fun unionWith comb (MAP(n0, t0), MAP(n1, t1)) = let
          val n = ref 0w0 (* counts number of common elements *)
          fun swapComb (v0, v1) = comb(v1, v0)
          fun insert (c, key, v, t) = let
                fun ins Empty = Lf(key, v)
                  | ins (t as Lf(k, v')) = if (key = k)
                      then (n := !n + 0w1; Lf(key, c(v, v')))
                      else join(key, Lf(key, v), k, t)
                  | ins (t as Br(p, m, t0, t1)) =
                      if matchPrefix(key, p, m)
                        then if zeroBit(key, m)
                          then Br(p, m, ins t0, t1)
                          else Br(p, m, t0, ins t1)
                        else join (key, Lf(key, v), p, t)
                in
                  ins t
                end
          fun union (Empty, t1) = t1
            | union (t0, Empty) = t0
            | union (Lf(k0, v0), t1) = insert (comb, k0, v0, t1)
            | union (t0, Lf(k1, v1)) = insert (swapComb, k1, v1, t0)
            | union (t0 as Br(p0, m0, t00, t01), t1 as Br(p1, m1, t10, t11)) =
                if (m0 = m1) andalso (p0 = p1)
                  (* the trees have the same prefixes, so merge the subtrees *)
                  then Br(p0, m0, union(t00, t10), union(t01, t11))
                else if (m0 < m1) andalso matchPrefix(p1, p0, m0)
                  (* prefix p1 contains p0, so merge t1 with a subtree of t0 *)
                  then if zeroBit(p1, m0)
                    then Br(p0, m0, union(t00, t1), t01)
                    else Br(p0, m0, t00, union(t01, t1))
                else if (m0 > m1) andalso matchPrefix(p0, p1, m1)
                  (* prefix p0 contains p1, so merge t0 with a subtree of t1 *)
                  then if zeroBit(p1, m0)
                    then Br(p1, m1, union(t10, t0), t11)
                    else Br(p1, m1, t10, union(t11, t0))
                  (* the prefixes disagree *)
                  else join (p0, t0, p1, t1)
          val t' = union (t0, t1)
          in
            MAP(n0 + n1 - !n, t')
          end

  end
