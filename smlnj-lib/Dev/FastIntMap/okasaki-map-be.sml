(* okasaki-be.sml
 *
 * COPYRIGHT (c) 2023 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * This is a "git-endian" Patricia-tree implementation of finite maps
 * based on the paper "Fast Mergeable Integer Maps" by Chris Okasaki
 * and Andrew Gill.
 *
 * This module processes bits from the MSB to the LSB (i.e., it is
 * a big-endian implementation).  Note that in the big-endian representation,
 * the leaves are ordered in increasing key order (i.e., the tree can
 * be viewed as a binary-search tree).
 *)

structure OkasakiMapBig :> INT_MAP =
  struct

    (* are we on a 64-bit system? *)
    val is64Bit = (Word.wordSize > 32) (* (Unsafe.wordSize() = 64) *)

    datatype 'a tree
        (** The empty tree; this constructor never appears in a non-empty tree *)
      = Empty
        (** `Lf(key, v)` is a leaf of the tree, where the key maps to the value v. *)
      | Lf of word * 'a
        (** `Br(prefix, mask, t0, t1)` is a branching node of the tree, where
         ** `mask` is a power of 2 (i.e., `mask = (1 << n)`), `prefix` is the
         ** high-order bits of the keys in the subtree rooted at this node,
         ** and `t0` is the non-empty tree containing keys where the nth bit
         ** is 0 and t1 is non-empty tree containing keys where the nth bit
         ** is 1.
         **)
      | Br of word * word * 'a tree * 'a tree

    (* Utility functions *)

    (* given the key `k` and mask bit `m = (1 << n)`, `mask(k, m)` returns a
     * word `w` that has the same bits as `k` above the n'th bit, the n'th
     * bit set to zero, and the bits below the n'th bit set to one.
     *)
    fun mask (k, m) = Word.andb(Word.orb(k, m-0w1), Word.notb m)

    fun matchPrefix (k, p, m) = (mask (k, m) = p)

    fun zeroBit (k, m) = (Word.andb(k, m) = 0w0)

    (* given a word `w`, returns a number `m = (1 << n)`, such that `m & w = m`
     * and no higher bit in `w` is set.
     *)
    fun highestBit w = let
          (* each step doubles the number of high bits set;
           * e.g., 0b1000 ==> 0b1100 ==> 0b1111.
           *)
          val w = Word.orb(w, Word.>>(w, 0w1))
          val w = Word.orb(w, Word.>>(w, 0w2))
          val w = Word.orb(w, Word.>>(w, 0w4))
          val w = Word.orb(w, Word.>>(w, 0w8))
          val w = Word.orb(w, Word.>>(w, 0w16))
          (* this step is conditional based on the size of words *)
          val w = if is64Bit then Word.orb(w, Word.>>(w, 0w32)) else w
          in
            (* `w` is all ones from the highest bit down, so we mask out
             * the low bits by a shift and xor.
             *)
            Word.xorb(w, Word.>>(w, 0w1))
          end

    (* return the "branching bit" for two prefixes; this is a number
     * k = (1 << i), where i is the highest bit where the two prefixes
     * disagree.
     *)
    fun branchingBit (p0, p1) = highestBit (Word.xorb(p0, p1))

    fun join (p0, t0, p1, t1) = let
          val m = branchingBit (p0, p1)
          in
(* QUESTION: can we replace this test with a comparison? *)
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
            | find' (Br(p, m, t0, t1)) = if (key <= p) then find' t0 else find' t1
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

    fun dump fmt (outS, MAP(_, Empty)) = TextIO.output(outS, "Empty\n")
      | dump fmt (outS, MAP(n, t)) = let
          fun bits2s w = StringCvt.padLeft #"0" 20 (Word.fmt StringCvt.BIN w)
          fun pr s = TextIO.output(outS, s)
          fun prl s = TextIO.output(outS, String.concat s)
          fun indent 0 = ()
            | indent 1 = pr "+->"
            | indent n = (pr "|  "; indent(n-1))
          fun prNd (i, nd) = (
                indent i;
                case nd
                 of Empty => raise Fail "impossible"
                  | Lf(key, v) => prl["Lf(", bits2s key, ") => ", fmt v, "\n"]
                  | Br(p, m, t0, t1) => (
                      prl["Br(", bits2s p, ",", bits2s m, "):\n"];
                      prNd(i+1, t0);
                      prNd(i+1, t1))
                (* end case *))
          in
            prl [Word.fmt StringCvt.DEC n, " items:\n"];
            prNd (0, t)
          end

  end
