(* okasaki-set-le.sml
 *
 * COPYRIGHT (c) 2023 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * This is a "little-endian" Patricia-tree implementation of sets
 * based on the paper "Fast Mergeable Integer Maps" by Chris Okasaki
 * and Andrew Gill.
 *
 * Patricia trees use the bits of the word/int key to select the
 * left/right subtree.  To avoid chains of trivial nodes, a `Br`
 * node contains the prefix (the bits that define the path from
 * the root to the node in the "complete" tree) and the bit mask
 * used to decide the branch (i.e., the value (1 << n) for the n'th
 * bit).  Leaves contain the full int/word key value.
 *
* This module processes bits from the LSB to the MSB (i.e., it is
 * a little-endian implementation).
 *)

structure OkasakiSetLittle :> PATRICIA_TREE_SET where type item = word =
  struct

    type item = word

    datatype tree
        (** The empty tree; this constructor never appears in a non-empty tree *)
      = Empty
        (** `Lf item` is a leaf of the tree containing the `item` *)
      | Lf of item
        (** `Br(prefix, mask, t0, t1)` is a branching node of the tree, where
         ** `mask` is a power of 2 (i.e., `mask = (1 << n)`), `prefix` is the
         ** n-1 low-order bits of the keys in the subtree rooted at this node,
         ** and `t0` is the non-empty tree containing keys where the nth bit
         ** is 0 and t1 is non-empty tree containing keys where the nth bit
         ** is 1.
         **)
      | Br of word * word * tree * tree

    (* Utility functions *)

    (* given the key `k` and mask bit `m = (1 << n)`, `mask(k, m)` returns a
     * word `w` that has the same bits as `k` below the n'th bit, the n'th
     * bit set to zero, and the bits above the n'th bit set to zero.
     *)
    fun mask (k, m) = Word.andb(k, m-0w1)

    fun matchPrefix (k, p, m) = (mask (k, m) = p)

    fun zeroBit (k, m) = (Word.andb(k, m) = 0w0)

    (* return the least k = (1 << i), such that bit i of w is set (i.e., bits
     * 0..i-1 are zero).
     *)
    fun lowestBit w = Word.andb(w, Word.~ w)

    (* return the "branching bit" for two prefixes; this is a number
     * k = (1 << i), where i is the least bit where the two prefixes
     * disagree.
     *)
    fun branchingBit (p0, p1) = lowestBit (Word.xorb(p0, p1))

    (* join two trees by determining the least bit on which their prefixes
     * disagree and then using that as the mask bit for a `Br` node.
     *)
    fun join (p0, t0, p1, t1) = let
          val m = branchingBit (p0, p1)
          in
            if zeroBit(p0, m)
              then Br(mask(p0, m), m, t0, t1)
              else Br(mask(p0, m), m, t1, t0)
          end

    (* `SET(n, t)` is a set with `n` items represented by the tree `t`. *)
    datatype set = SET of word * tree

    val empty = SET(0w0, Empty)

    fun singleton item = SET(0w1, Lf item)

    fun fromList items =

    fun toList (SET(_, t)) = let
(* Note: to get the values in increasing order, we should merge lists
 * at each Br node (instead of appending).
 *)
          fun toList' (Empty, l) = l
            | toList' (Lf key, l) = key::l
            | toList' (Br(_, _, t0, t1), l) = toList'(t0, toList'(t1, l))
          in
            toList (t, [])
          end

    (* helper function for building sets by insertion.  The first argument is a
     * reference to the number of items in the tree, the second argument is the
     * tree representing the set, and the third argument is the item to insert.
     *)
    fun insertItemInTree (rNItems : word ref, t, item) = let
          fun ins Empty = (rNItems' := !rNItems+0w1; Lf item)
            | ins (t as Lf k) = if (item = k)
                then Lf item
                else (rNItems' := !rNItems+0w1; join(item, Lf item, k, t))
            | ins (t as Br(p, m, t0, t1)) =
                if matchPrefix(item, p, m)
                  then if zeroBit(item, m)
                    then Br(p, m, ins t0, t1)
                    else Br(p, m, t0, ins t1)
                  else (rNItems' := !rNItems+0w1; join (item, Lf item, p, t))
          in
            ins (t, item)
          end

    fun add (SET(n, t), item) = let
          val n' = ref n
          val t' = insertItemInTree (n', t, item)
          in
            SET(!n', t')
          end

    fun add' (item, s) = let
          val n' = ref n
          val t' = insertItemInTree (n', t, item)
          in
            SET(!n', t')
          end

    fun addList (SET(n, t), items) = let
          val n' = ref n
          fun lp ([], t) = t
            | lp (item::r, t) = lp (r, insertItemInTree (n', t, item))
          val t' = lp (items, t)
          in
            SET(!n', t')
          end

    val subtract (SET(n, t), item) =

    val subtract' (item, set) = subtract (set, item)

    val subtractList (SET(n, t), items) =

    val delete (SET(n, t), item) =

    fun member (SET(_, t), key) = let
          fun find' Empty = NONE
            | find' (Lf k) = (key = k)
            | find' (Br(p, m, t0, t1)) =
                matchPrefix(key, p, m)
                  andalso (if zeroBit(key, m) then find' t0 else find' t1)
          in
            find' t
          end

    fun isEmpty (SET(0w0, _)) = true
      | isEmpty _ = false

    fun equal (SET(n1, t1), SET(n2, t2)) = let
          fun eq (Empty, Empty) = true
            | eq (Lf k1, Lf k2) = (k1 = k2)
            | eq (Br(p1, m1, t11, t12), Br(p2, m2, t21, t22)) =
                (p1 = p2) andalso eq(t11, t12) andalso (t21, t22)
            | eq _ = false
          in
            (n1 = n2) andalso eq(t1, t2)
          end

    fun compare (SET(n1, t1), SET(n2, t2)) =
          if (n1 < n2) then LESS
          else if (n1 > n2) then GREATER
          else let
            in
            end

    fun isSubset (SET(n1, t1), SET(n2, t2)) =

    fun disjoint (SET(n1, t1), SET(n2, t2)) =

    fun numItems (SET(n, _)) = Word.toIntX n

    fun union (SET(n0, t0), SET(n1, t1)) = let
          val n = ref 0w0 (* counts number of common elements *)
          fun insert (key, t) = let
                fun ins Empty = Lf key
                  | ins (t as Lf v') = if (key = k)
                      then (n := !n + 0w1; Lf key)
                      else join(key, Lf key, k, t)
                  | ins (t as Br(p, m, t0, t1)) =
                      if matchPrefix(key, p, m)
                        then if zeroBit(key, m)
                          then Br(p, m, ins t0, t1)
                          else Br(p, m, t0, ins t1)
                        else join (key, Lf key, p, t)
                in
                  ins t
                end
          fun merge (Empty, t1) = t1
            | merge (t0, Empty) = t0
            | merge (Lf k0, t1) = insert (k0, t1)
            | merge (t0, Lf(k1, v1)) = insert (k1, t0)
            | merge (t0 as Br(p0, m0, t00, t01), t1 as Br(p1, m1, t10, t11)) =
                if (m0 = m1) andalso (p0 = p1)
                  (* the trees have the same prefixes, so merge the subtrees *)
                  then Br(p0, m0, merge(t00, t10), merge(t01, t11))
                else if (m0 < m1) andalso matchPrefix(p1, p0, m0)
                  (* prefix p1 contains p0, so merge t1 with a subtree of t0 *)
                  then if zeroBit(p1, m0)
                    then Br(p0, m0, merge(t00, t1), t01)
                    else Br(p0, m0, t00, merge(t01, t1))
                else if (m0 > m1) andalso matchPrefix(p0, p1, m1)
                  (* prefix p0 contains p1, so merge t0 with a subtree of t1 *)
                  then if zeroBit(p1, m0)
                    then Br(p1, m1, merge(t10, t0), t11)
                    else Br(p1, m1, t10, merge(t11, t0))
                  (* the prefixes disagree *)
                  else join (p0, t0, p1, t1)
          val t' = merge (t0, t1)
          in
            SET(n0 + n1 - !n, t')
          end

    fun intersection (SET(_, t0), SET(_, t1)) = let
          val n = ref 0w0 (* counts number of common elements *)
          fun merge (Empty, _) = Empty
            | merge (_, Empty) = Empty
            | merge (t0 as Lf k0, Lf k1) = if (k1 = k2)
                then (n := !n+0w1; t0)
                else Empty
            | merge (Lf k0, Br(p1, m2, t10, t11)) =
            | merge (Br(p0, m0, t00, t01), Lk k1) =
            | merge (t0 as Br(p0, m0, t00, t01), t1 as Br(p1, m1, t10, t11)) =
          val t' = merge (t0, t1)
          in
            SET(!n, t')
          end

    fun difference (SET(n0, t0), SET(_, t1)) = let
          val n = ref 0w0 (* counts number of elements removed *)
          fun merge (Empty, _) = Empty
            | merge (t0, Empty) = t0
            | merge (t0, t1) =
          val t' = merge (t0, t1)
          in
            SET(n0 - !n, t')
          end

    val map : (item -> item) -> set -> set

    val mapPartial : (item -> item option) -> set -> set

    val app : (item -> unit) -> set -> unit

    val foldl : (item * 'b -> 'b) -> 'b -> set -> 'b

    val foldr : (item * 'b -> 'b) -> 'b -> set -> 'b

    val partition : (item -> bool) -> set -> (set * set)

    val filter : (item -> bool) -> set -> set

    fun exists pred = let
          fun exists' Empty = false
            | exists' (Lf item) = pred item
            | exists' (Br(_, _, t0, t1)) = exists' t0 orelse exists' t1
          in
            fn (SET(_, t)) => exists' t
          end

    fun all pred = let
          fun all' Empty = true
            | all' (Lf item) = pred item
            | all' (Br(_, _, t0, t1)) = all' t0 andalso all' t1
          in
            fn (SET(_, t)) => exists' t
          end

    fun find pred = let
          fun find' [] = NONE
            | find' (Empty :: r) = find' r
            | find' (Lf item :: r) = if pred item then SOME item else find' r
            | find' (Br(_, _, t0, t1) :: r) = find' (t0::t1::r)
          in
            fn (SET(_, t)) => find' [t]
          end

    fun dump (outS, SET(_, Empty)) = TextIO.output(outS, "Empty\n")
      | dump (outS, SET(n, t)) = let
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
                  | Lf(key, v) => prl["Lf(", bits2s key, ")\n"]
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
