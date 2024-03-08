(* patricia-tree-set.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * This is a "big-endian" Patricia-tree implementation of sets
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
 * This module processes bits from the MSB to the LSB (i.e., it is
 * a big-endian implementation).  Note that in the big-endian representation,
 * the leaves are ordered in increasing key order (i.e., the tree can
 * be viewed as a binary-search tree).
 *)

structure PTSet :> PATRICIA_TREE_SET where type Key.key = word =
  struct

    structure Key =
      struct
        type key = word
        val hash = Fn.id
      end

    (* are we on a 64-bit system? *)
    val is64Bit = (Word.wordSize > 32) (* (Unsafe.wordSize() = 64) *)

    type item = Key.key

    datatype tree
        (** The empty tree; this constructor never appears in a non-empty tree *)
      = Empty
        (** `Lf item` is a leaf of the tree containing the `item` *)
      | Lf of item
        (** `Br(prefix, mask, t0, t1)` is a branching node of the tree, where
         ** `mask` is a power of 2 (i.e., `mask = (1 << n)`); `prefix` has the
         ** form "[prefix bits][0][(n-1)*1 bits]", where all of the keys in
         ** the subtree rooted in this node match the prefix bits; `t0` is the
         ** non-empty tree containing keys where the nth bit is 0; and t1 is
         ** a non-empty tree containing keys where the nth bit is 1.  Note
         ** that for any key `k` in `t0`, we have `k <= p`; likewise, for any
         ** `k` in `t1`, we have `p < k`.
         ** We use the notation |p| for the number of prefix bits above the
         ** mask bit.
         **)
      | Br of word * word * tree * tree

    (* Utility functions *)

    (* given the key `k` and mask bit `m = (1 << n)`, `mask(k, m)` returns a
     * word `w` that has the same bits as `k` above the n'th bit, the n'th
     * bit set to zero, and the bits below the n'th bit set to one.
     *)
    fun mask (k, m) = Word.andb(Word.orb(k, m-0w1), Word.notb m)

    (* given a key `k`, prefix `p`, and mask bit `m = (1 << n)`, return true
     * if the prefix matches the masked key.
     *)
    fun matchesPrefix (k, p, m) = (mask (k, m) = p)
    fun notMatchesPrefix (k, p, m) = (mask (k, m) <> p)

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

    (* join two trees by determining the greatest bit on which their prefixes
     * disagree and then using that as the mask bit for a `Br` node.
     *)
    fun join (p0, t0, p1, t1) = let
          val m = highestBit (Word.xorb(p0, p1))
          in
            if (p0 <= p1)
              then Br(mask(p0, m), m, t0, t1)
              else Br(mask(p0, m), m, t1, t0)
          end

    (* `SET(n, t)` is a set with `n` items represented by the tree `t`. *)
    datatype set = SET of word * tree

    fun bits2s w = StringCvt.padLeft #"0" 12 (Word.fmt StringCvt.BIN w)

    fun dumpTree (outS, Empty) = TextIO.output(outS, "Empty\n")
      | dumpTree (outS, t) = let
          fun pr s = TextIO.output(outS, s)
          fun prl s = TextIO.output(outS, String.concat s)
          fun indent 0 = ()
            | indent 1 = pr "+->"
            | indent n = (pr "|  "; indent(n-1))
          fun prNd (i, nd) = (
                indent i;
                case nd
                 of Empty => prl ["Empty\n"] (* should never happen *)
                  | Lf item => prl["Lf(", bits2s item, ")\n"]
                  | Br(p, m, t0, t1) => (
                      prl["Br(", bits2s p, ",", bits2s m, "):\n"];
                      prNd(i+1, t0);
                      prNd(i+1, t1))
                (* end case *))
          in
            prNd (0, t)
          end

    (* print the tree structure of a set to `outS` *)
    fun dump (outS, SET(n, t)) = (
          TextIO.output(outS, String.concat[Word.fmt StringCvt.DEC n, " items:\n"]);
          dumpTree (outS, t))

    (* check that a set is well formed. *)
    fun check (s as SET(0w0, Empty)) = s
      | check (s as SET(n, t)) = let
          val n' = ref 0w0
          fun bad msg = (
                print(concat("#### " :: msg @ ["\n"]));
                dump (TextIO.stdOut, s);
                print "####\n";
                raise Fail(concat msg))
          (* compute a word that is the bits of a prefix above the mask bit and zeros
           * elsewhere.
           *)
          fun keyPrefix (p, m) = Word.andb(p, Word.notb(Word.<<(m, 0w1) - 0w1))
          fun chk (_, Empty) = bad ["unexpected Empty subtree"]
            | chk (kp, Lf item) = (
                n' := !n' + 0w1;
                if (Word.andb(kp, item) <> kp)
                  then bad [
                      "leaf (", bits2s item, ") does not have expected key prefix (",
                      bits2s kp, ")"
                    ]
                  else ())
            | chk (kp, Br(p', m', t0, t1)) = (
                if (Word.andb(kp, p') <> kp)
                  then bad ["prefix mismatch (", bits2s kp, " <> ", bits2s p', ")"]
                else if (Word.andb(kp, Word.orb(m', m'-0w1)) <> 0w0)
                  then bad [
                      "mask (", bits2s m', ") overlaps parent key prefix (",
                      bits2s kp, ")"
                    ]
                else if (Word.andb(p', m') <> 0w0)
                  then bad ["mask overlaps prefix"]
                  else ();
                let val kp' = keyPrefix (p', m')
                in
                  chk (kp', t0);
                  chk (Word.orb(kp', m'), t1)
                end)
          in
            case t
             of Br(p, m, _, _) => if (Word.<<(m, 0w1) = 0w0)
                  (* `m` is highest bit, so the initial prefix is 0 *)
                  then chk (0w0, t)
                  (* initial prefix is bits above `m` from the root node's prefix *)
                  else chk (keyPrefix(p, m), t)
              | _ => chk (0w0, t)
            (* end case *);
            if (!n' <> n)
              then bad [
                  "expected ", Word.fmt StringCvt.DEC n, " items, but found ",
                  Word.fmt StringCvt.DEC (!n'), "\n"
                ]
              else ();
            s
          end

    val empty = SET(0w0, Empty)

    fun singleton item = SET(0w1, Lf item)

    (* helper function for testing if an item is in a tree *)
    fun inTree (item, t) = let
          fun look Empty = false
            | look (Lf k) = (item = k)
            | look (Br(p, m, t0, t1)) =
                matchesPrefix(item, p, m)
                  andalso (if (item <= p) then look t0 else look t1)
          in
            look t
          end

    (* helper function for building sets by insertion.  The first argument is a
     * reference to the number of items in the tree, the second argument is the
     * tree representing the set, and the third argument is the item to insert.
     *)
    fun insertItemInTree (rNItems : word ref, t, item) = let
          fun ins Empty = (rNItems := !rNItems+0w1; Lf item)
            | ins (t as Lf k) = if (item = k)
                then Lf item
                else (rNItems := !rNItems+0w1; join(item, Lf item, k, t))
            | ins (t as Br(p, m, t0, t1)) =
                if matchesPrefix(item, p, m)
                  then if (item <= p)
                    then Br(p, m, ins t0, t1)
                    else Br(p, m, t0, ins t1)
                  else (rNItems := !rNItems+0w1; join (item, Lf item, p, t))
          in
            ins t
          end

    fun fromList items = let
          val n' = ref 0w0
          fun lp ([], t) = SET(!n', t)
            | lp (item::r, t) = lp (r, insertItemInTree (n', t, item))
          in
            lp (items, Empty)
          end

    fun toList (SET(_, t)) = let
          fun toList' (Empty, l) = l
            | toList' (Lf item, l) = item::l
            | toList' (Br(_, _, t0, t1), l) = toList'(t0, toList'(t1, l))
          in
            toList' (t, [])
          end

    fun add (SET(n, t), item) = let
          val n' = ref n
          val t' = insertItemInTree (n', t, item)
          in
            SET(!n', t')
          end

    fun add' (item, SET(n, t)) = let
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

    datatype zipper
     = Root
     | LeftBr of word * word * tree * zipper
     | RightBr of word * word * tree * zipper

    fun zipUpEmpty Root = Empty
      | zipUpEmpty (LeftBr(_, _, t1, z)) = zipUp (z, t1)
      | zipUpEmpty (RightBr(_, _, t0, z)) = zipUp (z, t0)

    and zipUp (Root, t) = t
      | zipUp (LeftBr(p, m, t1, z), t) = zipUp (z, Br(p, m, t, t1))
      | zipUp (RightBr(p, m, t0, z), t) = zipUp (z, Br(p, m, t0, t))

    fun subtractItemFromTree (rNItems : word ref, t, item) = let
          fun find (Empty, _) = t    (* tree is unchanged *)
            | find (Lf k, zip) = if (item = k)
                then (rNItems := !rNItems - 0w1; zipUpEmpty zip)
                else t (* tree is unchanged *)
            | find (Br(p, m, t0, t1), zip) = if matchesPrefix(item, p, m)
                then if (item <= p)
                  then find(t0, LeftBr(p, m, t1, zip))
                  else find(t1, RightBr(p, m, t0, zip))
                else t (* tree is unchanged *)
          in
            find (t, Root)
          end

    fun subtract (SET(n, t), item) = let
          val n' = ref n
          val t' = subtractItemFromTree (n', t, item)
          in
            SET(!n', t')
          end

    fun subtract' (item, SET(n, t)) = let
          val n' = ref n
          val t' = subtractItemFromTree (n', t, item)
          in
            SET(!n', t')
          end

    fun subtractList (SET(n, t), items) = let
          val n' = ref n
          fun lp ([], t) = t
            | lp (item::r, t) = lp (r, subtractItemFromTree (n', t, item))
          val t' = lp (items, t)
          in
            SET(!n', t')
          end

    fun delete (SET(n, t), item) = let
          val n' = ref n
          val t' = subtractItemFromTree (n', t, item)
          in
            if (!n' = n)
              then raise LibBase.NotFound
              else SET(!n', t')
          end

    fun member (SET(_, t), item) = inTree (item, t)

    fun isEmpty (SET(0w0, _)) = true
      | isEmpty _ = false

    fun equal (SET(n1, t1), SET(n2, t2)) = let
          fun eq (Empty, Empty) = true
            | eq (Lf k1, Lf k2) = (k1 = k2)
            | eq (Br(p1, m1, t10, t11), Br(p2, m2, t20, t21)) =
                (p1 = p2) andalso eq(t10, t11) andalso eq(t20, t21)
            | eq _ = false
          in
            (n1 = n2) andalso eq(t1, t2)
          end

    (* compare two sets for order; we first use the size (i.e., smaller sets are always
     * less than bigger sets) and only compare elements for equal-sized sets.
     *)
    fun compare (SET(n1, t1), SET(n2, t2)) =
          if (n1 < n2) then LESS
          else if (n1 > n2) then GREATER
          else let
            fun cmp (Lf item1, Lf item2) = Word.compare(item1, item2)
              | cmp (Lf _, _) = LESS
              | cmp (_, Lf _) = GREATER
              | cmp (Br(p1, m1, t10, t11), Br(p2, m2, t20, t21)) = (
                  case Word.compare(p1, p2)
                   of EQUAL => (case cmp(t10, t20)
                         of EQUAL => cmp(t11, t21)
                          | order => order
                        (* end case *))
                    | order => order
                  (* end case *))
              | cmp _ = EQUAL (* only when n1 = n2 = 0 *)
            in
              cmp (t1, t2)
            end

    fun isSubset (SET(n1, t1), SET(n2, t2)) = let
(* TODO *)
          fun chk (Empty, Empty) = true
            | chk (Empty, _) = true
            | chk (_, Empty) = raise Fail "impossible" (* since n1 <= n2 *)
            | chk (Lf item1, Lf item2) = (item1 = item2)
            | chk (t1 as Br(p1, m1, t10, t11), Br(p2, m2, t20, t21)) =
                if (m1 = m2)
                  (* |p1| = |p2| if the prefixes are the same, then check
                   * the subtrees, otherwise the subtrees are disjoint.
                   *)
                  then (p1 = p2) andalso chk(t10, t20) andalso chk(t11, t21)
                else if (m1 < m2)
                  (* |p1| > |p2| ==> if the prefixes match, then any item in
                   * t1 might be in t2.  We only need to check against one of
                   * t2's subtrees (depending on the value of the m2 bit in p1).
                   *)
                  then matchesPrefix(p2, p1, m1)
                    andalso (if p1 <= p2 then chk(t1, t20) else chk(t1, t21))
                  (* |p2| > |p1| ==> there are items in t1 that cannot be in t2 *)
                  else false
            | chk (Br(p1, m1, t10, t11), t2 as Lf item2) =
                (* first set has more than one item *)
                false
            | chk (Lf item1, t2) = inTree(item1, t2)
          in
            (n1 <= n2) andalso chk(t1, t2)
          end

    fun disjoint (SET(n1, t1), SET(n2, t2)) = let
          fun chk (Lf item1, Lf item2) = (item1 <> item2)
            | chk (t1 as Br(p1, m1, t10, t11), t2 as Br(p2, m2, t20, t21)) =
                if (m1 = m2)
                  (* if the trees have the same prefixes, then check the subtrees *)
                  then (p1 <> p2) orelse (chk(t10, t20) andalso chk(t11, t21))
                else if (m1 < m2)
                  (* |p1| > |p2| ==> t1 is disjoint with one of the subtrees of
                   * t2 (depends on value of p1&m2)
                   *)
                  then matchesPrefix(p1, p2, m2)
                    andalso (if (p1 <= p2) then chk(t10, t2) else chk(t11, t2))
                  (* |p2| > |p1| ==> t2 is disjoint with one of the subtrees of
                   * t1 (depends on value of p2&m1)
                   *)
                  else matchesPrefix(p2, p1, m1)
                    andalso (if (p2 <= p1) then chk(t1, t20) else chk(t1, t21))
            | chk (Br(p1, m1, t10, t11), t2 as Lf item2) =
                (notMatchesPrefix(item2, p1, m1))
                  orelse (if (item2 <= p1) then chk (t10, t2) else chk (t11, t2))
            | chk (t1 as Lf item1, Br(p2, m2, t20, t21)) =
                (notMatchesPrefix(item1, p2, m2))
                  orelse (if (item1 <= p2) then chk (t1, t20) else chk (t1, t21))
            | chk _ = raise Fail "impossible"
          in
            ((n1 = 0w0) orelse (n2 = 0w0)) orelse chk(t1, t2)
          end

    fun numItems (SET(n, _)) = Word.toIntX n

    local
      (* merge two trees where `n` is a reference for counting the number of
       * common elements in the result.
       *)
      fun merge (n, t1, t2) = let
            fun insert (item, t) = let
                  fun ins Empty = Lf item
                    | ins (t as Lf k) = if (item = k)
                        then (n := !n + 0w1; t)
                        else join(item, Lf item, k, t)
                    | ins (t as Br(p, m, t0, t1)) =
                        if matchesPrefix(item, p, m)
                          then if (item <= p)
                            then Br(p, m, ins t0, t1)
                            else Br(p, m, t0, ins t1)
                          else join (item, Lf item, p, t)
                  in
                    ins t
                  end
            fun merge' (Empty, t2) = t2
              | merge' (t1, Empty) = t1
              | merge' (Lf k1, t2) = insert (k1, t2)
              | merge' (t1, Lf k2) = insert (k2, t1)
              | merge' (t1 as Br(p1, m1, t10, t11), t2 as Br(p2, m2, t20, t21)) =
                  if (m1 = m2)
                    then if (p1 = p2)
                      (* the trees have the same prefixes, so merge' the subtrees *)
                      then Br(p1, m1, merge'(t10, t20), merge'(t11, t21))
                      (* the prefixes disagree *)
                      else join (p1, t1, p2, t2)
                  else if (m1 < m2)
                    (* |p1| > |p2| *)
                    then if matchesPrefix(p1, p2, m2)
                      (* prefix p2 contains p1, so merge' t2 with a subtree of t1 *)
                      then if (p1 <= p2)
                        then Br(p1, m1, merge'(t10, t2), t11)
                        else Br(p1, m1, t10, merge'(t11, t2))
                      (* the prefixes disagree *)
                      else join (p1, t1, p2, t2)
                    (* |p1| < |p2| *)
                    else if matchesPrefix(p2, p1, m1)
                      then if (p2 <= p1)
                        then Br(p2, m2, merge'(t20, t1), t21)
                        else Br(p2, m2, t20, merge'(t21, t1))
                      (* the prefixes disagree *)
                      else join (p1, t1, p2, t2)
              in
                merge' (t1, t2)
              end
(*
val merge = fn (n, t1, t2) => let val t = merge(n, t1, t2)
in
  print "## MERGE ## (\n";
  dumpTree (TextIO.stdOut, t1);
  print "## ,\n";
  dumpTree (TextIO.stdOut, t2);
  print "## ) ====>\n";
  dumpTree (TextIO.stdOut, t);
  print "####\n";
  t
end
*)
    in

    fun union (SET(n1, t1), SET(n2, t2)) = let
          val n = ref 0w0 (* counts number of common elements *)
          val t' = merge (n, t1, t2)
          in
            SET(n1 + n2 - !n, t')
          end

    fun intersection (SET(_, t1), SET(_, t2)) = let
          val n = ref 0w0 (* counts number of common elements *)
          fun intersect (Empty, _) = Empty
            | intersect (_, Empty) = Empty
            | intersect (t1 as Lf k1, t2) = if inTree(k1, t2)
                then (n := !n+0w1; t1)
                else Empty
            | intersect (t1, t2 as Lf k2) = if inTree(k2, t1)
                then (n := !n+0w1; t2)
                else Empty
            | intersect (t1 as Br(p1, m1, t10, t11), t2 as Br(p2, m2, t20, t21)) =
                if (m1 = m2)
                  then if (p1 = p2)
                    then merge (n, intersect (t10, t20), intersect (t11, t21))
                    else Empty (* prefixes are disjoint ==> sets are disjoint *)
                else if (m1 < m2)
                  (* |p1| > |p2| *)
                  then if matchesPrefix (p1, m2, p2)
                    then if (p1 <= p2) then intersect (t1, t20) else intersect (t1, t21)
                    else Empty
                (* |p1| < |p2| *)
                else if matchesPrefix (p2, m1, p1)
                  then if (p2 <= p1) then intersect (t10, t2) else intersect (t11, t2)
                  else Empty
          val t' = intersect (t1, t2)
          in
            SET(!n, t')
          end

    fun difference (SET(n1, t1), SET(_, t2)) = let
          val n = ref 0w0 (* counts number of elements removed *)
          fun diff (Empty, _) = Empty
            | diff (t1, Empty) = t1
            | diff (t1 as Lf k1, Lf k2) = if (k1 = k2)
                then (n := !n+0w1; Empty)
                else t1
            | diff (t1 as Lf k1, t2) = if inTree(k1, t2)
                then (n := !n + 0w1; Empty)
                else t1
            | diff (t1 as Br(p1, m1, t10, t11), t2 as Lf k2) =
                if matchesPrefix (k2, p1, m1)
                  then if (k2 <= p1)
                    then merge (n, diff (t10, t2), t11)
                    else merge (n, t10, diff (t11, t2))
                  else t1
            | diff (t1 as Br(p1, m1, t10, t11), t2 as Br(p2, m2, t20, t21)) =
                if (m1 = m2)
                  then if (p1 = p2)
                    (* equal prefixes *)
                    then merge (n, diff(t10, t20), diff(t11, t21))
                    (* disjoint trees *)
                    else t1
                else if (m1 < m2)
                  (* |p1| > |p2| *)
                  then if matchesPrefix (p1, m2, p2)
                  (* items in one of the subtrees of `t2` may be in `t1` *)
                    then if (p2 <= p1)
                      then diff (t1, t20)
                      else diff (t1, t21)
                    (* disjoint trees *)
                    else t1
                (* |p1| < |p2| *)
                else if matchesPrefix (p2, m1, p1)
                  (* items in `t2` may be in one of the subtrees of `t1` *)
                  then if (p1 <= p2)
                    then merge (n, diff (t10, t2), t11)
                    else merge (n, t10, diff (t11, t2))
                  (* disjoint trees *)
                  else t1
          val t' = diff (t1, t2)
          in
            SET(n1 - !n, t')
          end

    end (* local *)

    fun map f (SET(_, t)) = let
          val n' = ref 0w0
          fun mapf (t, t') = (case t
                 of Empty => t'
                  | Lf item => insertItemInTree (n', t', f item)
                  | Br(_, _, t0, t1) => mapf(t1, mapf(t0, t'))
                (* end case *))
          val t' = mapf (t, Empty)
          in
            SET(!n', t')
          end

    fun mapPartial f (SET(_, t)) = let
          val n' = ref 0w0
          fun mapf (t, t') = (case t
                 of Empty => t'
                  | Lf item => (case f item
                       of SOME item' => insertItemInTree (n', t', item')
                        | NONE => t'
                      (* end case *))
                  | Br(_, _, t0, t1) => mapf(t1, mapf(t0, t'))
                (* end case *))
          val t' = mapf (t, Empty)
          in
            SET(!n', t')
          end

    fun app f = let
          fun appf Empty = ()
            | appf (Lf item) = f item
            | appf (Br(_, _, t0, t1)) = (appf t0; appf t1)
          in
            fn (SET(_, t)) => appf t
          end

    fun foldl f = let
          fun fold (Empty, acc) = acc
            | fold (Lf item, acc) = f (item, acc)
            | fold (Br(_, _, t0, t1), acc) = fold (t1, fold (t0, acc))
          in
            fn init => fn (SET(_, t)) => fold (t, init)
          end

    fun foldr f = let
          fun fold (Empty, acc) = acc
            | fold (Lf item, acc) = f (item, acc)
            | fold (Br(_, _, t0, t1), acc) = fold (t0, fold (t1, acc))
          in
            fn init => fn (SET(_, t)) => fold (t, init)
          end

    fun partition pred (SET(_, t)) = let
          val nt = ref 0w0
          val nf = ref 0w0
          fun part (t, (tt, tf)) = (case t
                 of Empty => (tt, tf)
                  | Lf item => if pred item
                      then (insertItemInTree (nt, tt, item), tf)
                      else (tt, insertItemInTree (nf, tf, item))
                  | Br(_, _, t0, t1) => part(t1, part(t0, (tt, tf)))
                (* end case *))
          val (tt, tf) = part (t, (Empty, Empty))
          in
            (SET(!nt, tt), SET(!nf, tf))
          end

    fun filter pred (SET(_, t)) = let
          val n' = ref 0w0
          fun filt (t, t') = (case t
                 of Empty => t'
                  | Lf item => if pred item
                      then insertItemInTree (n', t', item)
                      else t'
                  | Br(_, _, t0, t1) => filt(t1, filt(t0, t'))
                (* end case *))
          val t' = filt (t, Empty)
          in
            SET(!n', t')
          end

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
            fn (SET(_, t)) => all' t
          end

    fun find pred = let
          fun find' [] = NONE
            | find' (Empty :: r) = find' r
            | find' (Lf item :: r) = if pred item then SOME item else find' r
            | find' (Br(_, _, t0, t1) :: r) = find' (t0::t1::r)
          in
            fn (SET(_, t)) => find' [t]
          end

  end
