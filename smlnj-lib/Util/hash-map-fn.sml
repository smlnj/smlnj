(* hash-map-fn.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

signature HASH_KEY2 =
  sig

    type key

    (* compute a hash of the key *)
    val hash : key -> word

    (* `same (k1, k2)` returns true when the two keys are the same *)
    val same : key * key -> bool

  end

functor HashMapFn (Key : HASH_KEY) : MONO_HASH_MAP =
  struct

    structure Key = Key
    structure A = Array

  (* NOTE: someday we will change the HASH_KEY signature to follow the naming
   * conventions of the SML basis, so we use those names internally to ease future porting.
   *)
    type key = Key.hash_key
    val hash = Key.hashVal
    val same = Key.sameKey

    datatype 'a bucket
      = NIL
      | B of (word * key * 'a * 'a bucket)

    datatype 'a map = MAP of {
	table : 'a bucket array ref,
	nItems : int ref
      }

    fun index (i, sz) = Word.toIntX(Word.andb(i, Word.fromInt sz - 0w1))

    (* minimum and maximum hash table sizes.  We use powers of two for hash table
     * sizes, since that give efficient indexing, and assume a minimum size of 32.
     *)
    val minSize = 32
    val maxSize = MaxHashTableSize.maxSize

    (* round up `n` to the next hash-table size *)
    fun roundUp n = if (n >= maxSize)
	  then maxSize
	  else let
	    fun f i = if (i >= n) then i else f(i + i)
	    in
	      f minSize
	    end

    (* Create a new table; the int is a size hint and the exception
     * is to be raised by find.
     *)
    fun alloc sizeHint = A.array(roundUp sizeHint, NIL)

    (* copy a table to a new table with a new size *)
    fun resizeTable (table, newSz) = let
	  val newArr = A.array (newSz, NIL)
	  fun copy NIL = ()
	    | copy (B(h, key, item, rest)) = let
		val indx = index (h, newSz)
		in
		  A.update (newArr, indx, B(h, key, item, A.sub(newArr, indx)));
		  copy rest
		end
	  in
	    A.app copy table;
	    newArr
	  end

    (* conditionally grow a table; return true if it grew. *)
    fun growTableIfNeeded (table, nItems) = let
	    val arr = !table
	    val sz = A.length arr
	    in
	      if (nItems >= sz)
		then table := resizeTable (arr, sz+sz)
		else ()
	    end

    (* look for a key with hash `h` in a bucket *)
    fun findInBucket (NIL, h, _) = false
      | findInBucket (B(h', key', item', r), h, key) =
          ((h = h') andalso same (key, key')) orelse findInBucket (r, h, key)

    fun mkEmpty sz = MAP{table = ref(alloc sz), nItems = ref 0}

    fun isEmpty (MAP{nItems, ...}) = (!nItems = 0)

    fun numItems (MAP{nItems, ...}) = !nItems

    fun resize (m as MAP{table, nItems}) = let
          (* the prefered size based on the number of items *)
          val dfltSz = roundUp(!nItems)
          (* the actual size *)
          val sz = A.length(!table)
          in
            if (sz < dfltSz)
              then table := resizeTable(!table, dfltSz)
            else if (dfltSz+dfltSz < sz)
              then table := resizeTable(!table, dfltSz)
              else ()
          end

(* QUESTION: should we shrink the table size? *)
    fun clear (MAP{table, nItems}) = (
          A.modify (fn _ => NIL) (!table);
          nItems := 0)

    fun copy (MAP{table=ref tbl, nItems}) = MAP{
	    table = ref(A.tabulate(A.length tbl, fn i => A.sub(tbl, i))),
	    nItems = ref(!nItems)
	  }

    (* `insertWithHash (m, h, key, item)` inserts the `(key, item)` pair into the
     * hashmap `m`, where `h` is the pre-computed hash of `key`.
     *)
    fun insertWithHash (MAP{table, nItems}, h, key, item) = let
	  val arr = !table
	  val indx = index (h, A.length arr)
	  fun look NIL = (
		A.update(arr, indx, B(h, key, item, A.sub(arr, indx)));
		nItems := !nItems + 1;
		ignore (growTableIfNeeded (table, !nItems));
		NIL)
	    | look (B(h', k, v, r)) = if ((h = h') andalso same(key, k))
		then B(h, key, item, r)
		else (case (look r)
		   of NIL => NIL
		    | rest => B(h', k, v, rest)
		  (* end case *))
	  in
	    case (look (A.sub (arr, indx)))
	     of NIL => ()
	      | b => A.update(arr, indx, b)
	    (* end case *)
	  end

    fun insertWithi combine (MAP{table, nItems}, key, item) = let
	  val arr = !table
	  val h = hash key
	  val indx = index (h, A.length arr)
	  fun look NIL = (
		A.update(arr, indx, B(h, key, item, A.sub(arr, indx)));
		nItems := !nItems + 1;
		growTableIfNeeded (table, !nItems);
		NIL)
	    | look (B(h', k, v, r)) = if ((h = h') andalso same(key, k))
		then B(h, key, combine(k, v, item), r)
		else (case (look r)
		   of NIL => NIL
		    | rest => B(h', k, v, rest)
		  (* end case *))
	  in
	    case (look (A.sub (arr, indx)))
	     of NIL => ()
	      | b => A.update(arr, indx, b)
	    (* end case *)
	  end

    fun insertWith combine = insertWithi (fn (_, v1, v2) => combine(v1, v2))

    fun insert (m, key, item) = insertWithHash (m, hash key, key, item)
    fun insert' ((key, item), map) = insert (map, key, item)
    fun insertc map (key, item) = insert (map, key, item)

    fun mkSingleton (key, item) = let
          val m = MAP{table = ref(alloc minSize), nItems = ref 0}
          in
            insert(m, key, item);
            m
          end

    (* `findWithHash (m, h, key)` searches for the `key` in the
     * hashmap `m`, where `h` is the pre-computed hash of `key`.
     *)
    fun findWithHash (MAP{table, ...}, h, key) = let
	  val arr = !table
	  val indx = index (h, A.length arr)
	  fun look NIL = NONE
	    | look (B(h', k, v, r)) = if ((h = h') andalso same(key, k))
		then SOME v
		else look r
	  in
	    look (A.sub (arr, indx))
	  end

    fun find (m, key) = findWithHash (m, hash key, key)

    fun lookup (MAP{table, ...}, key) = let
	  val arr = !table
	  val h = hash key
	  val indx = index (h, A.length arr)
	  fun look NIL = raise LibBase.NotFound
	    | look (B(h', k, v, r)) = if ((h = h') andalso same(key, k))
		then v
		else look r
	  in
	    look (A.sub (arr, indx))
	  end

    fun inDomain (MAP{table, ...}, key) = let
	  val arr = !table
	  val h = hash key
	  val indx = index (h, A.length arr)
	  fun look NIL = false
	    | look (B(h', k, v, r)) = ((h = h') andalso same(key, k)) orelse look r
	  in
	    look (A.sub (arr, indx))
	  end

    fun findAndRemove (MAP{table, nItems}, key) = let
	  val arr = !table
	  val h = hash key
	  val indx = index (h, A.length arr)
          fun look NIL = raise LibBase.NotFound
            | look (B(h', k, v, r)) = if ((h = h') andalso same(key, k))
                then (v, r)
                else let
                  val (v', r') = look r
                  in
                    (v', B(h', k, v, r'))
                  end
          val (v, bucket) = look (A.sub (arr, indx))
          in
            A.update (arr, indx, bucket); SOME v
	  end
            handle _ => NONE

    fun remove (MAP{table, nItems}, key) = let
	  val arr = !table
	  val h = hash key
	  val indx = index (h, A.length arr)
	  fun look NIL = raise LibBase.NotFound
	    | look (B(h', k, v, r)) = if ((h = h') andalso same(key, k))
		then (v, r)
		else let
                  val (item, r') = look r
                  in
                    (item, B(h', k, v, r'))
                  end
	  val (item, bucket) = look (A.sub (arr, indx))
	  in
	    A.update (arr, indx, bucket);
	    nItems := !nItems - 1;
	    item
	  end (* remove *)

    fun listItems (MAP{table=ref tbl, nItems}) = let
	  fun f (_, l, 0) = l
	    | f (~1, l, _) = l
	    | f (i, l, n) = let
		fun g (NIL, l, n) = f (i-1, l, n)
		  | g (B(_, _, v, r), l, n) = g(r, v::l, n-1)
		in
		  g (A.sub(tbl, i), l, n)
		end
	  in
	    f ((A.length tbl) - 1, [], !nItems)
	  end (* listItems *)

    fun listItemsi (MAP{table=ref tbl, nItems}) = let
	  fun f (_, l, 0) = l
	    | f (~1, l, _) = l
	    | f (i, l, n) = let
		fun g (NIL, l, n) = f (i-1, l, n)
		  | g (B(_, k, v, r), l, n) = g(r, (k, v)::l, n-1)
		in
		  g (A.sub(tbl, i), l, n)
		end
	  in
	    f ((A.length tbl) - 1, [], !nItems)
	  end (* listItemsi *)

    fun listKeys (MAP{table=ref tbl, nItems}) = let
	  fun f (_, l, 0) = l
	    | f (~1, l, _) = l
	    | f (i, l, n) = let
		fun g (NIL, l, n) = f (i-1, l, n)
		  | g (B(_, k, _, r), l, n) = g(r, k::l, n-1)
		in
		  g (A.sub(tbl, i), l, n)
		end
	  in
	    f ((A.length tbl) - 1, [], !nItems)
	  end (* listKeys *)

    (* helper function for iterating over the elements of a table *)
    fun apply f (MAP{table, ...}) = let
          fun lp NIL = ()
            | lp (B(h, key, item, r)) = (f(h, key, item); lp r)
          in
            A.app lp (!table)
          end

    fun unionWithi comb (m1, m2) = let
          val n1 = numItems m1
          val sz = roundUp(Int.max(n1, numItems m2))
          val tbl = A.array(sz, NIL)
          (* add an item from `m1` to the new map *)
          fun add (h, key, item) = let
                val idx = index(h, sz)
                in
                  A.update(tbl, idx, B(h, key, item, A.sub(tbl, idx)))
                end
          (* process an item from m1 *)
          fun doItem1 (h, key, item) = (case findWithHash(m2, h, key)
                 of NONE => add(h, key, item)
                  | SOME item' => add(h, key, comb(key, item, item'))
                (* end case *))
          (* the initial table has all of the items from m1 *)
          val nItems = ref n1
          val m' = MAP{table = ref tbl, nItems = nItems}
          (* process an item from m2 *)
          fun doItem2 (h, key, item) = if inDomain(m1, key)
                then () (* item is already in the result *)
                else (
                  nItems := !nItems + 1;
                  add (h, key, item))
          in
            apply doItem1 m1;
            apply doItem2 m2;
            resize m';
            m'
          end

    fun unionWith comb = unionWithi (fn (_, item1, item2) => comb(item1, item2))

    fun intersectWithi comb (m1, m2) = let
          val n1 = numItems m1
          val n2 = numItems m2
          val n = ref 0
          val tbl = if (n1 < n2)
                then let
                  val tbl = alloc n1
                  val sz = A.length tbl
                  fun insert (h, key, item1) = (case findWithHash(m2, h, key)
                         of SOME item2 => let
                              val idx = index(h, sz)
                              val item = comb(key, item1, item2)
                              in
                                n := !n + 1;
                                A.update (tbl, idx, B(h, key, item, A.sub(tbl, idx)))
                              end
                          | _ => ()
                        (* end case *))
                  in
                    apply insert m1;
                    tbl
                  end
                else let
                  val tbl = alloc n2
                  val sz = A.length tbl
                  fun insert (h, key, item2) = (case findWithHash(m1, h, key)
                         of SOME item1 => let
                              val idx = index(h, sz)
                              val item = comb(key, item1, item2)
                              in
                                n := !n + 1;
                                A.update (tbl, idx, B(h, key, item, A.sub(tbl, idx)))
                              end
                          | _ => ()
                        (* end case *))
                  in
                    apply insert m2;
                    tbl
                  end
          val m = MAP{table = ref tbl, nItems = n}
          in
            resize m;
            m
          end

    fun intersectWith comb = intersectWithi (fn (_, item1, item2) => comb(item1, item2))

    fun mergeWithi comb (m1, m2) = let
          val n1 = numItems m1
          val sz = roundUp(Int.max(n1, numItems m2))
          val tbl = A.array(sz, NIL)
          val nItems = ref 0
          val m' = MAP{table = ref tbl, nItems = nItems}
          (* add an item to the new map *)
          fun add (h, key, optItem1, optItem2) = (case comb(key, optItem1, optItem2)
                 of SOME item => let
                      val idx = index(h, sz)
                      in
                        nItems := !nItems + 1;
                        A.update(tbl, idx, B(h, key, item, A.sub(tbl, idx)))
                      end
                  | NONE => ()
                (* end case *))
          (* process an item from m1 *)
          fun doItem1 (h, key, item) = (case findWithHash(m2, h, key)
                 of NONE => add(h, key, SOME item, NONE)
                  | SOME item' => add(h, key, SOME item, SOME item')
                (* end case *))
          (* process an item from m2 *)
          fun doItem2 (h, key, item) = if inDomain(m1, key)
                then () (* item has already been processed *)
                else add(h, key, NONE, SOME item)
          in
            apply doItem1 m1;
            apply doItem2 m2;
            resize m';
            m'
          end

    fun mergeWith comb =
          mergeWithi (fn (_, optItem1, optItem2) => comb(optItem1, optItem2))

    fun equiv rngEq (m1, m2) = let
          val MAP{table=ref a1, nItems=ref n1} = m1
          val MAP{table=ref a2, nItems=ref n2} = m2
          val sz2 = A.length a2
          fun find2 (h1, k1, v1) = let
                fun look NIL = false
                  | look (B(h2, k2, v2, r)) =
                      ((h1 = h2) andalso same(k1, k2) andalso rngEq(v1, v2))
                        orelse look r
                in
                  look (A.sub(a2, index(h1, sz2)))
                end
          fun iter1 NIL = true
            | iter1 (B(h, k, v, r)) = find2(h, k, v) andalso iter1 r
          in
            (n1 = n2) andalso A.exists iter1 a1
          end

    fun extends rngEq (m1, m2) = let
          val MAP{table=ref a1, nItems=ref n1} = m1
          val MAP{table=ref a2, nItems=ref n2} = m2
          val sz1 = A.length a1
          fun find1 (h2, k2, v2) = let
                fun look NIL = false
                  | look (B(h1, k1, v1, r)) =
                      ((h2 = h1) andalso same(k2, k1) andalso rngEq(v1, v2))
                        orelse look r
                in
                  look (A.sub(a1, index(h2, sz1)))
                end
          fun iter2 NIL = true
            | iter2 (B(h, k, v, r)) = find1(h, k, v) andalso iter2 r
          in
            (n2 <= n1) andalso A.exists iter2 a2
          end

    fun app f (MAP{table, ...}) = let
          fun lp NIL = ()
            | lp (B(_, _, item, r)) = (f item; lp r)
          in
            A.app lp (!table)
          end

    fun appi f (MAP{table, ...}) = let
          fun lp NIL = ()
            | lp (B(_, key, item, r)) = (f (key, item); lp r)
          in
            A.app lp (!table)
          end

    fun map f (MAP{table = ref tbl, nItems}) = let
          fun mapf i = let
                fun copyBucket NIL = NIL
                  | copyBucket (B(h, key, item, r)) = B(h, key, f item, copyBucket r)
                in
                  copyBucket (A.sub(tbl, i))
                end
          in
            MAP{table = ref(Array.tabulate(A.length tbl, mapf)), nItems = ref(!nItems)}
          end

    fun mapi f (MAP{table = ref tbl, nItems}) = let
          fun mapf i = let
                fun copyBucket NIL = NIL
                  | copyBucket (B(h, key, item, r)) =
                      B(h, key, f (key, item), copyBucket r)
                in
                  copyBucket (A.sub(tbl, i))
                end
          in
            MAP{table = ref(Array.tabulate(A.length tbl, mapf)), nItems = ref(!nItems)}
          end

    fun fold f init (MAP{table = ref tbl, nItems}) = let
          val sz = A.length tbl
          fun foldf (i, acc) = if (i < sz)
                then let
                  fun foldBucket (NIL, acc) = foldf (i+1, acc)
                    | foldBucket (B(h, key, item, r), acc) =
                        foldBucket (r, f(item, acc))
                  in
                    foldBucket (A.sub(tbl, i), acc)
                  end
                else acc
          in
            foldf (0, init)
          end

    fun foldi f init (MAP{table = ref tbl, nItems}) = let
          val sz = A.length tbl
          fun foldf (i, acc) = if (i < sz)
                then let
                  fun foldBucket (NIL, acc) = foldf (i+1, acc)
                    | foldBucket (B(h, key, item, r), acc) =
                        foldBucket (r, f(key, item, acc))
                  in
                    foldBucket (A.sub(tbl, i), acc)
                  end
                else acc
          in
            foldf (0, init)
          end

    fun filter pred (MAP{table = ref tbl, nItems}) = let
          val n = ref(!nItems)
          fun filterf i = let
                fun copyBucket NIL = NIL
                  | copyBucket (B(h, key, item, r)) = if pred item
                      then B(h, key, item, copyBucket r)
                      else (n := !n - 1; copyBucket r)
                in
                  copyBucket (A.sub(tbl, i))
                end
          in
            MAP{table = ref(Array.tabulate(A.length tbl, filterf)), nItems = n}
          end

    fun filteri pred (MAP{table = ref tbl, nItems}) = let
          val n = ref(!nItems)
          fun mapf i = let
                fun copyBucket NIL = NIL
                  | copyBucket (B(h, key, item, r)) = if pred (key, item)
                      then B(h, key, item, copyBucket r)
                      else (n := !n - 1; copyBucket r)
                in
                  copyBucket (A.sub(tbl, i))
                end
          in
            MAP{table = ref(Array.tabulate(A.length tbl, mapf)), nItems = n}
          end

    fun mapPartial f (MAP{table = ref tbl, nItems}) = let
          val n = ref(!nItems)
          fun mapf i = let
                fun copyBucket NIL = NIL
                  | copyBucket (B(h, key, item, r)) = (case f item
                       of SOME item' => B(h, key, item', copyBucket r)
                        | NONE => (n := !n - 1; copyBucket r)
                      (* end case *))
                in
                  copyBucket (A.sub(tbl, i))
                end
          in
            MAP{table = ref(Array.tabulate(A.length tbl, mapf)), nItems = n}
          end

    fun mapPartiali f (MAP{table = ref tbl, nItems}) = let
          val n = ref(!nItems)
          fun mapf i = let
                fun copyBucket NIL = NIL
                  | copyBucket (B(h, key, item, r)) = (case f (key, item)
                       of SOME item' => B(h, key, item', copyBucket r)
                        | NONE => (n := !n - 1; copyBucket r)
                      (* end case *))
                in
                  copyBucket (A.sub(tbl, i))
                end
          in
            MAP{table = ref(Array.tabulate(A.length tbl, mapf)), nItems = n}
          end

    fun exists pred (MAP{table, ...}) = let
	  fun chk NIL = false
	    | chk (B(_, _, item, r)) = pred item orelse chk r
	  in
	    A.exists chk (!table)
	  end

    fun existsi pred (MAP{table, ...}) = let
	  fun chk NIL = false
	    | chk (B(_, key, item, r)) = pred(key, item) orelse chk r
	  in
	    A.exists chk (!table)
	  end

    fun all pred (MAP{table, ...}) = let
	  fun chk NIL = true
	    | chk (B(_, _, item, r)) = pred item andalso chk r
	  in
	    A.all chk (!table)
	  end

    fun alli pred (MAP{table, ...}) = let
	  fun chk NIL = true
	    | chk (B(_, key, item, r)) = pred(key, item) andalso chk r
	  in
	    A.all chk (!table)
	  end

  end (* functor HashMapFn *)
