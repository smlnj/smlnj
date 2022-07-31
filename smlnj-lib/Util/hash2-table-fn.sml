(* hash2-table-fn.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Hash tables that are keyed by two keys (in different domains).
 *
 * AUTHOR:  John Reppy
 *	    University of Chicago
 *	    https://cs.uchicago.edu/~jhr
 *)

functor Hash2TableFn (
    structure Key1 : HASH_KEY
    structure Key2 : HASH_KEY
  ) : MONO_HASH2_TABLE = struct

    structure Key1 = Key1
    structure Key2 = Key2

    structure HTRep = HashTableRep

  (* the representation of a double-keyed hash table is two tables
   * that will always hold the same number of items and be the same
   * size.
   *)
    datatype 'a hash_table = TBL of {
	not_found : exn,
	tbl1 : (Key1.hash_key, Key2.hash_key * 'a) HTRep.table ref,
	tbl2 : (Key2.hash_key, Key1.hash_key * 'a) HTRep.table ref,
	n_items : int ref
      }

    fun index (i, sz) = Word.toIntX(Word.andb(i, Word.fromInt sz - 0w1))

  (* Create a new table; the int is a size hint and the exception
   * is to be raised by find.
   *)
    fun mkTable (n, exn) = TBL{
	    not_found = exn,
	    tbl1 = ref(HTRep.alloc n),
	    tbl2 = ref(HTRep.alloc n),
	    n_items = ref 0
	  }

  (* remove all elements from the table *)
    fun clear (TBL{tbl1, tbl2, n_items, ...}) = (
	  HTRep.clear(!tbl1); HTRep.clear(!tbl2); n_items := 0)

  (* Remove an item, returning the item.  The table's exception is raised if
   * the item doesn't exist.
   *)
    fun remove (hashVal, sameKey) (arr, not_found, key) =  let
	  val hash = hashVal key
	  val indx = index (hash, Array.length arr)
	  fun look HTRep.NIL = raise not_found
	    | look (HTRep.B(h, k, v, r)) = if ((hash = h) andalso sameKey(key, k))
		then (v, r)
		else let val (item, r') = look r in (item, HTRep.B(h, k, v, r')) end
	  val (item, bucket) = look (Array.sub (arr, indx))
	  in
	    Array.update (arr, indx, bucket);
	    item
	  end (* remove *)
    fun delete1 (tbl, not_found, k) =
	  remove (Key1.hashVal, Key1.sameKey) (tbl, not_found, k)
    fun delete2 (tbl, not_found, k) =
	  remove (Key2.hashVal, Key2.sameKey) (tbl, not_found, k)

    fun remove1 (TBL{tbl1, tbl2, n_items, not_found, ...}) k1 = let
	  val (k2, item) = delete1 (!tbl1, not_found, k1)
	  in
	    delete2 (!tbl2, not_found, k2);
	    n_items := !n_items - 1;
	    item
	  end
    fun remove2 (TBL{tbl1, tbl2, n_items, not_found, ...}) k2 = let
	  val (k1, item) = delete2 (!tbl2, not_found, k2)
	  in
	    delete1 (!tbl1, not_found, k1);
	    n_items := !n_items - 1;
	    item
	  end

  (* Insert an item.  If there is already an item that has either of the two keys,
   * then the old item is discarded (from both tables)
   *)
    fun insert (TBL{tbl1, tbl2, n_items, ...}) (k1, k2, item) = let
	  val arr1 = !tbl1 and arr2 = !tbl2
	  val sz = Array.length arr1
	  val h1 = Key1.hashVal k1 and h2 = Key2.hashVal k2
	  val i1 = index(h1, sz) and i2 = index(h2, sz)
	  fun look1 HTRep.NIL = (
		Array.update(arr1, i1,
		  HTRep.B(h1, k1, (k2, item), Array.sub(arr1, i1)));
	      (* we increment the number of items and grow the tables here,
	       * but not when inserting into tbl2.
	       *)
		n_items := !n_items + 1;
		if (HTRep.growTableIfNeeded (tbl1, !n_items))
		  then tbl2 := HTRep.growTable (arr2, Array.length(! tbl1))
		  else ();
		HTRep.NIL)
	    | look1 (HTRep.B(h1', k1', (k2', v), r)) =
		if ((h1' = h1) andalso Key1.sameKey(k1', k1))
		  then (
		    if not(Key2.sameKey(k2, k2'))
		      then ignore(delete2 (arr2, Fail "insert.look1", k2'))
		      else ();
		    HTRep.B(h1, k1, (k2, item), r))
		  else (case (look1 r)
		     of HTRep.NIL => HTRep.NIL
		      | rest => HTRep.B(h1', k1', (k2', v), rest)
		    (* end case *))
	  fun look2 HTRep.NIL = (
		Array.update(arr2, i2,
		  HTRep.B(h2, k2, (k1, item), Array.sub(arr2, i2)));
		HTRep.NIL)
	    | look2 (HTRep.B(h2', k2', (k1', v), r)) =
		if ((h2' = h2) andalso Key2.sameKey(k2', k2))
		  then (
		    if not(Key1.sameKey(k1, k1'))
		      then ignore(delete1 (arr1, Fail "insert.look2", k1'))
		      else ();
		    HTRep.B(h2, k2, (k1, item), r))
		  else (case (look2 r)
		     of HTRep.NIL => HTRep.NIL
		      | rest => HTRep.B(h2, k2, (k1, v), rest)
		    (* end case *))
	  in
	    case (look1 (Array.sub (arr1, i1)), look2 (Array.sub (arr2, i2)))
	     of (HTRep.NIL, HTRep.NIL) => ()
	      | (b1, b2) => (
		(* NOTE: both b1 and b2 should be non-nil, since we should
		 * have replaced an item in both tables.
		 *)
		  Array.update(arr1, i1, b1);
		  Array.update(arr2, i2, b2))
	    (* end case *)
	  end

  (* return true, if the key is in the domain of the table *)
    fun inDomain (hashVal, sameKey) tbl key = let
	  val arr = !tbl
	  val hash = hashVal key
	  val indx = index (hash, Array.length arr)
	  fun look HTRep.NIL = false
	    | look (HTRep.B(h, k, v, r)) =
		((hash = h) andalso sameKey(key, k)) orelse look r
	  in
	    look (Array.sub (arr, indx))
	  end
    fun inDomain1 (TBL{tbl1, ...}) = inDomain (Key1.hashVal, Key1.sameKey) tbl1
    fun inDomain2 (TBL{tbl2, ...}) = inDomain (Key2.hashVal, Key2.sameKey) tbl2

  (* Look for an item, the table's exception is raised if the item doesn't exist *)
    fun lookup (hashVal, sameKey) (tbl, not_found) key = let
	  val arr = !tbl
	  val hash = hashVal key
	  val indx = index (hash, Array.length arr)
	  fun look HTRep.NIL = raise not_found
	    | look (HTRep.B(h, k, (_, v), r)) =
		if ((hash = h) andalso sameKey(key, k)) then v else look r
	  in
	    look (Array.sub (arr, indx))
	  end
    fun lookup1 (TBL{tbl1, not_found, ...}) =
	  lookup (Key1.hashVal, Key1.sameKey) (tbl1, not_found)
    fun lookup2 (TBL{tbl2, not_found, ...}) =
	  lookup (Key2.hashVal, Key2.sameKey) (tbl2, not_found)

  (* Look for an item, return NONE if the item doesn't exist *)
    fun find (hashVal, sameKey) table key = let
	  val arr = !table
	  val sz = Array.length arr
	  val hash = hashVal key
	  val indx = index (hash, sz)
	  fun look HTRep.NIL = NONE
	    | look (HTRep.B(h, k, (_, v), r)) = if ((hash = h) andalso sameKey(key, k))
		then SOME v
		else look r
	  in
	    look (Array.sub (arr, indx))
	  end
    fun find1 (TBL{tbl1, ...}) = find (Key1.hashVal, Key1.sameKey) tbl1
    fun find2 (TBL{tbl2, ...}) = find (Key2.hashVal, Key2.sameKey) tbl2

  (* Return the number of items in the table *)
    fun numItems (TBL{n_items, ...}) = !n_items

  (* Return a list of the items (and their keys) in the table *)
    fun listItems (TBL{tbl1, ...}) =
	  HTRep.fold (fn ((_, item), l) => item::l) [] (! tbl1)
    fun listItemsi (TBL{tbl1, ...}) =
	  HTRep.foldi (fn (k1, (k2, item), l) => (k1, k2, item)::l) [] (! tbl1)

  (* Apply a function to the entries of the table *)
    fun app f (TBL{tbl1, ...}) =
	  HTRep.app (fn (_, v) => f v) (! tbl1)
    fun appi f (TBL{tbl1, ...}) =
	  HTRep.appi (fn (k1, (k2, v)) => f(k1, k2, v)) (! tbl1)

  (* Map a table to a new table that has the same keys *)
    fun map f (TBL{tbl1, tbl2, n_items, not_found}) = let
	  val sz = Array.length (! tbl1)
	  val newTbl = TBL{
		  tbl1 = ref (HTRep.alloc sz),
		  tbl2 = ref (HTRep.alloc sz),
		  n_items = ref 0,
		  not_found = not_found
		}
	  fun ins (k1, (k2, v)) = insert newTbl (k1, k2, f v)
	  in
	    HTRep.appi ins (! tbl1); newTbl
	  end
    fun mapi f (TBL{tbl1, tbl2, n_items, not_found}) = let
	  val sz = Array.length (! tbl1)
	  val newTbl = TBL{
		  tbl1 = ref (HTRep.alloc sz),
		  tbl2 = ref (HTRep.alloc sz),
		  n_items = ref 0,
		  not_found = not_found
		}
	  fun ins (k1, (k2, v)) = insert newTbl (k1, k2, f(k1, k2, v))
	  in
	    HTRep.appi ins (! tbl1); newTbl
	  end

(* TODO: add mapPartial and mapPartiali *)

    fun fold f init (TBL{tbl1, ...}) =
	  HTRep.fold (fn ((_, v), accum) => f(v, accum)) init (! tbl1)
    fun foldi f init (TBL{tbl1, ...}) =
	  HTRep.foldi (fn (k1, (k2, v), accum) => f(k1, k2, v, accum)) init (! tbl1)

(* TODO: add modify and modifyi *)

  (* remove any hash table items that do not satisfy the given
   * predicate.
   *)
    fun filter pred (TBL{tbl1, tbl2, n_items, ...}) = let
	  fun ins (k1, (k2, v)) = if (pred v)
		then ()
		else (
		  delete1 (!tbl1, Fail "filter", k1);
		  delete2 (!tbl2, Fail "filter", k2);
		  n_items := !n_items-1)
	  in
	    HTRep.appi ins (! tbl1)
	  end
    fun filteri pred (TBL{tbl1, tbl2, n_items, not_found}) = let
	  fun ins (k1, (k2, v)) = if (pred(k1, k2, v))
		then ()
		else (
		  delete1 (!tbl1, Fail "filteri", k1);
		  delete2 (!tbl2, Fail "filteri", k2);
		  n_items := !n_items-1)
	  in
	    HTRep.appi ins (! tbl1)
	  end

  (* Create a copy of a hash table *)
    fun copy (TBL{tbl1, tbl2, n_items, not_found}) = TBL{
	    tbl1 = ref(HTRep.copy (! tbl1)),
	    tbl2 = ref(HTRep.copy (! tbl2)),
	    n_items = ref(! n_items),
	    not_found = not_found
	  }

  (* returns a list of the sizes of the various buckets.  This is to
   * allow users to gauge the quality of their hashing function.
   *)
    fun bucketSizes (TBL{tbl1, tbl2, ...}) =
	  (HTRep.bucketSizes(! tbl1), HTRep.bucketSizes(! tbl2))

  end (* MONO_HASH2_TABLE *)
