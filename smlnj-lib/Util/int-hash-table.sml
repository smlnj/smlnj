(* int-hash-table.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * A specialization of hash tables to integer keys.  The hash values are just the
 * word representation of the integer keys, so we can eliminate the equality test
 * on key values.
 *
 * Note that we could further specialize the representation of bucket items, since
 * the hash value and key are the same bit pattern, so we do not need to store
 * both!  Preliminary experiments, however, suggest that performance might not
 * improve because of increased GC time (possibly a cache alignment issue).
 *
 * AUTHOR:  John Reppy
 *	    University of Chicago
 *	    https://cs.uchicago.edu/~jhr
 *)

structure IntHashTable :> MONO_HASH_TABLE where type Key.hash_key = int =
  struct

    structure Key =
      struct
	type hash_key = int
	fun sameKey (a : int, b) = (a = b)
	fun hashVal a = Word.fromInt a
      end

    open Key

    structure HTRep = HashTableRep

    datatype 'a hash_table = HT of {
	not_found : exn,
	table : (hash_key, 'a) HTRep.table ref,
	n_items : int ref
      }

    fun index (i, sz) = Word.toIntX(Word.andb(i, Word.fromInt sz - 0w1))

  (* Create a new table; the int is a size hint and the exception
   * is to be raised by find.
   *)
    fun mkTable (sizeHint, notFound) = HT{
	    not_found = notFound,
	    table = ref (HTRep.alloc sizeHint),
	    n_items = ref 0
	  }

  (* remove all elements from the table *)
    fun clear (HT{table, n_items, ...}) = (HTRep.clear(!table); n_items := 0)

    fun insertWithi combine (tbl as HT{table, n_items, ...}) (key, item) = let
	  val arr = !table
	  val sz = Array.length arr
	  val hash = hashVal key
	  val indx = index (hash, sz)
	  fun look HTRep.NIL = (
		Array.update(arr, indx, HTRep.B(hash, key, item, Array.sub(arr, indx)));
		n_items := !n_items + 1;
		HTRep.growTableIfNeeded (table, !n_items);
		HTRep.NIL)
	    | look (HTRep.B(h, k, v, r)) = if (hash = h)
		then HTRep.B(hash, key, combine(k, v, item), r)
		else (case (look r)
		   of HTRep.NIL => HTRep.NIL
		    | rest => HTRep.B(h, k, v, rest)
		  (* end case *))
	  in
	    case (look (Array.sub (arr, indx)))
	     of HTRep.NIL => ()
	      | b => Array.update(arr, indx, b)
	    (* end case *)
	  end

    fun insertWith combine = insertWithi (fn (_, v1, v2) => combine(v1, v2))

  (* Insert an item.  If the key already has an item associated with it,
   * then the old item is discarded.
   *)
    fun insert (tbl as HT{table, n_items, ...}) (key, item) = let
	  val arr = !table
	  val sz = Array.length arr
	  val hash = hashVal key
	  val indx = index (hash, sz)
	  fun look HTRep.NIL = (
		Array.update(arr, indx, HTRep.B(hash, key, item, Array.sub(arr, indx)));
		n_items := !n_items + 1;
		HTRep.growTableIfNeeded (table, !n_items);
		HTRep.NIL)
	    | look (HTRep.B(h, k, v, r)) = if (hash = h)
		then HTRep.B(hash, key, item, r)
		else (case (look r)
		   of HTRep.NIL => HTRep.NIL
		    | rest => HTRep.B(h, k, v, rest)
		  (* end case *))
	  in
	    case (look (Array.sub (arr, indx)))
	     of HTRep.NIL => ()
	      | b => Array.update(arr, indx, b)
	    (* end case *)
	  end

  (* return true, if the key is in the domain of the table *)
    fun inDomain (HT{table, ...}) key = let
	  val arr = !table
	  val hash = hashVal key
	  val indx = index (hash, Array.length arr)
	  fun look HTRep.NIL = false
	    | look (HTRep.B(h, k, v, r)) = (hash = h) orelse look r
	  in
	    look (Array.sub (arr, indx))
	  end

  (* find an item, the table's exception is raised if the item doesn't exist *)
    fun lookup (HT{table, not_found, ...}) key = let
	  val arr = !table
	  val hash = hashVal key
	  val indx = index (hash, Array.length arr)
	  fun look HTRep.NIL = raise not_found
	    | look (HTRep.B(h, k, v, r)) = if (hash = h)
		then v
		else look r
	  in
	    look (Array.sub (arr, indx))
	  end

  (* look for an item, return NONE if the item doesn't exist *)
    fun find (HT{table, ...}) key = let
	  val arr = !table
	  val sz = Array.length arr
	  val hash = hashVal key
	  val indx = index (hash, sz)
	  fun look HTRep.NIL = NONE
	    | look (HTRep.B(h, k, v, r)) = if (hash = h)
		then SOME v
		else look r
	  in
	    look (Array.sub (arr, indx))
	  end

    fun findAndRemove (HT{not_found, table, n_items}) key = let
	  val arr = !table
	  val sz = Array.length arr
	  val hash = hashVal key
	  val indx = index (hash, sz)
          fun look HTRep.NIL = raise not_found
            | look (HTRep.B(h, k, v, r)) = if (hash = h)
                then (v, r)
                else let
                  val (v', r') = look r
                  in
                    (v', HTRep.B(h, k, v, r'))
                  end
          val (v, bucket) = look (Array.sub (arr, indx))
          in
            Array.update (arr, indx, bucket); SOME v
	  end
            handle _ => NONE

  (* Remove an item.  The table's exception is raised if
   * the item doesn't exist.
   *)
    fun remove (HT{not_found, table, n_items}) key = let
	  val arr = !table
	  val sz = Array.length arr
	  val hash = hashVal key
	  val indx = index (hash, sz)
	  fun look HTRep.NIL = raise not_found
	    | look (HTRep.B(h, k, v, r)) = if (hash = h)
		then (v, r)
		else let val (item, r') = look r in (item, HTRep.B(h, k, v, r')) end
	  val (item, bucket) = look (Array.sub (arr, indx))
	  in
	    Array.update (arr, indx, bucket);
	    n_items := !n_items - 1;
	    item
	  end (* remove *)

  (* Return the number of items in the table *)
   fun numItems (HT{n_items, ...}) = !n_items

  (* return a list of the items in the table *)
    fun listItems (HT{table = ref arr, n_items, ...}) =
	  HTRep.listItems (arr, n_items)
    fun listItemsi (HT{table = ref arr, n_items, ...}) =
	  HTRep.listItemsi (arr, n_items)

  (* Apply a function to the entries of the table *)
    fun appi f (HT{table, ...}) = HTRep.appi f (! table)
    fun app f (HT{table, ...}) = HTRep.app f (! table)

  (* Map a table to a new table that has the same keys and exception *)
    fun mapi f (HT{table, n_items, not_found}) = HT{
	    table = ref(HTRep.mapi f (! table)),
	    n_items = ref(!n_items),
	    not_found = not_found
	  }
    fun map f (HT{table, n_items, not_found}) = HT{
	    table = ref(HTRep.map f (! table)),
	    n_items = ref(!n_items),
	    not_found = not_found
	  }

  (* Fold a function over the entries of the table *)
    fun foldi f init (HT{table, ...}) = HTRep.foldi f init (! table)
    fun fold f init (HT{table, ...}) = HTRep.fold f init (! table)

  (* modify the hash-table items in place *)
    fun modifyi f (HT{table, ...}) = HTRep.modifyi f (!table)
    fun modify f (HT{table, ...}) = HTRep.modify f (!table)

  (* remove any hash table items that do not satisfy the given
   * predicate.
   *)
    fun filteri pred (HT{table, n_items, ...}) =
	  n_items := HTRep.filteri pred (! table)
    fun filter pred (HT{table, n_items, ...}) =
	  n_items := HTRep.filter pred (! table)

  (* Create a copy of a hash table *)
    fun copy (HT{table, n_items, not_found}) = HT{
	    table = ref(HTRep.copy(! table)),
	    n_items = ref(!n_items),
	    not_found = not_found
	  }

  (* returns a list of the sizes of the various buckets.  This is to
   * allow users to gauge the quality of their hashing function.
   *)
    fun bucketSizes (HT{table, ...}) = HTRep.bucketSizes (! table)

  end (* HashTableFn *)
