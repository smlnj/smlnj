(* hash-table-fn.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * A hash table functor.  It takes a key type with two operations: sameKey and
 * hashVal as arguments (see hash-key-sig.sml).
 *
 * AUTHOR:  John Reppy
 *	    University of Chicago
 *	    https://cs.uchicago.edu/~jhr
 *)

functor HashTableFn (Key : HASH_KEY) : MONO_HASH_TABLE =
  struct

    structure Key = Key
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
	    | look (HTRep.B(h, k, v, r)) = if ((hash = h) andalso sameKey(key, k))
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
	    | look (HTRep.B(h, k, v, r)) =
		((hash = h) andalso sameKey(key, k)) orelse look r
	  in
	    look (Array.sub (arr, indx))
	  end

  (* find an item, the table's exception is raised if the item doesn't exist *)
    fun lookup (HT{table, not_found, ...}) key = let
	  val arr = !table
	  val hash = hashVal key
	  val indx = index (hash, Array.length arr)
	  fun look HTRep.NIL = raise not_found
	    | look (HTRep.B(h, k, v, r)) = if ((hash = h) andalso sameKey(key, k))
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
	    | look (HTRep.B(h, k, v, r)) = if ((hash = h) andalso sameKey(key, k))
		then SOME v
		else look r
	  in
	    look (Array.sub (arr, indx))
	  end

  (* Remove an item.  The table's exception is raised if
   * the item doesn't exist.
   *)
    fun remove (HT{not_found, table, n_items}) key = let
	  val arr = !table
	  val sz = Array.length arr
	  val hash = hashVal key
	  val indx = index (hash, sz)
	  fun look HTRep.NIL = raise not_found
	    | look (HTRep.B(h, k, v, r)) = if ((hash = h) andalso sameKey(key, k))
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
