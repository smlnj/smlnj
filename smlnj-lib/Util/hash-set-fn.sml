(* hash-set-fn.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * AUTHOR:  John Reppy
 *	    University of Chicago
 *	    https://cs.uchicago.edu/~jhr
 *)

functor HashSetFn (Key : HASH_KEY) : MONO_HASH_SET =
  struct

    structure Key = Key
  (* NOTE: someday we will change the HASH_KEY signature to follow the naming conventions of
   * the SML basis, so we use those names internally to ease future porting.
   *)
    type item = Key.hash_key
    val hash = Key.hashVal
    val same = Key.sameKey

    datatype bucket
      = NIL
      | B of (word * item * bucket)

    datatype set = SET of {
	table : bucket array ref,
	nItems : int ref
      }

    fun index (i, sz) = Word.toIntX(Word.andb(i, Word.fromInt sz - 0w1))

  (* minimum and maximum hash table sizes.  We use powers of two for hash table
   * sizes, since that give efficient indexing, and assume a minimum size of 32.
   *)
    val minSize = 32
    val maxSize = let
	  fun f i = let
		  val i' = i+i
		  in
		    if i' < Array.maxLen then f i' else i
		  end
	  in
	    f 0x10000
	  end

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
    fun alloc sizeHint = Array.array(roundUp sizeHint, NIL)

  (* grow a table to the specified size *)
    fun growTable (table, newSz) = let
	  val newArr = Array.array (newSz, NIL)
	  fun copy NIL = ()
	    | copy (B(h, key, rest)) = let
		val indx = index (h, newSz)
		in
		  Array.update (newArr, indx, B(h, key, Array.sub(newArr, indx)));
		  copy rest
		end
	  in
	    Array.app copy table;
	    newArr
	  end

  (* conditionally grow a table; return true if it grew. *)
    fun growTableIfNeeded (table, nItems) = let
	    val arr = !table
	    val sz = Array.length arr
	    in
	      if (nItems >= sz)
		then (table := growTable (arr, sz+sz); true)
		else false
	    end

  (* reverse-append for buckets *)
    fun revAppend (NIL, b) = b
      | revAppend (B(h, x, r), b) = revAppend(r, B(h, x, b))

    fun addWithHash (tbl as SET{table, nItems}, h, item) = let
	  val arr = !table
	  val sz = Array.length arr
	  val indx = index (h, sz)
	  fun look NIL = (
		Array.update(arr, indx, B(h, item, Array.sub(arr, indx)));
		nItems := !nItems + 1;
		growTableIfNeeded (table, !nItems);
		NIL)
	    | look (B(h', item', r)) = if ((h = h') andalso same(item, item'))
		then NIL (* item already present *)
		else (case (look r)
		   of NIL => NIL
		    | rest => B(h', item', rest)
		  (* end case *))
	  in
	    case (look (Array.sub (arr, indx)))
	     of NIL => ()
	      | b => Array.update(arr, indx, b)
	    (* end case *)
	  end

  (* Add an item to a set *)
    fun add (tbl, item) = addWithHash(tbl, hash item, item)
    fun addc set item = add(set, item)

  (* The empty set *)
    fun mkEmpty sizeHint = SET{
	    table = ref (alloc sizeHint),
	    nItems = ref 0
	  }

  (* Create a singleton set *)
    fun mkSingleton item = let
          val set = mkEmpty minSize
          in
            add (set, item);
            set
          end

  (* create a set from a list of items *)
    fun mkFromList items = let
          val set = mkEmpty(List.length items)
          in
            List.app (addc set) items;
            set
          end

    fun copy (SET{table=ref tbl, nItems}) = SET{
	    table = ref(Array.tabulate(Array.length tbl, fn i => Array.sub(tbl, i))),
	    nItems = ref(!nItems)
	  }

  (* Return a list of the items in the set *)
    fun toList (SET{table, nItems}) =
          if (!nItems = 0)
            then []
            else let
              fun f (NIL, l) = l
                | f (B(_, x, r), l) = f(r, x::l)
              in
                Array.foldl f [] (!table)
              end

  (* Insert items from list. *)
    fun addList (set, items) = List.app (addc set) items

  (* Remove an item. Raise NotFound if not found. *)
    fun delete (SET{table, nItems}, item) = let
	  val arr = !table
	  val sz = Array.length arr
	  val h = hash item
	  val indx = index (h, sz)
	  fun look (_, NIL) = false
	    | look (prefix, B(h', item', r)) = if ((h = h') andalso same(item, item'))
		then (
                  Array.update(arr, indx, revAppend(prefix, r));
                  nItems := !nItems - 1;
                  true)
		else look (B(h', item', prefix), r)
          in
            look (NIL, Array.sub(arr, indx))
          end

  (* Remove the item, if it is in the set.  Otherwise the set is unchanged. *)
    fun subtract (set, item) = ignore(delete (set, item))
    fun subtractc set item = subtract(set, item)

    fun subtractList (set, items) = List.app (subtractc set) items

  (* Return true if and only if item is an element in the set *)
    fun member (SET{table, ...}, item) = let
	  val arr = !table
	  val sz = Array.length arr
	  val h = hash item
	  val indx = index (h, sz)
	  fun look NIL = false
	    | look (B(h', item', r)) = ((h = h') andalso same(item, item')) orelse look r
          in
            look (Array.sub(arr, indx))
          end

  (* Return true if and only if the set is empty *)
    fun isEmpty (SET{nItems, ...}) = (!nItems = 0)

  (* Return true if and only if the first set is a subset of the second *)
    fun isSubset (SET{table=tbl1, nItems=n1}, s2 as SET{table=tbl2, nItems=n2}) =
          if (!n1 <= !n2)
            then let
              val arr1 = !tbl1 and arr2 = !tbl2
              val sz1 = Array.length arr1 and sz2 = Array.length arr2
              fun lp i = if (i <= sz1)
                    then let
                    (* iterate over the items in bucket i *)
                      fun look1 NIL = lp(i+1)
                        | look1 (B(h, item, r)) = let
                          (* search s2 for the item *)
                            fun look2 NIL = false
                              | look2 (B(h', item', r')) =
                                  if ((h = h') andalso same(item, item'))
                                    then look1 r
                                    else look2 r'
                            in
                              look2 (Array.sub(arr2, index (h, sz2)))
                            end
                      in
                        look1 (Array.sub(arr1, i))
                      end
                    else true
              in
                lp 0
              end
            else false

  (* Return the number of items in the table *)
    fun numItems (SET{nItems, ...}) = !nItems

  (* Create a new set by applying a map function to the elements
   * of the set.
   *)
    fun map f (SET{nItems, table}) = let
          val s = mkEmpty (!nItems)
          fun mapf NIL = ()
            | mapf (B(_, x, r)) = (add(s, f x); mapf r)
          in
            Array.app mapf (!table);
            s
          end

    fun mapPartial f (SET{nItems, table}) = let
	  val s = mkEmpty (!nItems)
	  fun mapf NIL = ()
	    | mapf (B(_, x, r)) = (case f x
		 of SOME x' => (add(s, x'); mapf r)
		  | NONE => mapf r
		(* end case *))
	  in
	    Array.app mapf (!table);
            s
          end

  (* Apply a function to the entries of the set. *)
    fun app f (SET{nItems, table}) = let
          fun appf NIL = ()
            | appf (B(_, x, r)) = (f x; appf r)
          in
            Array.app appf (!table)
          end

  (* Apply a folding function to the entries of the set. *)
    fun fold f init (SET{nItems, table}) = let
          fun foldf (NIL, acc) = acc
            | foldf (B(_, x, r), acc) = foldf (r, f(x, acc))
          in
            Array.foldl foldf init (!table)
          end

    fun partition pred (SET{table, nItems}) = let
	  val n = (!nItems div 2) + 1
	  val ts = mkEmpty n
	  val fs = mkEmpty n
	  fun part NIL = ()
	    | part (B(h, x, r)) = if pred x
		then (addWithHash(ts, h, x); part r)
		else (addWithHash(fs, h, x); part r)
	  in
	    Array.app part (!table);
	    (ts, fs)
	  end

    fun filter pred (SET{table=ref tbl, nItems}) = let
	  val len = Array.length tbl
	  fun remove (_, 0) = ()
	    | remove (i, n) = if (i < len)
		then (case Array.sub(tbl, i)
		   of NIL => remove(i+1, n)
		    | bucket => let
			fun rmv (NIL, items, n) = (
			      Array.update(tbl, i, items);
			      remove (i+1, n))
			  | rmv (B(h, x, r), items, n) = if pred x
			      then rmv(r, B(h, x, items), n)
			      else rmv(r, items, n-1)
			in
			  rmv (bucket, NIL, n)
			end
		  (* end case *))
		else nItems := n
	  in
	    remove (0, !nItems)
	  end

    fun exists pred (SET{table, ...}) = let
	  fun chk NIL = false
	    | chk (B(_, x, r)) = pred x orelse chk r
	  in
	    Array.exists chk (!table)
	  end

    fun all pred (SET{table, ...}) = let
	  fun chk NIL = true
	    | chk (B(_, x, r)) = pred x andalso chk r
	  in
	    Array.all chk (!table)
	  end

    fun find pred (SET{table=ref tbl, ...}) = let
	  val len = Array.length tbl
	  fun find' i = if (i < len)
		then let
		  fun chk NIL = find' (i+1)
		    | chk (B(_, x, r)) = if pred x then SOME x else chk r
		  in
		    chk (Array.sub(tbl, i))
		  end
		else NONE
	  in
	    find' 0
	  end

  (* DEPRECATED FUNCTIONS *)

    val listItems = toList
    val without = subtract

  end
