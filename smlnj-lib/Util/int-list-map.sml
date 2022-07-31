(* int-list-map.sml
 *
 * COPYRIGHT (c) 2012 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * COPYRIGHT (c) 1996 by AT&T Research.  See COPYRIGHT file for details.
 *
 * An implementation of finite maps on integer keys, which uses a sorted list
 * representation.
 *)

structure IntListMap :> ORD_MAP where type Key.ord_key = Int.int =
  struct

    structure Key =
      struct
	type ord_key = int
	val compare = Int.compare
      end

    type 'a map = (int * 'a) list

    val empty = []

    fun isEmpty [] = true
      | isEmpty _ = false

  (* return the first item in the map (or NONE if it is empty) *)
    fun first [] = NONE
      | first ((_, value)::_) = SOME value

  (* return the first item in the map and its key (or NONE if it is empty) *)
    fun firsti [] = NONE
      | firsti ((key, value)::_) = SOME(key, value)

    fun singleton (key, item) = [(key, item)]

    fun insert (l, key, item) = let
	  fun f [] = [(key, item)]
	    | f ((elem as (key', _))::r) = (case Key.compare(key, key')
		   of LESS => (key, item) :: elem :: r
		    | EQUAL => (key, item) :: r
		    | GREATER => elem :: (f r)
		  (* end case *))
	  in
	    f l
	  end
    fun insert' ((k, x), m) = insert(m, k, x)

    fun insertWithi comb (l, key, item) = let
	  fun f [] = [(key, item)]
	    | f ((elem as (key', item'))::r) = (case Key.compare(key, key')
		   of LESS => (key, item) :: elem :: r
		    | EQUAL => (key, comb(key, item', item)) :: r
		    | GREATER => elem :: (f r)
		  (* end case *))
	  in
	    f l
	  end
    fun insertWith comb = insertWithi (fn (_, x1, x2) => comb(x1, x2))

  (* return true if the key is in the map's domain *)
    fun inDomain (l, key) = let
	  fun f [] = false
	    | f ((key', x) :: r) = (key' <= key) andalso ((key' = key) orelse f r)
	  in
	    f l
	  end

  (* Look for an item, return NONE if the item doesn't exist *)
    fun find (l, key) = let
	  fun f [] = NONE
	    | f ((key', x) :: r) =
		if (key < key') then NONE
		else if (key = key') then SOME x
		else f r
	  in
	    f l
	  end

  (* Look for an item, raise NotFound if the item doesn't exist *)
    fun lookup (l, key) = let
	  fun f [] = raise LibBase.NotFound
	    | f ((key', x) :: r) =
                  if (key' < key) then f r
                  else if (key' = key) then x
                  else raise LibBase.NotFound
	  in
	    f l
	  end

  (* Remove an item, returning new map and value removed.
   * Raise LibBase.NotFound if not found.
   *)
    fun remove (l, key) = let
	  fun f (_, []) = raise LibBase.NotFound
	    | f (prefix, (elem as (key', x)) :: r) =
                  if (key' < key) then f(elem :: prefix, r)
                  else if (key' = key) then (List.revAppend(prefix, r), x)
                  else raise LibBase.NotFound
	  in
	    f ([], l)
	  end

    fun findAndRemove (l, key) = let
	  fun f (_, []) = raise LibBase.NotFound
	    | f (prefix, (elem as (key', x)) :: r) =
                  if (key' < key) then f(elem :: prefix, r)
                  else if (key' = key) then SOME(List.revAppend(prefix, r), x)
                  else NONE
	  in
	    f ([], l)
	  end

  (* Return the number of items in the map *)
    fun numItems l = List.length l

  (* Return a list of the items (and their keys) in the map *)
    fun listItems (l : 'a map) = List.map #2 l
    fun listItemsi l = l

    fun listKeys (l : 'a map) = List.map #1 l

    fun equiv rngEq = let
          fun cmp ([], []) = true
            | cmp ((xk, x)::xr, (yk, y)::yr) =
                (xk = yk) andalso rngEq(x, y) andalso cmp(xr, yr)
            | cmp _ = false
          in
            cmp
          end

    fun collate cmpRng = let
	  fun cmp ([], []) = EQUAL
	    | cmp ([], _) = LESS
	    | cmp (_, []) = GREATER
            | cmp (xs as ((xk, x)::xr), (yk, y)::yr) =
                if (xk = yk)
                  then (case cmpRng(x, y)
                     of EQUAL => cmp (xr, yr)
                      | order => order
                    (* end case *))
                else if (xk < yk)
                  then LESS
                  else GREATER
	  in
	    cmp
	  end

    fun extends rngEx = let
	  fun cmp ([], []) = true
            | cmp (_, []) = true
            | cmp ([], _) = false
            | cmp ((xk, x)::xr, ys as ((yk, y)::yr)) =
                if (xk < yk) then cmp (xr, ys)
                else (xk = yk) andalso rngEx(x, y) andalso cmp (xr, yr)
	  in
	    cmp
	  end

  (* return a map whose domain is the union of the domains of the two input
   * maps, using the supplied function to define the map on elements that
   * are in both domains.
   *)
    fun unionWith f (m1 : 'a map, m2 : 'a map) = let
	  fun merge ([], [], l) = List.rev l
	    | merge ([], m2, l) = List.revAppend(l, m2)
	    | merge (m1, [], l) = List.revAppend(l, m1)
	    | merge (m1 as ((k1, x1)::r1), m2 as ((k2, x2)::r2), l) = (
		case Key.compare (k1, k2)
		 of LESS => merge (r1, m2, (k1, x1)::l)
		  | EQUAL => merge (r1, r2, (k1, f(x1, x2)) :: l)
		  | GREATER => merge (m1, r2, (k2, x2)::l)
		(* end case *))
	  in
	    merge (m1, m2, [])
	  end
    fun unionWithi f (m1 : 'a map, m2 : 'a map) = let
	  fun merge ([], [], l) = List.rev l
	    | merge ([], m2, l) = List.revAppend(l, m2)
	    | merge (m1, [], l) = List.revAppend(l, m1)
	    | merge (m1 as ((k1, x1)::r1), m2 as ((k2, x2)::r2), l) = (
		case Key.compare (k1, k2)
		 of LESS => merge (r1, m2, (k1, x1)::l)
		  | EQUAL => merge (r1, r2, (k1, f(k1, x1, x2)) :: l)
		  | GREATER => merge (m1, r2, (k2, x2)::l)
		(* end case *))
	  in
	    merge (m1, m2, [])
	  end

  (* return a map whose domain is the intersection of the domains of the
   * two input maps, using the supplied function to define the range.
   *)
    fun intersectWith f (m1 : 'a map, m2 : 'b map) = let
	  fun merge (m1 as ((k1, x1)::r1), m2 as ((k2, x2)::r2), l) = (
		case Key.compare (k1, k2)
		 of LESS => merge (r1, m2, l)
		  | EQUAL => merge (r1, r2, (k1, f(x1, x2)) :: l)
		  | GREATER => merge (m1, r2, l)
		(* end case *))
	    | merge (_, _, l) = List.rev l
	  in
	    merge (m1, m2, [])
	  end
    fun intersectWithi f (m1 : 'a map, m2 : 'b map) = let
	  fun merge (m1 as ((k1, x1)::r1), m2 as ((k2, x2)::r2), l) = (
		case Key.compare (k1, k2)
		 of LESS => merge (r1, m2, l)
		  | EQUAL => merge (r1, r2, (k1, f(k1, x1, x2)) :: l)
		  | GREATER => merge (m1, r2, l)
		(* end case *))
	    | merge (_, _, l) = List.rev l
	  in
	    merge (m1, m2, [])
	  end

    fun mergeWith f (m1 : 'a map, m2 : 'b map) = let
	  fun merge (m1 as ((k1, x1)::r1), m2 as ((k2, x2)::r2), l) =
		if (k1 < k2)
		  then mergef (k1, SOME x1, NONE, r1, m2, l)
		else if (k1 = k2)
		  then mergef (k1, SOME x1, SOME x2, r1, r2, l)
		  else mergef (k2, NONE, SOME x2, m1, r2, l)
	    | merge ([], [], l) = List.rev l
	    | merge ((k1, x1)::r1, [], l) = mergef (k1, SOME x1, NONE, r1, [], l)
	    | merge ([], (k2, x2)::r2, l) = mergef (k2, NONE, SOME x2, [], r2, l)
	  and mergef (k, x1, x2, r1, r2, l) = (case f (x1, x2)
		 of NONE => merge (r1, r2, l)
		  | SOME y => merge (r1, r2, (k, y)::l)
		(* end case *))
	  in
	    merge (m1, m2, [])
	  end
    fun mergeWithi f (m1 : 'a map, m2 : 'b map) = let
	  fun merge (m1 as ((k1, x1)::r1), m2 as ((k2, x2)::r2), l) =
		if (k1 < k2)
		  then mergef (k1, SOME x1, NONE, r1, m2, l)
		else if (k1 = k2)
		  then mergef (k1, SOME x1, SOME x2, r1, r2, l)
		  else mergef (k2, NONE, SOME x2, m1, r2, l)
	    | merge ([], [], l) = List.rev l
	    | merge ((k1, x1)::r1, [], l) = mergef (k1, SOME x1, NONE, r1, [], l)
	    | merge ([], (k2, x2)::r2, l) = mergef (k2, NONE, SOME x2, [], r2, l)
	  and mergef (k, x1, x2, r1, r2, l) = (case f (k, x1, x2)
		 of NONE => merge (r1, r2, l)
		  | SOME y => merge (r1, r2, (k, y)::l)
		(* end case *))
	  in
	    merge (m1, m2, [])
	  end

  (* Apply a function to the entries of the map in map order. *)
    val appi = List.app
    fun app f l = appi (fn (_, item) => f item) l

  (* Create a new table by applying a map function to the
   * name/value pairs in the table.
   *)
    fun mapi f l = List.map (fn (key, item) => (key, f(key, item))) l
    fun map f l = List.map (fn (key, item) => (key, f item)) l

  (* Apply a folding function to the entries of the map
   * in increasing map order.
   *)
    fun foldli f init l =
	  List.foldl (fn ((key, item), accum) => f(key, item, accum)) init l
    fun foldl f init l = List.foldl (fn ((_, item), accum) => f(item, accum)) init l

  (* Apply a folding function to the entries of the map
   * in decreasing map order.
   *)
    fun foldri f init l =
	  List.foldr (fn ((key, item), accum) => f(key, item, accum)) init l
    fun foldr f init l = List.foldr (fn ((_, item), accum) => f(item, accum)) init l

    fun filter pred l = List.filter (fn (_, item) => pred item) l
    fun filteri pred l = List.filter pred l

    fun mapPartiali f l = let
	  fun f' (key, item) = (case f (key, item)
		 of NONE => NONE
		  | SOME y => SOME(key, y)
		(* end case *))
	  in
	    List.mapPartial f' l
	  end
    fun mapPartial f l = mapPartiali (fn (_, item) => f item) l

  (* check the elements of a map with a predicate and return true if
   * any element satisfies the predicate. Return false otherwise.
   * Elements are checked in key order.
   *)
    fun exists pred = let
	  fun exists' [] = false
	    | exists' ((_, x)::r) = pred x orelse exists' r
	  in
	    exists'
	  end
    fun existsi pred = let
	  fun exists' [] = false
	    | exists' (arg::r) = pred arg orelse exists' r
	  in
	    exists'
	  end

  (* check the elements of a map with a predicate and return true if
   * they all satisfy the predicate. Return false otherwise.  Elements
   * are checked in key order.
   *)
    fun all pred = let
	  fun all' [] = false
	    | all' ((_, x)::r) = pred x andalso all' r
	  in
	    all'
	  end
    fun alli pred = let
	  fun all' [] = false
	    | all' (arg::r) = pred arg andalso all' r
	  in
	    all'
	  end

  end (* IntListMap *)

