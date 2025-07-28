(* list-set-fn.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *
 * An implementation of finite sets of ordered values, which uses a sorted list
 * representation.
 *)

functor ListSetFn (K : ORD_KEY) :> ORD_SET where type Key.ord_key = K.ord_key =
  struct

    structure Key = K

  (* sets are represented as ordered lists of key values *)
    type item = Key.ord_key
    type set = item list

    val empty = []

    fun singleton x = [x]

    fun add (l, item) = let
	  fun f [] = [item]
	    | f (elem::r) = (case Key.compare(item, elem)
		   of LESS => item :: elem :: r
		    | EQUAL => item :: r
		    | GREATER => elem :: (f r)
		  (* end case *))
	  in
	    f l
	  end
    fun add' (s, x) = add(x, s)

    fun union (s1, s2) = let
	  fun merge ([], l2) = l2
	    | merge (l1, []) = l1
	    | merge (x::r1, y::r2) = (case Key.compare(x, y)
		 of LESS => x :: merge(r1, y::r2)
		  | EQUAL => x :: merge(r1, r2)
		  | GREATER => y :: merge(x::r1, r2)
		(* end case *))
	  in
	    merge (s1, s2)
	  end

    fun intersection (s1, s2) = let
	  fun merge ([], l2) = []
	    | merge (l1, []) = []
	    | merge (x::r1, y::r2) = (case Key.compare(x, y)
		 of LESS => merge(r1, y::r2)
		  | EQUAL => x :: merge(r1, r2)
		  | GREATER => merge(x::r1, r2)
		(* end case *))
	  in
	    merge (s1, s2)
	  end

    fun difference (s1, s2) = let
	  fun merge ([], l2) = []
	    | merge (l1, []) = l1
	    | merge (x::r1, y::r2) = (case Key.compare(x, y)
		 of LESS => x :: merge(r1, y::r2)
		  | EQUAL => merge(r1, r2)
		  | GREATER => merge(x::r1, r2)
		(* end case *))
	  in
	    merge (s1, s2)
	  end

    (* general set combiner *)
    fun combineWith pred (s1, s2) = let
          fun comb ([], []) = []
            | comb (x1::r1, []) = condAdd (x1, true, false, r1, [])
            | comb ([], x2::r2) = condAdd (x2, false, true, [], r2)
            | comb (s1 as x1::r1, s2 as x2::r2) = (case Key.compare(x1, x2)
		 of LESS => condAdd (x1, true, false, r1, s2)
		  | EQUAL => condAdd (x1, true, true, r1, r2)
		  | GREATER => condAdd (x2, false, true, s1, r2)
		(* end case *))
          and condAdd (x, p1, p2, r1, r2) =
                if pred (x, p1, p2)
                  then x :: comb (r1, r2)
                  else comb (r1, r2)
          in
            comb (s1, s2)
          end

    fun addList (l, items) = let
	  val items' = List.foldl (fn (x, set) => add(set, x)) [] items
	  in
	    union (l, items')
	  end

    fun subtract (l, item) = let
	  fun f ([], _) = l
	    | f (elem::r, prefix) = (case Key.compare(item, elem)
		   of LESS => l
		    | EQUAL => List.revAppend(prefix, r)
		    | GREATER => f (r, elem::prefix)
		  (* end case *))
	  in
	    f (l, [])
	  end
    fun subtract' (item, l) = subtract (l, item)

    fun subtractList (l, items) = let
	  val items' = List.foldl (fn (x, set) => add(set, x)) [] items
	  in
	    difference (l, items')
	  end

  (* create a set from a list of items; this function works in linear time if the list
   * is in increasing order.
   *)
    fun fromList [] = []
      | fromList (first::rest) = let
	  fun add (prev, x::xs, s) = (case Key.compare(prev, x)
		 of LESS => add (x, xs, x::s)
		  | _ => (* not ordered, so fallback to addList *)
		      addList (List.rev s, x::xs)
		(* end case *))
	    | add (_, [], s) = List.rev s
	  in
	    add (first, rest, [first])
	  end

  (* Remove an item, returning new map and value removed.
   * Raise LibBase.NotFound if not found.
   *)
    fun delete (l, elem) = let
	  fun f (_, []) = raise LibBase.NotFound
	    | f (prefix, elem' :: r) = (case Key.compare(elem, elem')
		   of LESS => raise LibBase.NotFound
		    | EQUAL => List.revAppend(prefix, r)
		    | GREATER => f(elem' :: prefix, r)
		  (* end case *))
	  in
	    f ([], l)
	  end

    fun member (l, item) = let
	  fun f [] = false
	    | f (elem :: r) = (case Key.compare(item, elem)
		   of LESS => false
		    | EQUAL => true
		    | GREATER => f r
		  (* end case *))
	  in
	    f l
	  end

    fun isEmpty [] = true
      | isEmpty _ = false

    fun minItem [] = raise Empty
      | minItem (x::_) = x

    fun maxItem xs = List.last xs

    fun equal (s1, s2) = let
	  fun f ([], []) = true
	    | f (x::r1, y::r2) = (Key.compare(x, y) = EQUAL) andalso f (r1, r2)
	    | f _ = false
	  in
	    f (s1, s2)
	  end

    fun compare ([], []) = EQUAL
      | compare ([], _) = LESS
      | compare (_, []) = GREATER
      | compare (x1::r1, x2::r2) = (case Key.compare(x1, x2)
	   of EQUAL => compare (r1, r2)
	    | order => order
	  (* end case *))

  (* Return true if and only if the first set is a subset of the second *)
    fun isSubset (s1, s2) = let
	  fun f ([], _) = true
	    | f (_, []) = false
	    | f (x::r1, y::r2) = (case Key.compare(x, y)
		   of LESS => false
		    | EQUAL => f (r1, r2)
		    | GREATER => f (x::r1, r2)
		  (* end case *))
	  in
	    f (s1, s2)
	  end

    fun disjoint ([], _) = true
      | disjoint (_, []) = true
      | disjoint (x::r1, y::r2) = (case Key.compare(x, y)
	   of LESS => disjoint (r1, y::r2)
	    | EQUAL => false
	    | GREATER => disjoint (x::r1, r2)
	  (* end case *))

  (* Return the number of items in the set *)
    fun numItems l = List.length l

  (* Return a list of the items in the set *)
    fun toList l = l

    val app = List.app
    fun map f s1 = List.foldl (fn (x, s) => add(s, f x)) [] s1
    fun mapPartial f s = let
	  fun f' (x, acc) = (case f x of SOME x' => add(acc, x') | NONE => acc)
	  in
	    List.foldl f' [] s
	  end
    val foldr = List.foldr
    val foldl = List.foldl
    val filter = List.filter
    val partition = List.partition
    val exists = List.exists
    val all = List.all
    val find = List.find

  (* deprecated *)
    val listItems = toList

  end (* IntListMap *)

