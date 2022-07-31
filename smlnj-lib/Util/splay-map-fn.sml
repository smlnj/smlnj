(* splay-map-fn.sml
 *
 * COPYRIGHT (c) 2012 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Functor implementing dictionaries using splay trees.
 *
 *)

functor SplayMapFn (K : ORD_KEY) :> ORD_MAP where type Key.ord_key = K.ord_key =
  struct
    structure Key = K
    open SplayTree

    datatype 'a map
      = EMPTY
      | MAP of {
	  root : (K.ord_key * 'a) splay ref,
	  nobj : int
	}

    fun cmpf k (k', _) = K.compare(k',k)

    val empty = EMPTY

    fun isEmpty EMPTY = true
      | isEmpty _ = false

  (* return the first item in the map (or NONE if it is empty) *)
    fun first EMPTY = NONE
      | first (MAP{root, ...}) = let
	  fun f (SplayObj{value=(_, value), left=SplayNil, ...}) = SOME value
	    | f (SplayObj{left, ...}) = f left
	    | f SplayNil = raise Fail "SplayMapFn.first"
	  in
	    f (!root)
	  end

  (* return the first item in the map and its key (or NONE if it is empty) *)
    fun firsti EMPTY = NONE
      | firsti (MAP{root, ...}) = let
	  fun f (SplayObj{value=(key, value), left=SplayNil, ...}) = SOME(key, value)
	    | f (SplayObj{left, ...}) = f left
	    | f SplayNil = raise Fail "SplayMapFn.firsti"
	  in
	    f (!root)
	  end

    fun singleton (key, v) =
          MAP{nobj=1,root=ref(SplayObj{value=(key,v),left=SplayNil,right=SplayNil})}

  (* Insert an item.  *)
    fun insert (EMPTY,key,v) =
          MAP{nobj=1,root=ref(SplayObj{value=(key,v),left=SplayNil,right=SplayNil})}
      | insert (MAP{root,nobj},key,v) =
          case splay (cmpf key, !root) of
            (EQUAL,SplayObj{value,left,right}) =>
              MAP{nobj=nobj,root=ref(SplayObj{value=(key,v),left=left,right=right})}
          | (LESS,SplayObj{value,left,right}) =>
              MAP{
                nobj=nobj+1,
                root=ref(SplayObj{value=(key,v),left=SplayObj{value=value,left=left,right=SplayNil},right=right})
              }
          | (GREATER,SplayObj{value,left,right}) =>
              MAP{
                nobj=nobj+1,
                root=ref(SplayObj{
                  value=(key,v),
                  left=left,
                  right=SplayObj{value=value,left=SplayNil,right=right}
                })
              }
          | (_,SplayNil) => raise LibBase.Impossible "SplayMapFn.insert SplayNil"
    fun insert' ((k, x), m) = insert(m, k, x)

    fun insertWithi comb (m, key, v) = let
	  fun insert EMPTY =
		MAP{nobj=1,root=ref(SplayObj{value=(key,v),left=SplayNil,right=SplayNil})}
	    | insert (MAP{root,nobj}) = (case splay (cmpf key, !root)
		 of (EQUAL, SplayObj{value,left,right}) => let
		      val v' = (key, comb(key, #2 value, v))
		      in
			MAP{
			    nobj=nobj,
			    root=ref(SplayObj{value=v', left=left, right=right})
		          }
		      end
		 | (LESS,SplayObj{value,left,right}) => MAP{
			nobj=nobj+1,
			root=ref(SplayObj{value=(key,v),left=SplayObj{value=value,left=left,right=SplayNil},right=right})
		      }
		 | (GREATER,SplayObj{value,left,right}) => MAP{
			nobj=nobj+1,
			root=ref(SplayObj{
			  value=(key,v),
			  left=left,
			  right=SplayObj{value=value,left=SplayNil,right=right}
			})
		      }
		| (_,SplayNil) => raise LibBase.Impossible "SplayMapFn.insert SplayNil"
	      (* end case *))
	  in
	    insert m
	  end
    fun insertWith comb = insertWithi (fn (_, x1, x2) => comb(x1, x2))

    fun inDomain (EMPTY, _) = false
      | inDomain (MAP{root,nobj}, key) = (case splay (cmpf key, !root)
	   of (EQUAL, r as SplayObj{value,...}) => (root := r; true)
	    | (_, r) => (root := r; false)
	  (* end case *))

  (* Look for an item, return NONE if the item doesn't exist *)
    fun find (EMPTY,_) = NONE
      | find (MAP{root,nobj},key) = (case splay (cmpf key, !root)
	   of (EQUAL, r as SplayObj{value,...}) => (root := r; SOME(#2 value))
	    | (_, r) => (root := r; NONE)
	  (* end case *))

  (* Look for an item, raise NotFound if the item doesn't exist *)
    fun lookup (EMPTY,_) = raise LibBase.NotFound
      | lookup (MAP{root,nobj},key) = (case splay (cmpf key, !root)
	   of (EQUAL, r as SplayObj{value,...}) => (root := r; #2 value)
	    | (_, r) => (root := r; raise LibBase.NotFound)
	  (* end case *))

	(* Remove an item.
         * Raise LibBase.NotFound if not found
	 *)
    fun remove (EMPTY, _) = raise LibBase.NotFound
      | remove (MAP{root,nobj}, key) = (case (splay (cmpf key, !root))
	 of (EQUAL, SplayObj{value, left, right}) =>
	      if nobj = 1
		then (EMPTY, #2 value)
		else (MAP{root=ref(join(left,right)),nobj=nobj-1}, #2 value)
	    | (_,r) => (root := r; raise LibBase.NotFound)
	  (* end case *))
    fun findAndRemove arg = SOME(remove arg) handle LibBase.NotFound => NONE

	(* Return the number of items in the table *)
    fun numItems EMPTY = 0
      | numItems (MAP{nobj,...}) = nobj

	(* Return a list of the items (and their keys) in the dictionary *)
    fun listItems EMPTY = []
      | listItems (MAP{root,...}) = let
	  fun apply (SplayNil, l) = l
            | apply (SplayObj{value=(_, v), left, right}, l) =
                apply(left, v::(apply (right,l)))
        in
          apply (!root, [])
        end
    fun listItemsi EMPTY = []
      | listItemsi (MAP{root,...}) = let
	  fun apply (SplayNil,l) = l
            | apply (SplayObj{value,left,right},l) =
                apply(left, value::(apply (right,l)))
        in
          apply (!root,[])
        end

    fun listKeys EMPTY = []
      | listKeys (MAP{root,...}) = let
	  fun apply (SplayNil, l) = l
            | apply (SplayObj{value=(key, _),left,right},l) =
                apply(left, key::(apply (right,l)))
        in
          apply (!root, [])
        end

    local
      fun next ((t as SplayObj{right, ...})::rest) = (t, left(right, rest))
	| next _ = (SplayNil, [])
      and left (SplayNil, rest) = rest
	| left (t as SplayObj{left=l, ...}, rest) = left(l, t::rest)
      fun start s = left(!s, [])
    in
    fun equiv rngEq (MAP{root=s1, nobj=n1},  MAP{root=s2, nobj=n2}) = let
          fun cmp (t1, t2) = (case (next t1, next t2)
		 of ((SplayObj{value=(xk, x), ...}, r1), (SplayObj{value=(yk, y), ...}, r2)) => (
		      case Key.compare(xk, yk)
		       of EQUAL => rngEq (x, y) andalso cmp (r1, r2)
			| _ => false
		      (* end case *))
		  | ((SplayNil, _), (SplayNil, _)) => true
		  | _ => false
		(* end case *))
          in
            (n1 = n2) andalso cmp(start s1, start s2)
          end
      | equiv _ (EMPTY, EMPTY) = true
      | equiv _ _ = false

    fun collate rngCmp (EMPTY, EMPTY) = EQUAL
      | collate rngCmp (EMPTY, _) = LESS
      | collate rngCmp (_, EMPTY) = GREATER
      | collate rngCmp (MAP{root=s1, ...}, MAP{root=s2, ...}) = let
	  fun cmp (t1, t2) = (case (next t1, next t2)
		 of ((SplayNil, _), (SplayNil, _)) => EQUAL
		  | ((SplayNil, _), _) => LESS
		  | (_, (SplayNil, _)) => GREATER
		  | ((SplayObj{value=(xk, x), ...}, r1), (SplayObj{value=(yk, y), ...}, r2)) => (
		      case Key.compare(xk, yk)
		       of EQUAL => (case rngCmp (x, y)
			     of EQUAL => cmp (r1, r2)
			      | order => order
			    (* end case *))
			| order => order
		      (* end case *))
		(* end case *))
	  in
	    cmp (start s1, start s2)
	  end

    fun extends rngEx (MAP{root=s1, nobj=n1},  MAP{root=s2, nobj=n2}) = let
	  fun cmp (t1, t2) = (case (next t1, next t2)
		 of ((SplayNil, _), (SplayNil, _)) => true
		  | ((SplayNil, _), _) => false (* domain of second map is bigger than first *)
		  | (_, (SplayNil, _)) => true
		  | ((SplayObj{value=(xk, x), ...}, r1), (SplayObj{value=(yk, y), ...}, r2)) => (
		      case Key.compare(xk, yk)
		       of LESS => cmp (r1, t2)
                        | EQUAL => rngEx (x, y) andalso cmp (r1, r2)
                        | GREATER => false
		      (* end case *))
		(* end case *))
          in
            (n1 >= n2) andalso cmp (start s1, start s2)
          end
      | extends _ (EMPTY, MAP _) = false
      | extends _ _ = true
    end (* local *)

    (* Apply a function to the entries of the dictionary *)
    fun appi af EMPTY = ()
      | appi af (MAP{root,...}) =
          let fun apply SplayNil = ()
                | apply (SplayObj{value,left,right}) =
                    (apply left; af value; apply right)
        in
          apply (!root)
        end

    fun app af EMPTY = ()
      | app af (MAP{root,...}) =
          let fun apply SplayNil = ()
                | apply (SplayObj{value=(_,value),left,right}) =
                    (apply left; af value; apply right)
        in
          apply (!root)
        end
(*
    fun revapp af (MAP{root,...}) =
          let fun apply SplayNil = ()
                | apply (SplayObj{value,left,right}) =
                    (apply right; af value; apply left)
        in
          apply (!root)
        end
*)

	(* Fold function *)
    fun foldri (abf : K.ord_key * 'a * 'b -> 'b) b EMPTY = b
      | foldri (abf : K.ord_key * 'a * 'b -> 'b) b (MAP{root,...}) =
          let fun apply (SplayNil : (K.ord_key * 'a) splay, b) = b
                | apply (SplayObj{value,left,right},b) =
                    apply(left,abf(#1 value,#2 value,apply(right,b)))
        in
          apply (!root,b)
        end

    fun foldr (abf : 'a * 'b -> 'b) b EMPTY = b
      | foldr (abf : 'a * 'b -> 'b) b (MAP{root,...}) =
          let fun apply (SplayNil : (K.ord_key * 'a) splay, b) = b
                | apply (SplayObj{value=(_,value),left,right},b) =
                    apply(left,abf(value,apply(right,b)))
        in
          apply (!root,b)
        end

    fun foldli (abf : K.ord_key * 'a * 'b -> 'b) b EMPTY = b
      | foldli (abf : K.ord_key * 'a * 'b -> 'b) b (MAP{root,...}) =
          let fun apply (SplayNil : (K.ord_key * 'a) splay, b) = b
                | apply (SplayObj{value,left,right},b) =
                    apply(right,abf(#1 value,#2 value,apply(left,b)))
        in
          apply (!root,b)
        end

    fun foldl (abf : 'a * 'b -> 'b) b EMPTY = b
      | foldl (abf : 'a * 'b -> 'b) b (MAP{root,...}) =
          let fun apply (SplayNil : (K.ord_key * 'a) splay, b) = b
                | apply (SplayObj{value=(_,value),left,right},b) =
                    apply(right,abf(value,apply(left,b)))
        in
          apply (!root,b)
        end

	(* Map a table to a new table that has the same keys*)
    fun mapi (af : K.ord_key * 'a -> 'b) EMPTY = EMPTY
      | mapi (af : K.ord_key * 'a -> 'b) (MAP{root,nobj}) =
          let fun ap (SplayNil : (K.ord_key * 'a) splay) = SplayNil
                | ap (SplayObj{value,left,right}) = let
                    val left' = ap left
                    val value' = (#1 value, af value)
                    in
                      SplayObj{value = value', left = left', right = ap right}
                    end
        in
          MAP{root = ref(ap (!root)), nobj = nobj}
        end

    fun map (af : 'a -> 'b) EMPTY = EMPTY
      | map (af : 'a -> 'b) (MAP{root,nobj}) =
          let fun ap (SplayNil : (K.ord_key * 'a) splay) = SplayNil
                | ap (SplayObj{value,left,right}) = let
                    val left' = ap left
                    val value' = (#1 value, af (#2 value))
                    in
                      SplayObj{value = value', left = left', right = ap right}
                    end
        in
          MAP{root = ref(ap (!root)), nobj = nobj}
        end

(* the following are generic implementations of the unionWith, intersectWith,
 * and mergeWith operetions.  These should be specialized for the internal
 * representations at some point.
 *)
    fun unionWith f (m1, m2) = let
	  fun ins f (key, x, m) = (case find(m, key)
		 of NONE => insert(m, key, x)
		  | (SOME x') => insert(m, key, f(x, x'))
		(* end case *))
	  in
	    if (numItems m1 > numItems m2)
	      then foldli (ins (fn (a, b) => f(b, a))) m1 m2
	      else foldli (ins f) m2 m1
	  end
    fun unionWithi f (m1, m2) = let
	  fun ins f (key, x, m) = (case find(m, key)
		 of NONE => insert(m, key, x)
		  | (SOME x') => insert(m, key, f(key, x, x'))
		(* end case *))
	  in
	    if (numItems m1 > numItems m2)
	      then foldli (ins (fn (k, a, b) => f(k, b, a))) m1 m2
	      else foldli (ins f) m2 m1
	  end

    fun intersectWith f (m1, m2) = let
	(* iterate over the elements of m1, checking for membership in m2 *)
	  fun intersect f (m1, m2) = let
		fun ins (key, x, m) = (case find(m2, key)
		       of NONE => m
			| (SOME x') => insert(m, key, f(x, x'))
		      (* end case *))
		in
		  foldli ins empty m1
		end
	  in
	    if (numItems m1 > numItems m2)
	      then intersect f (m1, m2)
	      else intersect (fn (a, b) => f(b, a)) (m2, m1)
	  end

    fun intersectWithi f (m1, m2) = let
	(* iterate over the elements of m1, checking for membership in m2 *)
	  fun intersect f (m1, m2) = let
		fun ins (key, x, m) = (case find(m2, key)
		       of NONE => m
			| (SOME x') => insert(m, key, f(key, x, x'))
		      (* end case *))
		in
		  foldli ins empty m1
		end
	  in
	    if (numItems m1 > numItems m2)
	      then intersect f (m1, m2)
	      else intersect (fn (k, a, b) => f(k, b, a)) (m2, m1)
	  end

    fun mergeWith f (m1, m2) = let
	  fun merge ([], [], m) = m
	    | merge ((k1, x1)::r1, [], m) = mergef (k1, SOME x1, NONE, r1, [], m)
	    | merge ([], (k2, x2)::r2, m) = mergef (k2, NONE, SOME x2, [], r2, m)
	    | merge (m1 as ((k1, x1)::r1), m2 as ((k2, x2)::r2), m) = (
		case Key.compare (k1, k2)
		 of LESS => mergef (k1, SOME x1, NONE, r1, m2, m)
		  | EQUAL => mergef (k1, SOME x1, SOME x2, r1, r2, m)
		  | GREATER => mergef (k2, NONE, SOME x2, m1, r2, m)
		(* end case *))
	  and mergef (k, x1, x2, r1, r2, m) = (case f (x1, x2)
		 of NONE => merge (r1, r2, m)
		  | SOME y => merge (r1, r2, insert(m, k, y))
		(* end case *))
	  in
	    merge (listItemsi m1, listItemsi m2, empty)
	  end

    fun mergeWithi f (m1, m2) = let
	  fun merge ([], [], m) = m
	    | merge ((k1, x1)::r1, [], m) = mergef (k1, SOME x1, NONE, r1, [], m)
	    | merge ([], (k2, x2)::r2, m) = mergef (k2, NONE, SOME x2, [], r2, m)
	    | merge (m1 as ((k1, x1)::r1), m2 as ((k2, x2)::r2), m) = (
		case Key.compare (k1, k2)
		 of LESS => mergef (k1, SOME x1, NONE, r1, m2, m)
		  | EQUAL => mergef (k1, SOME x1, SOME x2, r1, r2, m)
		  | GREATER => mergef (k2, NONE, SOME x2, m1, r2, m)
		(* end case *))
	  and mergef (k, x1, x2, r1, r2, m) = (case f (k, x1, x2)
		 of NONE => merge (r1, r2, m)
		  | SOME y => merge (r1, r2, insert(m, k, y))
		(* end case *))
	  in
	    merge (listItemsi m1, listItemsi m2, empty)
	  end

  (* this is a generic implementation of mapPartial.  It should
   * be specialized to the data-structure at some point.
   *)
    fun mapPartial f m = let
	  fun g (key, item, m) = (case f item
		 of NONE => m
		  | (SOME item') => insert(m, key, item')
		(* end case *))
	  in
	    foldli g empty m
	  end
    fun mapPartiali f m = let
	  fun g (key, item, m) = (case f(key, item)
		 of NONE => m
		  | (SOME item') => insert(m, key, item')
		(* end case *))
	  in
	    foldli g empty m
	  end

  (* this is a generic implementation of filter.  It should
   * be specialized to the data-structure at some point.
   *)
    fun filter predFn m = let
	  fun f (key, item, m) = if predFn item
		then insert(m, key, item)
		else m
	  in
	    foldli f empty m
	  end
    fun filteri predFn m = let
	  fun f (key, item, m) = if predFn(key, item)
		then insert(m, key, item)
		else m
	  in
	    foldli f empty m
	  end

  (* check the elements of a map with a predicate and return true if
   * any element satisfies the predicate. Return false otherwise.
   * Elements are checked in key order.
   *)
    fun exists pred = let
	  fun exists' SplayNil = false
	    | exists' (SplayObj{value=(_, x), left, right}) =
		exists' left orelse pred x orelse exists' right
	  in
	    fn EMPTY => false | (MAP{root, ...}) => exists' (!root)
	  end
    fun existsi pred = let
	  fun exists' SplayNil = false
	    | exists' (SplayObj{value, left, right}) =
		exists' left orelse pred value orelse exists' right
	  in
	    fn EMPTY => false | (MAP{root, ...}) => exists' (!root)
	  end

  (* check the elements of a map with a predicate and return true if
   * they all satisfy the predicate. Return false otherwise.  Elements
   * are checked in key order.
   *)
    fun all pred = let
	  fun all' SplayNil = true
	    | all' (SplayObj{value=(_, x), left, right}) =
		all' left andalso pred x andalso all' right
	  in
	    fn EMPTY => true | (MAP{root, ...}) => all' (!root)
	  end
    fun alli pred = let
	  fun all' SplayNil = true
	    | all' (SplayObj{value, left, right}) = all' left andalso pred value andalso all' right
	  in
	    fn EMPTY => true | (MAP{root, ...}) => all' (!root)
	  end

  end (* SplayDictFn *)
