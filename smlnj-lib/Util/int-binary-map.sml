(* int-binary-map.sml
 *
 * COPYRIGHT (c) 2012 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * This code was adapted from Stephen Adams' binary tree implementation
 * of applicative integer sets.
 *
 *   Copyright 1992 Stephen Adams.
 *
 *    This software may be used freely provided that:
 *      1. This copyright notice is attached to any copy, derived work,
 *         or work including all or part of this software.
 *      2. Any derived work must contain a prominent notice stating that
 *         it has been altered from the original.
 *
 *
 *   Name(s): Stephen Adams.
 *   Department, Institution: Electronics & Computer Science,
 *      University of Southampton
 *   Address:  Electronics & Computer Science
 *             University of Southampton
 *	     Southampton  SO9 5NH
 *	     Great Britian
 *   E-mail:   sra@ecs.soton.ac.uk
 *
 *   Comments:
 *
 *     1.  The implementation is based on Binary search trees of Bounded
 *         Balance, similar to Nievergelt & Reingold, SIAM J. Computing
 *         2(1), March 1973.  The main advantage of these trees is that
 *         they keep the size of the tree in the node, giving a constant
 *         time size operation.
 *
 *     2.  The bounded balance criterion is simpler than N&R's alpha.
 *         Simply, one subtree must not have more than `weight' times as
 *         many elements as the opposite subtree.  Rebalancing is
 *         guaranteed to reinstate the criterion for weight>2.23, but
 *         the occasional incorrect behaviour for weight=2 is not
 *         detrimental to performance.
 *
 *  Altered to work as a geneal intmap - Emden Gansner
 *)

structure IntBinaryMap :> ORD_MAP where type Key.ord_key = Int.int =
  struct

    structure Key =
      struct
	type ord_key = Int.int
	val compare = Int.compare
      end

    (*
    **  val weight = 3
    **  fun wt i = weight * i
    *)
    fun wt (i : int) = i + i + i

    datatype 'a map
      = E
      | T of {
          key : int,
          value : 'a,
          cnt : int,
          left : 'a map,
          right : 'a map
	}

    fun isEmpty E = true
      | isEmpty _ = false

    fun numItems E = 0
      | numItems (T{cnt,...}) = cnt

  (* return the first item in the map (or NONE if it is empty) *)
    fun first E = NONE
      | first (T{value, left=E, ...}) = SOME value
      | first (T{left, ...}) = first left

  (* return the first item in the map and its key (or NONE if it is empty) *)
    fun firsti E = NONE
      | firsti (T{key, value, left=E, ...}) = SOME(key, value)
      | firsti (T{left, ...}) = firsti left

    local
      fun N(k,v,E,E) = T{key=k,value=v,cnt=1,left=E,right=E}
        | N(k,v,E,r as T n) = T{key=k,value=v,cnt=1+(#cnt n),left=E,right=r}
        | N(k,v,l as T n,E) = T{key=k,value=v,cnt=1+(#cnt n),left=l,right=E}
        | N(k,v,l as T n,r as T n') =
            T{key=k,value=v,cnt=1+(#cnt n)+(#cnt n'),left=l,right=r}

      fun single_L (a,av,x,T{key=b,value=bv,left=y,right=z,...}) =
            N(b,bv,N(a,av,x,y),z)
        | single_L _ = raise Match
      fun single_R (b,bv,T{key=a,value=av,left=x,right=y,...},z) =
            N(a,av,x,N(b,bv,y,z))
        | single_R _ = raise Match
      fun double_L (a,av,w,T{key=c,value=cv,left=T{key=b,value=bv,left=x,right=y,...},right=z,...}) =
            N(b,bv,N(a,av,w,x),N(c,cv,y,z))
        | double_L _ = raise Match
      fun double_R (c,cv,T{key=a,value=av,left=w,right=T{key=b,value=bv,left=x,right=y,...},...},z) =
            N(b,bv,N(a,av,w,x),N(c,cv,y,z))
        | double_R _ = raise Match

      fun T' (k,v,E,E) = T{key=k,value=v,cnt=1,left=E,right=E}
        | T' (k,v,E,r as T{right=E,left=E,...}) =
            T{key=k,value=v,cnt=2,left=E,right=r}
        | T' (k,v,l as T{right=E,left=E,...},E) =
            T{key=k,value=v,cnt=2,left=l,right=E}

        | T' (p as (_,_,E,T{left=T _,right=E,...})) = double_L p
        | T' (p as (_,_,T{left=E,right=T _,...},E)) = double_R p

          (* these cases almost never happen with small weight*)
        | T' (p as (_,_,E,T{left=T{cnt=ln,...},right=T{cnt=rn,...},...})) =
            if ln < rn then single_L p else double_L p
        | T' (p as (_,_,T{left=T{cnt=ln,...},right=T{cnt=rn,...},...},E)) =
            if ln > rn then single_R p else double_R p

        | T' (p as (_,_,E,T{left=E,...})) = single_L p
        | T' (p as (_,_,T{right=E,...},E)) = single_R p

        | T' (p as (k,v,l as T{cnt=ln,left=ll,right=lr,...},
                        r as T{cnt=rn,left=rl,right=rr,...})) =
            if rn >= wt ln then (*right is too big*)
              let val rln = numItems rl
                  val rrn = numItems rr
              in
                if rln < rrn then  single_L p  else  double_L p
              end

            else if ln >= wt rn then  (*left is too big*)
              let val lln = numItems ll
                  val lrn = numItems lr
              in
                if lrn < lln then  single_R p  else  double_R p
              end

            else T{key=k,value=v,cnt=ln+rn+1,left=l,right=r}

      local
        fun min (T{left=E,key,value,...}) = (key,value)
          | min (T{left,...}) = min left
          | min _ = raise Match

        fun delmin (T{left=E,right,...}) = right
          | delmin (T{key,value,left,right,...}) = T'(key,value,delmin left,right)
          | delmin _ = raise Match
      in
        fun delete' (E,r) = r
          | delete' (l,E) = l
          | delete' (l,r) = let
              val (mink,minv) = min r
              in
                T'(mink,minv,l,delmin r)
              end
      end (* local *)
    in
    val empty = E

    fun singleton (x,v) = T{key=x,value=v,cnt=1,left=E,right=E}

    fun insert (E,x,v) = T{key=x,value=v,cnt=1,left=E,right=E}
      | insert (T(set as {key,left,right,value,...}),x,v) =
          if key > x then T'(key,value,insert(left,x,v),right)
          else if key < x then T'(key,value,left,insert(right,x,v))
          else T{key=x,value=v,left=left,right=right,cnt= #cnt set}
    fun insert' ((k, x), m) = insert(m, k, x)

    fun insertWithi comb (m, x, v) = let
	  fun insert E = T{key=x, value=v, cnt=1, left=E, right=E}
	    | insert (T{key, left, right, value, cnt}) =
		if key > x then T'(key, value, insert left,right)
		else if key < x then T'(key, value, left, insert right)
		else let
		  val v' = comb(x, value, v)
		  in
		    T{key=x,value=v,left=left,right=right,cnt=cnt}
		  end
	  in
	    insert m
	  end
    fun insertWith comb = insertWithi (fn (_, x1, x2) => comb(x1, x2))

    fun inDomain (set, x) = let
	  fun mem E = false
	    | mem (T(n as {key,left,right,...})) =
		if x > key then mem right
		else if x < key then mem left
		else true
	  in
	    mem set
	  end

    fun find (set, x) = let
	  fun mem E = NONE
	    | mem (T(n as {key,left,right,...})) =
		if x > key then mem right
		else if x < key then mem left
		else SOME(#value n)
	  in
	    mem set
	  end

    fun lookup (set, x) = let
	  fun mem E = raise LibBase.NotFound
	    | mem (T(n as {key,left,right,...})) =
		if x > key then mem right
		else if x < key then mem left
		else #value n
	  in
	    mem set
	  end

    fun remove (E,x) = raise LibBase.NotFound
      | remove (set as T{key,left,right,value,...},x) =
          if key > x then
            let val (left',v) = remove(left,x)
            in (T'(key,value,left',right),v) end
          else if key < x then
            let val (right',v) = remove(right,x)
            in (T'(key,value,left,right'),v) end
          else (delete'(left,right),value)

    fun findAndRemove arg = SOME(remove arg) handle LibBase.NotFound => NONE

    fun listItems d = let
	  fun d2l (E, l) = l
	    | d2l (T{key,value,left,right,...}, l) =
		d2l(left, value::(d2l(right,l)))
	  in
	    d2l (d,[])
	  end

    fun listItemsi d = let
	  fun d2l (E, l) = l
	    | d2l (T{key,value,left,right,...}, l) =
		d2l(left, (key,value)::(d2l(right,l)))
	  in
	    d2l (d,[])
	  end

    fun listKeys d = let
	  fun d2l (E, l) = l
	    | d2l (T{key,left,right,...}, l) = d2l(left, key::(d2l(right,l)))
	  in
	    d2l (d,[])
	  end

    local
      fun next ((t as T{right, ...})::rest) = (t, left(right, rest))
	| next _ = (E, [])
      and left (E, rest) = rest
	| left (t as T{left=l, ...}, rest) = left(l, t::rest)
      fun start m = left(m, [])
    in
    fun equiv rngEq (m1, m2) = let
	  fun cmp (t1, t2) = (case (next t1, next t2)
		 of ((E, _), (E, _)) => true
		  | ((E, _), _) => false
		  | (_, (E, _)) => false
                  | ((T{key=xk, value=x, ...}, r1), (T{key=yk, value=y, ...}, r2)) =>
                      (xk = yk) andalso rngEq(x, y) andalso cmp (r1, r2)
		(* end case *))
	  in
	    cmp (start m1, start m2)
	  end
    fun collate rngCmp (m1, m2) = let
	  fun cmp (t1, t2) = (case (next t1, next t2)
		 of ((E, _), (E, _)) => EQUAL
		  | ((E, _), _) => LESS
		  | (_, (E, _)) => GREATER
                  | ((T{key=xk, value=x, ...}, r1), (T{key=yk, value=y, ...}, r2)) =>
                      if (xk = yk)
			then (case rngCmp(x, y)
			   of EQUAL => cmp (r1, r2)
			    | order => order
			  (* end case *))
		      else if (xk < yk)
			then LESS
			else GREATER
		(* end case *))
	  in
	    cmp (start m1, start m2)
	  end
    fun extends rngEx (m1, m2) = let
          (* does t1 extend t2? *)
	  fun cmp (t1, t2) = (case (next t1, next t2)
		 of ((E, _), (E, _)) => true
		  | (_, (E, _)) => true
		  | ((E, _), _) => false
                  | ((T{key=xk, value=x, ...}, r1), (T{key=yk, value=y, ...}, r2)) =>
                      if (xk < yk) then cmp (r1, t2)
                      else (yk = xk) andalso rngEx(x, y) andalso cmp (r1, r2)
		(* end case *))
	  in
	    cmp (start m1, start m2)
	  end
    end (* local *)

    fun appi f d = let
	  fun appf E = ()
	    | appf (T{key,value,left,right,...}) = (
		appf left; f(key,value); appf right)
	  in
	    appf d
	  end
    fun app f d = appi (fn (_, v) => f v) d

    fun mapi f d = let
	  fun mapf E = E
	    | mapf (T{key,value,left,right,cnt}) = let
		val left' = mapf left
		val value' = f(key, value)
		val right' = mapf right
		in
		  T{cnt=cnt, key=key, value=value', left = left', right = right'}
		end
	  in
	    mapf d
	  end
    fun map f d = mapi (fn (_, x) => f x) d

    fun foldli f init d = let
	  fun fold (E,v) = v
	    | fold (T{key,value,left,right,...},v) =
		fold (right, f(key, value, fold(left, v)))
	  in
	    fold (d, init)
	  end
    fun foldl f init d = foldli (fn (_, v, accum) => f (v, accum)) init d

    fun foldri f init d = let
	  fun fold (E,v) = v
	    | fold (T{key,value,left,right,...},v) =
		fold (left, f(key, value, fold(right, v)))
	  in
	    fold (d, init)
	  end
    fun foldr f init d = foldri (fn (_, v, accum) => f (v, accum)) init d

    end (* local *)

(* the following are generic implementations of the unionWith, intersectWith,
 * and mergeWith operetions.  These should be specialized for the internal
 * representations at some point.
 *)
    fun unionWith f (m1, m2) = let
	  fun ins  f (key, x, m) = (case find(m, key)
		 of NONE => insert(m, key, x)
		  | (SOME x') => insert(m, key, f(x, x'))
		(* end case *))
	  in
	    if (numItems m1 > numItems m2)
	      then foldli (ins (fn (a, b) => f (b, a))) m1 m2
	      else foldli (ins f) m2 m1
	  end
    fun unionWithi f (m1, m2) = let
	  fun ins f (key, x, m) = (case find(m, key)
		 of NONE => insert(m, key, x)
		  | (SOME x') => insert(m, key, f(key, x, x'))
		(* end case *))
	  in
	    if (numItems m1 > numItems m2)
	      then foldli (ins (fn (k, a, b) => f (k, b, a))) m1 m2
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
		if (k1 < k2)
		  then mergef (k1, SOME x1, NONE, r1, m2, m)
		else if (k1 = k2)
		  then mergef (k1, SOME x1, SOME x2, r1, r2, m)
		  else mergef (k2, NONE, SOME x2, m1, r2, m)
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
		if (k1 < k2)
		  then mergef (k1, SOME x1, NONE, r1, m2, m)
		else if (k1 = k2)
		  then mergef (k1, SOME x1, SOME x2, r1, r2, m)
		  else mergef (k2, NONE, SOME x2, m1, r2, m)
		(* end case *))
	  and mergef (k, x1, x2, r1, r2, m) = (case f (k, x1, x2)
		 of NONE => merge (r1, r2, m)
		  | SOME y => merge (r1, r2, insert(m, k, y))
		(* end case *))
	  in
	    merge (listItemsi m1, listItemsi m2, empty)
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

  (* check the elements of a map with a predicate and return true if
   * any element satisfies the predicate. Return false otherwise.
   * Elements are checked in key order.
   *)
    fun exists pred = let
	  fun exists' E = false
	    | exists' (T{value, left, right, ...}) =
		exists' left orelse pred value orelse exists' right
	  in
	    exists'
	  end
    fun existsi pred = let
	  fun exists' E = false
	    | exists' (T{key, value, left, right, ...}) =
		exists' left orelse pred(key, value) orelse exists' right
	  in
	    exists'
	  end

  (* check the elements of a map with a predicate and return true if
   * they all satisfy the predicate. Return false otherwise.  Elements
   * are checked in key order.
   *)
    fun all pred = let
	  fun all' E = true
	    | all' (T{value, left, right, ...}) =
		all' left andalso pred value andalso all' right
	  in
	    all'
	  end
    fun alli pred = let
	  fun all' E = true
	    | all' (T{key, value, left, right, ...}) =
		all' left andalso pred(key, value) andalso all' right
	  in
	    all'
	  end

  end
