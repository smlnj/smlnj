(* int-binary-set.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * This code was adapted from Stephen Adams' binary tree implementation
 * of applicative integer sets.
 *
 *  Copyright 1992 Stephen Adams.
 *
 *  This software may be used freely provided that:
 *    1. This copyright notice is attached to any copy, derived work,
 *       or work including all or part of this software.
 *    2. Any derived work must contain a prominent notice stating that
 *       it has been altered from the original.
 *
 *  Altered to conform to SML library interface - Emden Gansner
 *
 *
 * Name(s): Stephen Adams.
 * Department, Institution: Electronics & Computer Science,
 *    University of Southampton
 * Address:  Electronics & Computer Science
 *           University of Southampton
 *           Southampton  SO9 5NH
 *           Great Britian
 * E-mail:   sra@ecs.soton.ac.uk
 *
 * Comments:
 *
 *   1.  The implementation is based on Binary search trees of Bounded
 *       Balance, similar to Nievergelt & Reingold, SIAM J. Computing
 *       2(1), March 1973.  The main advantage of these trees is that
 *       they keep the size of the tree in the node, giving a constant
 *       time size operation.
 *
 *   2.  The bounded balance criterion is simpler than N&R's alpha.
 *       Simply, one subtree must not have more than `weight' times as
 *       many elements as the opposite subtree.  Rebalancing is
 *       guaranteed to reinstate the criterion for weight>2.23, but
 *       the occasional incorrect behaviour for weight=2 is not
 *       detrimental to performance.
 *
 *   3.  There are two implementations of union.  The default,
 *       hedge_union, is much more complex and usually 20% faster.  I
 *       am not sure that the performance increase warrants the
 *       complexity (and time it took to write), but I am leaving it
 *       in for the competition.  It is derived from the original
 *       union by replacing the split_lt(gt) operations with a lazy
 *       version. The `obvious' version is called old_union.
 *
 *   4.  Most time is spent in T', the rebalancing constructor.  If my
 *       understanding of the output of *<file> in the sml batch
 *       compiler is correct then the code produced by NJSML 0.75
 *       (sparc) for the final case is very disappointing.  Most
 *       invocations fall through to this case and most of these cases
 *       fall to the else part, i.e. the plain contructor,
 *       T(v,ln+rn+1,l,r).  The poor code allocates a 16 word vector
 *       and saves lots of registers into it.  In the common case it
 *       then retrieves a few of the registers and allocates the 5
 *       word T node.  The values that it retrieves were live in
 *       registers before the massive save.
 *)

structure IntBinarySet :> ORD_SET where type Key.ord_key = Int.int =
  struct

    structure Key =
      struct
	type ord_key = Int.int
	val compare = Int.compare
      end

    type item = Key.ord_key

    datatype set
      = E
      | T of {
	  elt : item,
          cnt : int,
          left : set,
          right : set
	}

    fun numItems E = 0
      | numItems (T{cnt,...}) = cnt

    fun isEmpty E = true
      | isEmpty _ = false

    fun minItem E = raise Empty
      | minItem (T{elt, left=E, ...}) = elt
      | minItem (T{left, ...}) = minItem left

    fun maxItem E = raise Empty
      | maxItem (T{elt, right=E, ...}) = elt
      | maxItem (T{right, ...}) = maxItem right

    fun mkT(v,n,l,r) = T{elt=v,cnt=n,left=l,right=r}

      (* N(v,l,r) = T(v,1+numItems(l)+numItems(r),l,r) *)
    fun N(v,E,E) = mkT(v,1,E,E)
      | N(v,E,r as T{cnt=n,...}) = mkT(v,n+1,E,r)
      | N(v,l as T{cnt=n,...}, E) = mkT(v,n+1,l,E)
      | N(v,l as T{cnt=n,...}, r as T{cnt=m,...}) = mkT(v,n+m+1,l,r)

    fun single_L (a,x,T{elt=b,left=y,right=z,...}) = N(b,N(a,x,y),z)
      | single_L _ = raise Match
    fun single_R (b,T{elt=a,left=x,right=y,...},z) = N(a,x,N(b,y,z))
      | single_R _ = raise Match
    fun double_L (a,w,T{elt=c,left=T{elt=b,left=x,right=y,...},right=z,...}) =
          N(b,N(a,w,x),N(c,y,z))
      | double_L _ = raise Match
    fun double_R (c,T{elt=a,left=w,right=T{elt=b,left=x,right=y,...},...},z) =
          N(b,N(a,w,x),N(c,y,z))
      | double_R _ = raise Match

    (*
    **  val weight = 3
    **  fun wt i = weight * i
    *)
    fun wt (i : int) = i + i + i

    fun T' (v,E,E) = mkT(v,1,E,E)
      | T' (v,E,r as T{left=E,right=E,...}) = mkT(v,2,E,r)
      | T' (v,l as T{left=E,right=E,...},E) = mkT(v,2,l,E)

      | T' (p as (_,E,T{left=T _,right=E,...})) = double_L p
      | T' (p as (_,T{left=E,right=T _,...},E)) = double_R p

        (* these cases almost never happen with small weight*)
      | T' (p as (_,E,T{left=T{cnt=ln,...},right=T{cnt=rn,...},...})) =
            if ln<rn then single_L p else double_L p
      | T' (p as (_,T{left=T{cnt=ln,...},right=T{cnt=rn,...},...},E)) =
            if ln>rn then single_R p else double_R p

      | T' (p as (_,E,T{left=E,...})) = single_L p
      | T' (p as (_,T{right=E,...},E)) = single_R p

      | T' (p as (v,l as T{elt=lv,cnt=ln,left=ll,right=lr},
              r as T{elt=rv,cnt=rn,left=rl,right=rr})) =
          if rn >= wt ln (*right is too big*)
            then
              let val rln = numItems rl
                  val rrn = numItems rr
              in
                if rln < rrn then single_L p else double_L p
              end
          else if ln >= wt rn (*left is too big*)
            then
              let val lln = numItems ll
                  val lrn = numItems lr
              in
                if lrn < lln then single_R p else double_R p
              end
          else mkT(v,ln+rn+1,l,r)

    fun add (E,x) = mkT(x,1,E,E)
      | add (set as T{elt=v,left=l,right=r,cnt},x) = (
          case Key.compare(x,v)
	   of LESS => T'(v,add(l,x),r)
	    | GREATER => T'(v,l,add(r,x))
	    | EQUAL => mkT(x,cnt,l,r)
	  (* end case *))
    fun add' (s, x) = add(x, s)

    fun concat3 (E,v,r) = add(r,v)
      | concat3 (l,v,E) = add(l,v)
      | concat3 (l as T{elt=v1,cnt=n1,left=l1,right=r1}, v,
                  r as T{elt=v2,cnt=n2,left=l2,right=r2}) =
        if wt n1 < n2 then T'(v2,concat3(l,v,l2),r2)
        else if wt n2 < n1 then T'(v1,l1,concat3(r1,v,r))
        else N(v,l,r)

    fun split_lt (E,x) = E
      | split_lt (T{elt=v,left=l,right=r,...},x) =
          case Key.compare(v,x) of
            GREATER => split_lt(l,x)
          | LESS => concat3(l,v,split_lt(r,x))
          | _ => l

    fun split_gt (E,x) = E
      | split_gt (T{elt=v,left=l,right=r,...},x) =
          case Key.compare(v,x) of
            LESS => split_gt(r,x)
          | GREATER => concat3(split_gt(l,x),v,r)
          | _ => r

    fun min (T{elt=v,left=E,...}) = v
      | min (T{left=l,...}) = min l
      | min _ = raise Match

    fun delmin (T{left=E,right=r,...}) = r
      | delmin (T{elt=v,left=l,right=r,...}) = T'(v,delmin l,r)
      | delmin _ = raise Match

    fun delete' (E,r) = r
      | delete' (l,E) = l
      | delete' (l,r) = T'(min r,l,delmin r)

    fun concat (E, s) = s
      | concat (s, E) = s
      | concat (t1 as T{elt=v1,cnt=n1,left=l1,right=r1},
                  t2 as T{elt=v2,cnt=n2,left=l2,right=r2}) =
          if wt n1 < n2 then T'(v2,concat(t1,l2),r2)
          else if wt n2 < n1 then T'(v1,l1,concat(r1,t2))
          else T'(min t2,t1, delmin t2)


    local
      fun trim (lo,hi,E) = E
        | trim (lo,hi,s as T{elt=v,left=l,right=r,...}) =
            if (v > lo)
              then if (v < hi) then s else trim(lo,hi,l)
              else trim(lo,hi,r)

      fun uni_bd (s,E,_,_) = s
        | uni_bd (E,T{elt=v,left=l,right=r,...},lo,hi) =
             concat3(split_gt(l,lo),v,split_lt(r,hi))
        | uni_bd (T{elt=v,left=l1,right=r1,...},
                   s2 as T{elt=v2,left=l2,right=r2,...},lo,hi) =
            concat3(uni_bd(l1,trim(lo,v,s2),lo,v),
                v,
                uni_bd(r1,trim(v,hi,s2),v,hi))
              (* inv:  lo < v < hi *)

        (* all the other versions of uni and trim are
         * specializations of the above two functions with
         *     lo=-infinity and/or hi=+infinity
         *)

      fun trim_lo (_, E) = E
        | trim_lo (lo,s as T{elt=v,right=r,...}) =
            case Key.compare(v,lo) of
              GREATER => s
            | _ => trim_lo(lo,r)

      fun trim_hi (_, E) = E
        | trim_hi (hi,s as T{elt=v,left=l,...}) =
            case Key.compare(v,hi) of
              LESS => s
            | _ => trim_hi(hi,l)

      fun uni_hi (s,E,_) = s
        | uni_hi (E,T{elt=v,left=l,right=r,...},hi) =
             concat3(l,v,split_lt(r,hi))
        | uni_hi (T{elt=v,left=l1,right=r1,...},
                   s2 as T{elt=v2,left=l2,right=r2,...},hi) =
            concat3(uni_hi(l1,trim_hi(v,s2),v),v,uni_bd(r1,trim(v,hi,s2),v,hi))

      fun uni_lo (s,E,_) = s
        | uni_lo (E,T{elt=v,left=l,right=r,...},lo) =
             concat3(split_gt(l,lo),v,r)
        | uni_lo (T{elt=v,left=l1,right=r1,...},
                   s2 as T{elt=v2,left=l2,right=r2,...},lo) =
            concat3(uni_bd(l1,trim(lo,v,s2),lo,v),v,uni_lo(r1,trim_lo(v,s2),v))

      fun uni (s,E) = s
        | uni (E,s) = s
        | uni (T{elt=v,left=l1,right=r1,...},
                s2 as T{elt=v2,left=l2,right=r2,...}) =
            concat3(uni_hi(l1,trim_hi(v,s2),v), v, uni_lo(r1,trim_lo(v,s2),v))

    in
      val hedge_union = uni
    end

      (* The old_union version is about 20% slower than
       *  hedge_union in most cases
       *)
    fun old_union (E,s2)  = s2
      | old_union (s1,E)  = s1
      | old_union (T{elt=v,left=l,right=r,...},s2) =
          let val l2 = split_lt(s2,v)
              val r2 = split_gt(s2,v)
          in
            concat3(old_union(l,l2),v,old_union(r,r2))
          end

    val empty = E
    fun singleton x = T{elt=x,cnt=1,left=E,right=E}

    fun addList (s,l) = List.foldl (fn (i,s) => add(s,i)) s l

    fun fromList l = addList (E, l)

    val add = add

    fun member (set, x) = let
	  fun pk E = false
	    | pk (T{elt=v, left=l, right=r, ...}) = (
		case Key.compare(x,v)
		 of LESS => pk l
		  | EQUAL => true
		  | GREATER => pk r
		(* end case *))
	  in
	    pk set
	  end

    local
        (* true if every item in t is in t' *)
      fun treeIn (t,t') = let
            fun isIn E = true
              | isIn (T{elt,left=E,right=E,...}) = member(t',elt)
              | isIn (T{elt,left,right=E,...}) =
                  member(t',elt) andalso isIn left
              | isIn (T{elt,left=E,right,...}) =
                  member(t',elt) andalso isIn right
              | isIn (T{elt,left,right,...}) =
                  member(t',elt) andalso isIn left andalso isIn right
            in
              isIn t
            end
    in
    fun isSubset (E,_) = true
      | isSubset (_,E) = false
      | isSubset (t as T{cnt=n,...},t' as T{cnt=n',...}) =
          (n<=n') andalso treeIn (t,t')

    fun equal (E,E) = true
      | equal (t as T{cnt=n,...},t' as T{cnt=n',...}) =
          (n=n') andalso treeIn (t,t')
      | equal _ = false
    end

    local
      fun next ((t as T{right, ...})::rest) = (t, left(right, rest))
	| next _ = (E, [])
      and left (E, rest) = rest
	| left (t as T{left=l, ...}, rest) = left(l, t::rest)
    in
    fun compare (s1, s2) = let
	  fun cmp (t1, t2) = (case (next t1, next t2)
		 of ((E, _), (E, _)) => EQUAL
		  | ((E, _), _) => LESS
		  | (_, (E, _)) => GREATER
		  | ((T{elt=e1, ...}, r1), (T{elt=e2, ...}, r2)) => (
		      case Key.compare(e1, e2)
		       of EQUAL => cmp (r1, r2)
			| order => order
		      (* end case *))
		(* end case *))
	  in
	    cmp (left(s1, []), left(s2, []))
	  end

    fun disjoint (s1, s2) = let
	  fun walk (t1, t2) = (case (next t1, next t2)
		 of ((E, _), _) => true
		  | (_, (E, _)) => true
		  | ((T{elt=e1, ...}, r1), (T{elt=e2, ...}, r2)) =>
		      ((e1 < e2) andalso walk (r1, t2))
		      orelse ((e1 > e2) andalso walk (t1, r2))
		(* end case *))
	  in
	    walk (left(s1, []), left(s2, []))
	  end
    end

    fun delete (E,x) = raise LibBase.NotFound
      | delete (set as T{elt=v,left=l,right=r,...},x) =
          case Key.compare(x,v) of
            LESS => T'(v,delete(l,x),r)
          | GREATER => T'(v,l,delete(r,x))
          | _ => delete'(l,r)

    val union = hedge_union

    fun intersection (E, _) = E
      | intersection (_, E) = E
      | intersection (s, T{elt=v,left=l,right=r,...}) = let
	  val l2 = split_lt(s,v)
	  val r2 = split_gt(s,v)
          in
            if member(s,v)
	      then concat3(intersection(l2,l),v,intersection(r2,r))
	      else concat(intersection(l2,l),intersection(r2,r))
          end

    fun difference (E,s) = E
      | difference (s,E)  = s
      | difference (s, T{elt=v,left=l,right=r,...}) =
          let val l2 = split_lt(s,v)
              val r2 = split_gt(s,v)
          in
            concat(difference(l2,l),difference(r2,r))
          end

    fun subtract (s, item) = difference (s, singleton item)
    fun subtract' (item, s) = subtract (s, item)

    fun subtractList (l, items) = let
	  val items' = List.foldl (fn (x, set) => add(set, x)) E items
	  in
	    difference (l, items')
	  end

    fun map f set = let
	  fun map'(acc, E) = acc
	    | map'(acc, T{elt,left,right,...}) =
		map' (add (map' (acc, left), f elt), right)
	  in
	    map' (E, set)
	  end

    fun mapPartial f set = let
	  fun map' (acc, E) = acc
	    | map' (acc, T{elt, left, right, ...}) = let
		val acc = map' (acc, left)
		in
		  case f elt
		   of NONE => map' (acc, right)
		    | SOME elt' => map' (add (acc, elt'), right)
		  (* end case *)
		end
	  in
	    map' (E, set)
	  end

    fun app apf =
         let fun apply E = ()
               | apply (T{elt,left,right,...}) =
                   (apply left;apf elt; apply right)
         in
           apply
         end

    fun foldl f b set = let
	  fun foldf (E, b) = b
	    | foldf (T{elt,left,right,...}, b) =
		foldf (right, f(elt, foldf (left, b)))
          in
            foldf (set, b)
          end

    fun foldr f b set = let
	  fun foldf (E, b) = b
	    | foldf (T{elt,left,right,...}, b) =
		foldf (left, f(elt, foldf (right, b)))
          in
            foldf (set, b)
          end

    fun toList set = foldr (op::) [] set

    fun filter pred set =
	  foldl (fn (item, s) => if (pred item) then add(s, item) else s)
	    empty set

    fun partition pred set =
	  foldl
	    (fn (item, (s1, s2)) =>
		if (pred item) then (add(s1, item), s2) else (s1, add(s2, item))
	    )
	      (empty, empty) set

    fun exists p E = false
      | exists p (T{elt, left, right,...}) =
	  (exists p left) orelse (p elt) orelse (exists p right)

    fun all p E = true
      | all p (T{elt, left, right,...}) =
	  (all p left) andalso (p elt) andalso (all p right)

    fun find p E = NONE
      | find p (T{elt,left,right,...}) = (case find p left
	   of NONE => if (p elt)
		then SOME elt
		else find p right
	    | a => a
	  (* end case *))

  (* deprecated *)
    val listItems = toList

  end (* IntBinarySet *)
