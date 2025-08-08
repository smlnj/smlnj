(* splay-set-fn.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *
 * Functor implementing ordered sets using splay trees.
 *
 *)

functor SplaySetFn (K : ORD_KEY) :> ORD_SET where type Key.ord_key = K.ord_key =
  struct
    structure Key = K
    open SplayTree

    type item = K.ord_key

    datatype set
      = EMPTY
      | SET of {
	  root : item splay ref,
	  nobj : int
	}

    fun cmpf k = fn k' => K.compare(k',k)

    val empty = EMPTY

    fun singleton v = SET{root = ref(SplayObj{value=v,left=SplayNil,right=SplayNil}),nobj=1}

	(* Primitive insertion.
	 *)
    fun insert (v,(nobj,root)) =
          case splay (cmpf v, root) of
            (EQUAL,SplayObj{value,left,right}) =>
              (nobj,SplayObj{value=v,left=left,right=right})
          | (LESS,SplayObj{value,left,right}) =>
              (nobj+1,
               SplayObj{
                 value=v,
                 left=SplayObj{value=value,left=left,right=SplayNil},
                 right=right})
          | (GREATER,SplayObj{value,left,right}) =>
              (nobj+1,
               SplayObj{
                  value=v,
                  left=left,
                  right=SplayObj{value=value,left=SplayNil,right=right}})
          | (_,SplayNil) => (1,SplayObj{value=v,left=SplayNil,right=SplayNil})

	(* Add an item.
	 *)
    fun add (EMPTY,v) = singleton v
      | add (SET{root,nobj},v) = let
          val (cnt,t) = insert(v,(nobj,!root))
          in
            SET{nobj=cnt,root=ref t}
          end
    fun add' (s, x) = add(x, s)

	(* Insert a list of items.
	 *)
    fun addList (set,[]) = set
      | addList (set,l) = let
          val arg = case set of EMPTY => (0,SplayNil)
                              | SET{root,nobj} => (nobj,!root)
          val (cnt,t) = List.foldl insert arg l
          in
            SET{nobj=cnt,root=ref t}
          end

    fun fromList l = addList (empty, l)

	(* Remove an item.
         * Raise LibBase.NotFound if not found
	 *)
    fun delete (EMPTY,_) = raise LibBase.NotFound
      | delete (SET{root,nobj},key) =
          case splay (cmpf key, !root) of
            (EQUAL,SplayObj{value,left,right}) =>
              if nobj = 1 then EMPTY
              else SET{root=ref(join(left,right)),nobj=nobj-1}
          | (_,r) => (root := r; raise LibBase.NotFound)

  (* return true if the item is in the set *)
    fun member (EMPTY, key) = false
      | member (SET{root,nobj}, key) = (case splay (cmpf key, !root)
           of (EQUAL, r) => (root := r; true)
            | (_, r) => (root := r; false)
	  (* end case *))

    fun isEmpty EMPTY = true
      | isEmpty _ = false

    fun minItem EMPTY = raise Empty
      | minItem (SET{root, ...}) = let
	  fun min (SplayObj{value, left=SplayNil, ...}) = value
	    | min (SplayObj{left, ...}) = min left
	    | min SplayNil = raise Fail "impossible"
	  in
	    min (!root)
	  end

    fun maxItem EMPTY = raise Empty
      | maxItem (SET{root, ...}) = let
	  fun max (SplayObj{value, right=SplayNil, ...}) = value
	    | max (SplayObj{right, ...}) = max right
	    | max SplayNil = raise Fail "impossible"
	  in
	    max (!root)
	  end

    local
      fun member (x,tree) = let
            fun mbr SplayNil = false
              | mbr (SplayObj{value,left,right}) =
                  case K.compare(x,value) of
                    LESS => mbr left
                  | GREATER => mbr right
                  | _ => true
          in mbr tree end

        (* true if every item in t is in t' *)
      fun treeIn (t,t') = let
            fun isIn SplayNil = true
              | isIn (SplayObj{value,left=SplayNil,right=SplayNil}) =
                  member(value, t')
              | isIn (SplayObj{value,left,right=SplayNil}) =
                  member(value, t') andalso isIn left
              | isIn (SplayObj{value,left=SplayNil,right}) =
                  member(value, t') andalso isIn right
              | isIn (SplayObj{value,left,right}) =
                  member(value, t') andalso isIn left andalso isIn right
            in
              isIn t
            end
    in
    fun equal (SET{root=rt,nobj=n},SET{root=rt',nobj=n'}) =
          (n=n') andalso treeIn (!rt,!rt')
      | equal (EMPTY, EMPTY) = true
      | equal _ = false

    fun isSubset (SET{root=rt,nobj=n},SET{root=rt',nobj=n'}) =
          (n<=n') andalso treeIn (!rt,!rt')
      | isSubset (EMPTY,_) = true
      | isSubset _ = false
    end

    local
      fun next ((t as SplayObj{right, ...})::rest) = (t, left(right, rest))
	| next _ = (SplayNil, [])
      and left (SplayNil, rest) = rest
	| left (t as SplayObj{left=l, ...}, rest) = left(l, t::rest)
    in
    fun compare (EMPTY, EMPTY) = EQUAL
      | compare (EMPTY, _) = LESS
      | compare (_, EMPTY) = GREATER
      | compare (SET{root=s1, ...}, SET{root=s2, ...}) = let
	  fun cmp (t1, t2) = (case (next t1, next t2)
		 of ((SplayNil, _), (SplayNil, _)) => EQUAL
		  | ((SplayNil, _), _) => LESS
		  | (_, (SplayNil, _)) => GREATER
		  | ((SplayObj{value=e1, ...}, r1), (SplayObj{value=e2, ...}, r2)) => (
		      case Key.compare(e1, e2)
		       of EQUAL => cmp (r1, r2)
			| order => order
		      (* end case *))
		(* end case *))
	  in
	    cmp (left(!s1, []), left(!s2, []))
	  end

    fun disjoint (EMPTY, _) = true
      | disjoint (_, EMPTY) = true
      | disjoint (SET{root=rt, ...}, SET{root=rt', ...}) = let
	  fun walk (t1, t2) = (case (next t1, next t2)
		 of ((SplayNil, _), _) => true
		  | (_, (SplayNil, _)) => true
		  | ((SplayObj{value=e1, ...}, r1), (SplayObj{value=e2, ...}, r2)) => (
		      case Key.compare(e1, e2)
		       of LESS => walk(r1, t2)
			| EQUAL => false
			| GREATER => walk(t1, r2)
		      (* end case *))
		(* end case *))
	  in
	    walk (left(!rt, []), left(!rt', []))
	  end
    end (* local *)

	(* Return the number of items in the table *)
    fun numItems EMPTY = 0
      | numItems (SET{nobj,...}) = nobj

    fun toList EMPTY = []
      | toList (SET{root,...}) =
        let fun apply (SplayNil,l) = l
              | apply (SplayObj{value,left,right},l) =
                  apply(left, value::(apply (right,l)))
        in
          apply (!root,[])
        end

    fun split (value,s) =
          case splay(cmpf value, s) of
            (EQUAL,SplayObj{value,left,right}) => (SOME value, left, right)
          | (LESS,SplayObj{value,left,right}) => (NONE, SplayObj{value=value,left=left,right=SplayNil},right)
          | (GREATER,SplayObj{value,left,right}) => (NONE, left, SplayObj{value=value,right=right,left=SplayNil})
          | (_,SplayNil) => (NONE, SplayNil, SplayNil)

    fun intersection (EMPTY,_) = EMPTY
      | intersection (_,EMPTY) = EMPTY
      | intersection (SET{root,...},SET{root=root',...}) =
          let fun inter(SplayNil,_) = (SplayNil,0)
                | inter(_,SplayNil) = (SplayNil,0)
                | inter(s, SplayObj{value,left,right}) =
                    case split(value,s) of
                      (SOME v, l, r) =>
                        let val (l',lcnt) = inter(l,left)
                            val (r',rcnt) = inter(r,right)
                        in
                          (SplayObj{value=v,left=l',right=r'},lcnt+rcnt+1)
                        end
                    | (_,l,r) =>
                        let val (l',lcnt) = inter(l,left)
                            val (r',rcnt) = inter(r,right)
                        in
                          (join(l',r'),lcnt+rcnt)
                        end
          in
            case inter(!root,!root') of
              (_,0) => EMPTY
            | (root,cnt) => SET{root = ref root, nobj = cnt}
          end

    fun count st =
         let fun cnt(SplayNil,n) = n
               | cnt(SplayObj{left,right,...},n) = cnt(left,cnt(right,n+1))
         in
           cnt(st,0)
         end

    fun difference (EMPTY,_) = EMPTY
      | difference (s,EMPTY) = s
      | difference (SET{root,...}, SET{root=root',...}) =
          let fun diff(SplayNil,_) = (SplayNil,0)
                | diff(s,SplayNil) = (s, count s)
                | diff(s,SplayObj{value,right,left}) =
                    let val (_,l,r) = split(value,s)
                        val (l',lcnt) = diff(l,left)
                        val (r',rcnt) = diff(r,right)
                    in
                      (join(l',r'),lcnt+rcnt)
                    end
          in
            case diff(!root,!root')
             of (_,0) => EMPTY
              | (root,cnt) => SET{root = ref root, nobj = cnt}
          end

    fun union (EMPTY,s) = s
      | union (s,EMPTY) = s
      | union (SET{root,...}, SET{root=root',...}) =
          let fun uni(SplayNil,s) = (s,count s)
                | uni(s,SplayNil) = (s, count s)
                | uni(s,SplayObj{value,right,left}) =
                    let val (_,l,r) = split(value,s)
                        val (l',lcnt) = uni(l,left)
                        val (r',rcnt) = uni(r,right)
                    in
                      (SplayObj{value=value,right=r',left=l'},lcnt+rcnt+1)
                    end
              val (root,cnt) = uni(!root,!root')
          in
            SET{root = ref root, nobj = cnt}
          end

    local
      (* functions for walking the tree while keeping a stack of parents
       * to be visited.
       *)
      fun next ((t as SplayObj{right, ...})::rest) = (t, goLeft(right, rest))
        | next _ = (SplayNil, [])
      and goLeft (SplayNil, rest) = rest
        | goLeft (t as SplayObj{left, ...}, rest) = goLeft(left, t::rest)
      fun start EMPTY = []
        | start (SET{root, ...}) = goLeft (!root, [])
    in
    fun combineWith pred (s1, s2) = let
          (* we do a merge on the iteration of the two sets *)
          fun comb (t1, t2, result) = (case (next t1, next t2)
                 of ((SplayNil, _), (SplayNil, _)) => result
                  | ((SplayNil, _), (SplayObj{value, ...}, r2)) =>
                      condAdd (value, false, true, t1, r2, result)
                  | ((SplayObj{value, ...}, r1), (SplayNil, _)) =>
                      condAdd (value, true, false, r1, t2, result)
                  | ((SplayObj{value=x1, ...}, r1), (SplayObj{value=x2, ...}, r2)) => (
                      case K.compare(x1, x2)
                       of LESS => condAdd (x1, true, false, r1, t2, result)
                        | EQUAL => condAdd (x1, true, true, r1, r2, result)
                        | GREATER => condAdd (x2, false, true, t1, r2, result)
                      (* end case *))
                (* end case *))
          and condAdd (x, p1, p2, r1, r2, result) =
                if pred (x, p1, p2)
                  then comb (r1, r2, add (result, x))
                  else comb (r1, r2, result)
          in
            comb (start s1, start s2, EMPTY)
          end
    end (* local *)

    fun subtract (s, item) = difference (s, singleton item)
    fun subtract' (item, s) = subtract (s, item)

    fun subtractList (l, items) = let
	  val items' = List.foldl (fn (x, set) => add(set, x)) EMPTY items
	  in
	    difference (l, items')
	  end

    fun map f EMPTY = EMPTY
      | map f (SET{root, ...}) = let
	  fun mapf (acc, SplayNil) = acc
	    | mapf (acc, SplayObj{value,left,right}) =
		mapf (add (mapf (acc, left), f value), right)
	  in
	    mapf (EMPTY, !root)
	  end

    fun mapPartial f EMPTY = EMPTY
      | mapPartial f (SET{root, ...}) = let
	  fun mapf (acc, SplayNil) = acc
	    | mapf (acc, SplayObj{value,left,right}) = let
		val acc = mapf (acc, left)
		in
		  case f value
		   of SOME value' => mapf (add(acc, value'), right)
		    | NONE => mapf (acc, right)
		  (* end case *)
		end
	  in
	    mapf (EMPTY, !root)
	  end

    fun app af EMPTY = ()
      | app af (SET{root,...}) =
          let fun apply SplayNil = ()
                | apply (SplayObj{value,left,right}) =
                    (apply left; af value; apply right)
          in apply (!root) end
(*
    fun revapp af (SET{root,...}) =
          let fun apply SplayNil = ()
                | apply (SplayObj{value,left,right}) =
                    (apply right; af value; apply left)
          in apply (!root) end
*)
	(* Fold function *)
    fun foldr abf b EMPTY = b
      | foldr abf b (SET{root,...}) =
          let fun apply (SplayNil, b) = b
                | apply (SplayObj{value,left,right},b) =
                    apply(left,abf(value,apply(right,b)))
        in
          apply (!root,b)
        end

    fun foldl abf b EMPTY = b
      | foldl abf b (SET{root,...}) =
          let fun apply (SplayNil, b) = b
                | apply (SplayObj{value,left,right},b) =
                    apply(right,abf(value,apply(left,b)))
        in
          apply (!root,b)
        end

    fun filter p EMPTY = EMPTY
      | filter p (SET{root,...}) = let
          fun filt (SplayNil,tree) = tree
            | filt (SplayObj{value,left,right},tree) = let
                val t' = filt(right,filt(left,tree))
                in
                  if p value then insert(value,t') else t'
                end
          in
            case filt(!root,(0,SplayNil)) of
              (0,_) => EMPTY
            | (cnt,t) => SET{nobj=cnt,root=ref t}
          end

    fun partition p EMPTY = (EMPTY, EMPTY)
      | partition p (SET{root,...}) = let
          fun filt (SplayNil, tree1, tree2) = (tree1, tree2)
            | filt (SplayObj{value,left,right}, tree1, tree2) = let
                val (t1, t2) = filt(left, tree1, tree2)
		val (t1', t2') = filt(right, t1, t2)
                in
                  if p value
		    then (insert(value, t1'), t2')
                    else (t1', insert(value, t2'))
                end
	  fun mk (0, _) = EMPTY
	    | mk (cnt, t) = SET{nobj=cnt, root=ref t}
	  val (t1, t2) = filt (!root, (0, SplayNil), (0, SplayNil))
          in
	    (mk t1, mk t2)
          end

    fun exists p EMPTY = false
      | exists p (SET{root,...}) = let
          fun ex SplayNil = false
            | ex (SplayObj{value=v, left=l, right=r}) = p v orelse ex l orelse ex r
          in
            ex (!root)
          end

    fun all p EMPTY = true
      | all p (SET{root,...}) = let
          fun all' SplayNil = true
            | all' (SplayObj{value=v, left=l, right=r}) = p v andalso all' l andalso all' r
          in
            all' (!root)
          end

    fun find p EMPTY = NONE
      | find p (SET{root,...}) = let
          fun ex SplayNil = NONE
            | ex (SplayObj{value=v,left=l,right=r}) =
                if p v then SOME v
                else (case ex l
		    of NONE => ex r
		     | a => a
		  (* end case *))
          in
            ex (!root)
          end

  (* deprecated *)
    val listItems = toList

  end (* SplaySet *)
