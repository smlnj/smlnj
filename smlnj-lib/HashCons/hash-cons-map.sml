(* hash-cons-map.sml
 *
 * COPYRIGHT (c) 2011 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This is an implementation of the HASH_CONS_MAP signature that is built
 * on top of the WordRedBlackMap structure.  Eventually, it will be replaced
 * by an implmementation that uses Patricia trees.
 *)

structure HashConsMap : HASH_CONS_MAP =
  struct

    structure HC = HashCons
    structure Map = WordRedBlackMap

    type 'a obj = 'a HC.obj

    type ('a, 'b) map = ('a obj * 'b) Map.map

    fun lift2 f ((_, a), (_, b)) = f(a, b)
    fun lift2i f (_, (k, a), (_, b)) = f(k, a, b)

    val empty = Map.empty
    fun singleton (obj, v) = Map.singleton (HC.tag obj, (obj, v))
    fun insert (m, obj, v) = Map.insert(m, HC.tag obj, (obj, v))
    fun insert' (p as (obj, v), m) = Map.insert(m, HC.tag obj, p)

    fun insertWith merge (m, obj, v) = let
	  val tag = HC.tag obj
	  in
	    case Map.find(m, tag)
	     of NONE => Map.insert(m, tag, (obj, v))
	      | SOME(_, v') => Map.insert(m, tag, (obj, merge(v', v)))
	    (* end case *)
	  end
    fun insertWithi merge (m, obj, v) = let
	  val tag = HC.tag obj
	  in
	    case Map.find(m, tag)
	     of NONE => Map.insert(m, tag, (obj, v))
	      | SOME(_, v') => Map.insert(m, tag, (obj, merge(obj, v', v)))
	    (* end case *)
	  end

    fun find (map : ('a, 'b) map, obj) = Option.map #2 (Map.find(map, HC.tag obj))
    fun lookup (map : ('a, 'b) map, obj) = #2 (Map.lookup(map, HC.tag obj))
    fun inDomain (map, obj) = Map.inDomain (map, HC.tag obj)
    fun remove (map, obj) = let
	  val (map, (_, v)) = Map.remove (map, HC.tag obj)
	  in
	    (map, v)
	  end
    val isEmpty = Map.isEmpty
    val numItems = Map.numItems
    fun listItems map = Map.foldr (fn ((_, v), vs) => v::vs) [] map
    val listItemsi = Map.listItems
    fun listKeys map = Map.foldr (fn ((k, _), ks) => k::ks) [] map
    fun collate cmp = Map.collate (lift2 cmp)
    fun unionWith merge =
	  Map.unionWith (fn ((k, a), (_, b)) => (k, merge(a, b)))
    fun unionWithi merge =
	  Map.unionWithi (lift2i (fn (k, a, b) => (k, merge(k, a, b))))
    fun intersectWith join =
	  Map.intersectWith (fn ((k, a), (_, b)) => (k, join(a, b)))
    fun intersectWithi join =
	  Map.intersectWithi (lift2i (fn (k, a, b) => (k, join(k, a, b))))
    fun mergeWith join = let
	  fun result (k, SOME c) = SOME(k, c)
	    | result (_, NONE) = NONE
	  fun join' (SOME(k, a), SOME(_, b)) = result (k, join(SOME a, SOME b))
	    | join' (SOME(k, a), NONE) = result (k, join(SOME a, NONE))
	    | join' (NONE, SOME(k, b)) = result (k, join(NONE, SOME b))
	    | join' (NONE, NONE) = raise Fail "impossible"
	  in
	    Map.mergeWith join'
	  end
    fun mergeWithi join = let
	  fun result (k, SOME c) = SOME(k, c)
	    | result (_, NONE) = NONE
	  fun join' (SOME(k, a), SOME(_, b)) = result (k, join(k, SOME a, SOME b))
	    | join' (SOME(k, a), NONE) = result (k, join(k, SOME a, NONE))
	    | join' (NONE, SOME(k, b)) = result (k, join(k, NONE, SOME b))
	    | join' (NONE, NONE) = raise Fail "impossible"
	  in
	    Map.mergeWith join'
	  end
    fun app f = Map.app (fn (_, v) => f v)
    val appi = Map.app
    fun map f = Map.map (fn (k, v) => (k, f v))
    fun mapi f = Map.map (fn (k, v) => (k, f(k, v)))
    fun fold f = Map.foldl (fn ((_, x), acc) => f(x, acc))
    fun foldi f = Map.foldl (fn ((k, x), acc) => f(k, x, acc))
    val foldl = fold	(* DEPRECATED *)
    val foldli = foldi	(* DEPRECATED *)
    val foldr = fold	(* DEPRECATED *)
    val foldri = foldi	(* DEPRECATED *)
    fun filter pred = Map.filter (fn (_, x) => pred x)
    val filteri = Map.filter
    fun mapPartial f =
	  Map.mapPartial
	    (fn (k, v) => case f v of SOME v => SOME(k, v) | NONE => NONE)
    fun mapPartiali f =
	  Map.mapPartial
	    (fn (k, v) => case f(k, v) of SOME v => SOME(k, v) | NONE => NONE)
    fun exists f = Map.exists (fn (k, v) => f v)
    fun existsi f = Map.exists f
    fun all f = Map.all (fn (k, v) => f v)
    fun alli f = Map.all f

  end
