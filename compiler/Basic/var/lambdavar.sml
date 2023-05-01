(* lambdavar.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure LambdaVar :> LAMBDA_VAR =
  struct

    structure S = Symbol
    structure Map = IntRedBlackMap
    structure Set = IntRedBlackSet
    structure Tbl = IntHashTable

  (* if true, we remember the names of lambda vars *)
    val saveLvarNames = ElabDataControl.saveLvarNames

    type lvar = int      (* lambda variable id number *)

    fun inc r = r := !r + 1
    fun newLvar r () = (inc r; !r)
    val varcount = ref 0

    val mkLvar = newLvar varcount
    fun nextLvar () = !varcount
    fun diff (lv1, lv2) = lv2 - lv1

    exception NoLvarName

    val lvarNames : string Tbl.hash_table = Tbl.mkTable(64, NoLvarName)

    val findName = Tbl.find lvarNames

    val giveLvarName = Tbl.insert lvarNames

    val lvarIsNamed = Tbl.inDomain lvarNames

    fun toString (lvar:lvar) = Int.toString lvar
    val prLvar = toString (* temporary, for backward compatibility *)

    fun sameName (v, w) = if !saveLvarNames
	  then (case findName w
	     of SOME x => giveLvarName(v, x)
	      | NONE => (case findName v
		   of SOME x => giveLvarName (w, x)
		    | NONE => ()
		  (* end case *))
	    (* end case *))
	  else ()

    fun clear () = (varcount := 0; Tbl.clear lvarNames)

    fun dupLvar v = let
          val nv = mkLvar()
	  in
	    if !saveLvarNames
	      then (case findName v
		 of SOME x => giveLvarName(nv, x)
		  | NONE => ()
		(* end cadse *))
	      else ();
	    nv
	  end

    fun namedLvar (id: S.symbol) = let
	  val nv = mkLvar()
          in
	    if !saveLvarNames then giveLvarName(nv, S.name id) else ();
	    nv
          end

    fun lvarSym (lv : lvar) : S.symbol option = (case findName lv
	   of SOME x => SOME(S.varSymbol x)
	    | NONE => NONE
	  (* end case *))

    fun lvarName (lv : lvar) : string = (case findName lv
	   of SOME x => x ^ Int.toString lv
	    | NONE => "v" ^ Int.toString lv
	  (* end case *))

    val toId = Fn.id
    val fromId = Fn.id

  (* sorted lists of LVars *)
    structure SortedList =
      struct

	type t = lvar list

	fun enter (new : lvar, l) = let
	      fun f [] = [new]
		| f (l as h::t) = if new<h then new::l else if new>h then h::f t else l
	      in
		f l
	      end

	fun merge (a, []) = a
	  | merge ([], a) = a
	  | merge (l as (i : lvar)::a, m as j::b) = if j<i
	      then j::merge(l,b)
	      else i::merge(a,if i<j then m else b)

	local
	(* merge pairs of lists until we have a singleton *)
	  fun loop (a::b::rest) = loop(merge(a,b)::loop rest)
	    | loop l = l
	in
	fun foldmerge (l : t list) = (case loop l of [] => [] | xs::_ => xs)
	end

	fun uniq (l : lvar list) = let
	      fun split ([], l, r) = (l,r)
		| split (h::t,l,r) = split(t,r,h::l)
	      fun sort [] = []
		| sort (l as [_]) = l
		| sort (l as [x : lvar,y : lvar]) =
		   if x = y then [x] else if x < y then l else [y,x]
		| sort l = let
		    val (l,r) = split(l,[],[])
		    in
		      merge(sort l, sort r)
		    end
	    in
	      sort l
	    end

	fun remove (x as (xl : lvar)::xr, y as yl::yr) = if xl>yl
	      then yl::remove(x, yr)
	      else remove(xr, if xl<yl then y else yr)
	  | remove (_, y) = y

	fun rmv (x : int, l) = let
	      fun loop nil = nil
		| loop (a::b) = if x=a then b else a::loop b
	      in
		loop l
	      end

	fun member l (e : lvar) = let
	      fun f [] = false
		| f (h::t) = if h<e then f t else e=h
	      in
		f l
	      end

	fun intersect (nil, _) = nil
	  | intersect (_, nil) = nil
	  | intersect (l as (a : lvar)::b, r as c::d) =
	      if a=c then a::intersect(b,d)
	      else if a<c then intersect(b,r)
	      else intersect(l,d)

	fun difference (nil, _) = nil
	  | difference (l, nil) = l
	  | difference (l as (a : lvar)::b, r as c::d) =
	      if a=c then difference(b,d)
	      else if a<c then a::difference(b,r)
	      else difference(l,d)

      end (* SortedList *)

  (* comparisons of lvars *)
    val same : lvar * lvar -> bool = op =
    val op < : lvar * lvar -> bool = op <
    val op > : lvar * lvar -> bool = op >
    val compare : lvar * lvar -> order = Int.compare

  end (* structure LambdaVar *)
