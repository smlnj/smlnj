(* rb-set.sml
 *
 * Test that the invariants are being preserved in the red-black set implementation.
 *)

(*
use "int-redblack-set.sml";
*)

structure ChkSet (*:> ORD_SET where type Key.ord_key = int*) =
  struct
    local
      datatype set = datatype IntRedBlackSet.set
      datatype color = datatype IntRedBlackSet.color
      datatype tree = datatype IntRedBlackSet.tree
      fun dump (SET(n, tr)) = let
	      fun indent 0 = ()
		| indent n = (print "  "; indent(n-1))
	      fun prl (n, l) = (
		    indent n;
		    print(concat l);
		    print "\n")
	      fun c2s R = "R"
		| c2s B = "B"
	      fun pr (n, T(color, l, v, r)) = (
		    prl (n, [c2s color, ": ", Int.toString v]);
		    case (l, r)
		     of (E, E) => ()
		      | _ => (pr (n+1, l); pr (n+1, r))
		    (* end case *))
		| pr (n, E) = prl (n, ["E"])
	      in
		prl(0, ["SET; ", Int.toString n, " items:"]);
		pr (1, tr)
	      end

	fun check name (set as SET(n, tr)) = let
	      fun error msg = (
		    print(concat["***** Error in ", name, ": ", msg, "\n"]);
		    dump set;
		    raise Fail msg)
	      val SOME minInt = Int.minInt
	      fun cmp (a, b) = (a < b) orelse ((a = b) andalso (a = minInt))
	      fun chkOrder (prev, E) = prev
		| chkOrder (prev, T(_, E, x, r)) =
		    if cmp(prev, x) then chkOrder(x, r) else error "Order invariant failure"
		| chkOrder (prev, T(_, l, x, r)) =
		    if cmp(chkOrder(prev, l), x)
		      then chkOrder(x, r)
		      else error "Order invariant failure"
	      fun chk (parentColor, T(color, l, _, r)) = let
		    val (n1, bd1) = chk(color, l)
		    val (n2, bd2) = chk(color, r)
		    val n = n1+n2+1
		    in
		      if (bd1 <> bd2)
			then error "Black invariant failure"
			else ();
		      case (parentColor, color)
		       of (R, R) => error "Red invariant failure"
			| (_, B) => (n, bd1+1)
			| _ => (n, bd1)
		      (* end case *)
		    end
		| chk (_, E) = (0, 1)
	      val _ = chkOrder (minInt, tr)
	      val (n', _) = chk (B, tr)
	      in
		if (n <> n')
		  then error "Incorrect item count"
		  else set
	      end

    in
    structure Key = IntRedBlackSet.Key

    type item = Key.ord_key
    type set = IntRedBlackSet.set

val dump = dump

    val empty = check "empty" IntRedBlackSet.empty

    val singleton = (check "singleton") o IntRedBlackSet.singleton

    val fromList = (check "fromList") o IntRedBlackSet.fromList
    val add = (check "add") o IntRedBlackSet.add
    val add' = (check "add'") o IntRedBlackSet.add'
    val addList = (check "addList") o IntRedBlackSet.addList
    val subtract = (check "subtract") o IntRedBlackSet.subtract
    val subtract' = (check "subtract'") o IntRedBlackSet.subtract'
    val subtractList = (check "subtractList") o IntRedBlackSet.subtractList
    val delete = (check "delete") o IntRedBlackSet.delete

    val member = IntRedBlackSet.member
    val isEmpty = IntRedBlackSet.isEmpty
    val equal = IntRedBlackSet.equal
    val compare = IntRedBlackSet.compare
    val isSubset = IntRedBlackSet.isSubset
    val numItems = IntRedBlackSet.numItems
    val listItems = IntRedBlackSet.listItems

    val union = (check "union") o IntRedBlackSet.union
    val intersection = (check "intersection") o IntRedBlackSet.intersection
    val difference = (check "difference") o IntRedBlackSet.difference

    fun map f = (check "map") o (IntRedBlackSet.map f)
    val app = IntRedBlackSet.app
    val foldl = IntRedBlackSet.foldl
    val foldr = IntRedBlackSet.foldr

    fun partition pred s = let
	  val (s1, s2) = IntRedBlackSet.partition pred s
	  in
	    check "partition(true)" s1;
	    check "partition(false)" s2;
	    (s1, s2)
	  end

    fun filter pred = (check "filter") o (IntRedBlackSet.filter pred)

    val exists = IntRedBlackSet.exists
    val all = IntRedBlackSet.all
    val find = IntRedBlackSet.find

    end (* local *)
  end;

local
  open IntRedBlackSet
in
val _ = print "***** Regression tests *****\n"
val s1 = SET(7,
	  T(B,
	    T(R,
	      T(B, T(R, E, ~801, E), ~708, E),
	      ~595,
	      T(B, E, ~372, E)),
	    ~38,
	    T(B, T(R, E, 91, E), 578, E)))
val _ = ChkSet.dump s1
val _ = ChkSet.delete(s1, ~595)
val s2 = SET(5,
	  T(B,
	    T(R,
	      T(B, E, ~103, E),
	      ~98,
	      T(B, E, ~29, E)),
	    25,
	    T(B, E, 75, E)))
val _ = ChkSet.dump s2
val _ = ChkSet.delete(s2, ~98)
val s3 = SET(6,
	  T(B,
	    T(R,
	      T(B, E, ~1032, E),
	      ~584,
	      T(B,
		T(R, E, ~407, E),
		~305,
		E)),
	    ~22,
	    T(B, E, 74, E)))
val _ = ChkSet.dump s3
val _ = ChkSet.delete(s3, ~22)
(* an example from Achim D. Brucker and Burkhart Wolff that was known to produce an invalid tree *)
val s4 = let
      val s = add(empty, 5)
      val s = add(s, 6)
      val s = add(s, 8)
      val s = add(s, 2)
      in
        delete(s, 8)
      end
end;

local
  open ChkSet
  val randSrc = Random.rand(17, 42)
  datatype oper = ADD of int | DEL of int
  exception ERROR of oper list
  fun prTrace [] = print "empty"
    | prTrace (ADD n :: tr) = (
	print "add("; prTrace tr; print(concat[", ", Int.toString n, ")"]))
    | prTrace (DEL n :: tr) = (
	print "delete("; prTrace tr; print(concat[", ", Int.toString n, ")"]))
  fun addOrDelete limit (s, trace) = let
	val pct = real(numItems s) / real limit
	in
	  if (Random.randReal randSrc < pct)
	    then let
	    (* pick an item at random to delete *)
	      val item = List.nth(listItems s, Random.randRange (0, numItems s - 1) randSrc)
	      val trace = DEL item :: trace
	      in
		(delete (s, item), trace) handle _ => raise ERROR trace
	      end
	    else let
	      val item = Random.randInt randSrc
	      val trace = ADD item :: trace
	      in
		(add (s, item), trace) handle _ => raise ERROR trace
	      end
	end
  fun repeat f n = let
	fun lp (0, s : set, _) = s
	  | lp (i, s, trace) = let
	      val (s, trace) = f (s, trace)
		    handle ex as (ERROR trace) => (
		      case hd trace
		       of ADD n => print(concat["Failed on ADD ", Int.toString n, "\n"])
			| DEL n => print(concat["Failed on DEL ", Int.toString n, "\n"])
		      (* end case *);
		      print "Set prior to failure:\n"; ChkSet.dump s; raise ex)
	      in
		lp (i-1, s, trace)
	      end
	in
	  lp (n, empty, [])
	end
in
(* building sets from sorted lists *)
val _ = print "***** Sorted insertion tests *****\n"
val s01_1 = fromList(List.tabulate(5, fn i => i))
val s01_2 = fromList(List.tabulate(10, fn i => i))
val s01_3 = fromList(List.tabulate(50, fn i => i))
val s01_4 = fromList(List.tabulate(100, fn i => i))
(* tear down sets in order *)
val s02_1 = List.foldl (fn (i, s) => delete(s, i)) s01_1 (List.tabulate(5, fn i => i))
val s02_2 = List.foldl (fn (i, s) => delete(s, i)) s01_2 (List.tabulate(10, fn i => i))
val s02_3 = List.foldl (fn (i, s) => delete(s, i)) s01_3 (List.tabulate(50, fn i => i))
val s02_4 = List.foldl (fn (i, s) => delete(s, i)) s01_4 (List.tabulate(100, fn i => i))
val _ = print "***** Sorted insertion tests *****\n"
val s03_1 = fromList(List.tabulate(5, fn i => i))
val s03_2 = fromList(List.tabulate(10, fn i => i))
val s03_3 = fromList(List.tabulate(50, fn i => i))
val s03_4 = fromList(List.tabulate(100, fn i => i))
(* building sets from random lists *)
val _ = print "***** Random insertion tests *****\n"
val s04_1 = fromList(List.tabulate(5, fn _ => Random.randInt randSrc))
val s04_2 = fromList(List.tabulate(10, fn _ => Random.randInt randSrc))
val s04_3 = fromList(List.tabulate(50, fn _ => Random.randInt randSrc))
val s04_4 = fromList(List.tabulate(100, fn _ => Random.randInt randSrc))
(* sequences of random add/subtract operations *)
val _ = print "***** Random operation tests *****\n"
val s05_1 = repeat (addOrDelete 5) 5000
val s05_2 = repeat (addOrDelete 10) 10000
val s05_3 = repeat (addOrDelete 50) 50000
val s05_4 = repeat (addOrDelete 100) 100000
val s05_5 = repeat (addOrDelete 500) 500000
val _ = print "***** Union tests *****\n"
val s06_11 = union(s05_1, s05_1)
val s06_12 = union(s05_1, s05_2)
val s06_13 = union(s05_1, s05_3)
val s06_14 = union(s05_1, s05_4)
val s06_15 = union(s05_1, s05_5)
val s06_21 = union(s05_2, s05_1)
val s06_22 = union(s05_2, s05_2)
val s06_23 = union(s05_2, s05_3)
val s06_24 = union(s05_2, s05_4)
val s06_25 = union(s05_2, s05_5)
val s06_31 = union(s05_3, s05_1)
val s06_32 = union(s05_3, s05_2)
val s06_33 = union(s05_3, s05_3)
val s06_34 = union(s05_3, s05_4)
val s06_35 = union(s05_3, s05_5)
val s06_41 = union(s05_4, s05_1)
val s06_42 = union(s05_4, s05_2)
val s06_43 = union(s05_4, s05_3)
val s06_44 = union(s05_4, s05_4)
val s06_45 = union(s05_4, s05_5)
val s06_51 = union(s05_5, s05_1)
val s06_52 = union(s05_5, s05_2)
val s06_53 = union(s05_5, s05_3)
val s06_54 = union(s05_5, s05_4)
val s06_55 = union(s05_5, s05_5)
val _ = print "***** Intersection tests *****\n"
val s07_11 = intersection(s05_1, s05_1)
val s07_12 = intersection(s05_1, s05_2)
val s07_13 = intersection(s05_1, s05_3)
val s07_14 = intersection(s05_1, s05_4)
val s07_15 = intersection(s05_1, s05_5)
val s07_21 = intersection(s05_2, s05_1)
val s07_22 = intersection(s05_2, s05_2)
val s07_23 = intersection(s05_2, s05_3)
val s07_24 = intersection(s05_2, s05_4)
val s07_25 = intersection(s05_2, s05_5)
val s07_31 = intersection(s05_3, s05_1)
val s07_32 = intersection(s05_3, s05_2)
val s07_33 = intersection(s05_3, s05_3)
val s07_34 = intersection(s05_3, s05_4)
val s07_35 = intersection(s05_3, s05_5)
val s07_41 = intersection(s05_4, s05_1)
val s07_42 = intersection(s05_4, s05_2)
val s07_43 = intersection(s05_4, s05_3)
val s07_44 = intersection(s05_4, s05_4)
val s07_45 = intersection(s05_4, s05_5)
val s07_51 = intersection(s05_5, s05_1)
val s07_52 = intersection(s05_5, s05_2)
val s07_53 = intersection(s05_5, s05_3)
val s07_54 = intersection(s05_5, s05_4)
val s07_55 = intersection(s05_5, s05_5)
val _ = print "***** Difference tests *****\n"
val s08_11 = difference(s05_1, s05_1)
val s08_12 = difference(s05_1, s05_2)
val s08_13 = difference(s05_1, s05_3)
val s08_14 = difference(s05_1, s05_4)
val s08_15 = difference(s05_1, s05_5)
val s08_21 = difference(s05_2, s05_1)
val s08_22 = difference(s05_2, s05_2)
val s08_23 = difference(s05_2, s05_3)
val s08_24 = difference(s05_2, s05_4)
val s08_25 = difference(s05_2, s05_5)
val s08_31 = difference(s05_3, s05_1)
val s08_32 = difference(s05_3, s05_2)
val s08_33 = difference(s05_3, s05_3)
val s08_34 = difference(s05_3, s05_4)
val s08_35 = difference(s05_3, s05_5)
val s08_41 = difference(s05_4, s05_1)
val s08_42 = difference(s05_4, s05_2)
val s08_43 = difference(s05_4, s05_3)
val s08_44 = difference(s05_4, s05_4)
val s08_45 = difference(s05_4, s05_5)
val s08_51 = difference(s05_5, s05_1)
val s08_52 = difference(s05_5, s05_2)
val s08_53 = difference(s05_5, s05_3)
val s08_54 = difference(s05_5, s05_4)
val s08_55 = difference(s05_5, s05_5)
end (* end local *)
