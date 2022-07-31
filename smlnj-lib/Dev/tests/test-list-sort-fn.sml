(* test-list-sort-fn.sml
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

functor TestListSort (S : LIST_SORT) : sig

  (* tests for increasing/equal/descreasing lists *)
    val sortInc : int -> (int * int) list
    val sortEq : int -> (int * int) list
    val sortDec : int -> (int * int) list

    val run : {len : int, ntrials : int} -> unit

  end = struct

    structure R = Random

    fun mkList f n = List.tabulate (n, fn i => (f i, i))

    fun sort l = S.sort (fn ((a, _), (b, _)) => Int.>(a, b)) l

    fun sortInc n = sort (mkList Fn.id n)
    fun sortEq n = sort (mkList (fn i => 42) n)
    fun sortDec n = sort (mkList (fn i => (n - i)) n)

  (* checks that the result is valid (including stability *)
    fun valid ([], 0) = true
      | valid ((x, i)::xr, n) = let
	  fun chk (_, _, [], 0) = true
	    | chk (_, _, [], _) = false
	    | chk (x, i, (y, j)::r, k) = (case Int.compare(x, y)
		 of LESS => chk (y, j, r, k-1)
		  | EQUAL => (i < j) andalso chk (y, j, r, k-1)
		  | GREATER => false
		(* end case *))
	  in
	    chk (x, i, xr, n-1)
	  end

    fun run {len, ntrials} = let
	  val t = Time.toMilliseconds (Time.now())
	  val r = R.rand(
		Int.fromLarge((t div 972) mod 0x3fffffff),
		Int.fromLarge(t mod 0x3fffffff))
	  val ub = Int.max(2*len+1, 101)
	  fun doTrial 0 = ()
	    | doTrial n = let
		val l = mkList (fn _ => (R.randNat r) mod ub) len
		val l' = S.sort (fn ((a, _), (b, _)) => Int.>(a, b)) l
		in
		  if valid (l', len)
		    then doTrial (n-1)
		    else let
		      fun prItem (x, i) = String.concat[
			      "(", Int.toString x, ",", Int.toString i, ")"
			    ]
		      in
		        print (concat[
			    "### Trial ", Int.toString(ntrials - n + 1), " fails\n"
			    ]);
		        print "## Input:  [";
			print (String.concatWithMap "," prItem l);
			print "]\n";
			print "## Result: [";
			print (String.concatWithMap "," prItem l');
			print "]\n"
		      end
		end
	  in
	    doTrial ntrials
	  end

  end
