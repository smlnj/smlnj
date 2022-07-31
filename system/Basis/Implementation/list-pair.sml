(* list-pair.sml
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * If lists are of unequal length, the excess elements from the
 * tail of the longer one are ignored. No exception is raised.
 *
 *)

structure ListPair : LIST_PAIR =
  struct

    exception UnequalLengths

  (* for inlining *)
    fun rev l = let
          fun loop ([], acc) = acc
            | loop (a::r, acc) = loop(r, a::acc)
          in
	    loop (l, [])
	  end

    fun zip (l1, l2) = let
	  fun zip' ((a :: r1), (b :: r2), l) = zip' (r1, r2, (a, b)::l)
	    | zip' (_, _, l) = rev l
	  in
	    zip' (l1, l2, [])
	  end

    fun zipEq (l1, l2) = let
	  fun zip' ((a :: r1), (b :: r2), l) = zip' (r1, r2, (a, b)::l)
	    | zip' ([], [], l) = rev l
	    | zip' _ = raise UnequalLengths
	  in
	    zip' (l1, l2, [])
	  end

    fun unzip l = let
	  fun unzip' ([], l1, l2) = (l1, l2)
	    | unzip' ((a, b) :: r, l1, l2) = unzip' (r, a::l1, b::l2)
	  in
	    unzip' (rev l, [], [])
	  end

    fun map f = let
	  fun mapf (a::r1, b::r2, l) = mapf (r1, r2, f(a, b) :: l)
	    | mapf (_, _, l) = rev l
	  in
	    fn (l1, l2) => mapf (l1, l2, [])
	  end

    fun mapEq f = let
	  fun mapf (a::r1, b::r2, l) = mapf (r1, r2, f(a, b) :: l)
	    | mapf ([], [], l) = rev l
	    | mapf _ = raise UnequalLengths
	  in
	    fn (l1, l2) => mapf (l1, l2, [])
	  end

    fun app f = let
	  fun appf (a::r1, b::r2) = (f(a, b); appf(r1, r2))
	    | appf _ = ()
	  in
	    appf
	  end

    fun appEq f = let
	  fun appf (a::r1, b::r2) = (f(a, b); appf(r1, r2))
	    | appf ([], []) = ()
	    | appf _ = raise UnequalLengths
	  in
	    appf
	  end

    fun all pred = let
	  fun allp (a::r1, b::r2) = pred(a, b) andalso allp (r1, r2)
	    | allp _ = true
	  in
	    allp
	  end

    fun allEq pred = let
	  fun allp (a::r1, b::r2) = pred(a, b) andalso allp (r1, r2)
	    | allp ([], []) = true
	    | allp _ = false
	  in
	    allp
	  end

    fun foldl f init (l1, l2) = let
	  fun foldf (x::xs, y::ys, accum) = foldf(xs, ys, f(x, y, accum))
	    | foldf (_, _, accum) = accum
	  in
	    foldf (l1, l2, init)
	  end

    fun foldlEq f init (l1, l2) = let
	  fun foldf (x::xs, y::ys, accum) = foldf(xs, ys, f(x, y, accum))
	    | foldf ([], [], accum) = accum
	    | foldf _ = raise UnequalLengths
	  in
	    foldf (l1, l2, init)
	  end

    fun foldr f init (l1, l2) = let
	  fun foldf (x::xs, y::ys) = f(x, y, foldf(xs, ys))
	    | foldf _ = init
	  in
	    foldf (l1, l2)
	  end

    fun foldrEq f init (l1, l2) = let
	  fun foldf (x::xs, y::ys) = f(x, y, foldf(xs, ys))
	    | foldf ([], []) = init
	    | foldf _ = raise UnequalLengths
	  in
	    foldf (l1, l2)
	  end

    fun exists pred = let
	  fun existsp (a::r1, b::r2) = pred(a, b) orelse existsp (r1, r2)
	    | existsp _ = false
	  in
	    existsp
	  end

  (* added for Basis Library proposal 2015-003 *)

    fun appi f (l1, l2) = let
	  fun appf (i, x::xs, y::ys) = (f(i, x, y); appf(i+1, xs, ys))
	    | appf _ = ()
	  in
	    appf (0, l1, l2)
	  end

    fun appiEq f (l1, l2) = let
	  fun appf (i, x::xs, y::ys) = (f(i, x, y); appf(i+1, xs, ys))
	    | appf (_, [], []) = ()
	    | appf _ = raise UnequalLengths
	  in
	    appf (0, l1, l2)
	  end

    fun mapi f (l1, l2) = let
	  fun mapf (i, x::xs, y::ys, zs) = (mapf(i+1, xs, ys, f(i, x, y) :: zs))
	    | mapf (_, _, _, zs) = List.rev zs
	  in
	    mapf (0, l1, l2, [])
	  end

    fun mapiEq f (l1, l2) = let
	  fun mapf (i, x::xs, y::ys, zs) = (mapf(i+1, xs, ys, f(i, x, y) :: zs))
	    | mapf (_, [], [], zs) = List.rev zs
	    | mapf _ = raise UnequalLengths
	  in
	    mapf (0, l1, l2, [])
	  end

    fun mapPartial f (l1, l2) = let
	  fun mapf (x::xs, y::ys, acc) = (case f(x, y)
		 of SOME z => mapf(xs, ys, z::acc)
		  | NONE => mapf(xs, ys, acc)
		(* end case *))
	    | mapf (_, _, acc) = List.rev acc
	  in
	    mapf (l1, l2, [])
	  end

    fun mapPartialEq f (l1, l2) = let
	  fun mapf (x::xs, y::ys, acc) = (case f(x, y)
		 of SOME z => mapf(xs, ys, z::acc)
		  | NONE => mapf(xs, ys, acc)
		(* end case *))
	    | mapf ([], [], acc) = List.rev acc
	    | mapf _ = raise UnequalLengths
	  in
	    mapf (l1, l2, [])
	  end

    fun mapPartiali f (l1, l2) = let
	  fun mapf (i, x::xs, y::ys, acc) = (case f(i, x, y)
		 of SOME z => mapf(i+1, xs, ys, z::acc)
		  | NONE => mapf(i+1, xs, ys, acc)
		(* end case *))
	    | mapf (_, _, _, acc) = List.rev acc
	  in
	    mapf (0, l1, l2, [])
	  end

    fun mapPartialiEq f (l1, l2) = let
	  fun mapf (i, x::xs, y::ys, acc) = (case f(i, x, y)
		 of SOME z => mapf(i+1, xs, ys, z::acc)
		  | NONE => mapf(i+1, xs, ys, acc)
		(* end case *))
	    | mapf (_, [], [], acc) = List.rev acc
	    | mapf _ = raise UnequalLengths
	  in
	    mapf (0, l1, l2, [])
	  end

    fun foldli f init (l1, l2) = let
	  fun foldf (i, x::xs, y::ys, acc) = foldf (i+1, xs, ys, f(i, x, y, acc))
	    | foldf (_, _, _, acc) = acc
	  in
	    foldf (0, l1, l2, init)
	  end

    fun foldliEq f init (l1, l2) = let
	  fun foldf (i, x::xs, y::ys, acc) = foldf (i+1, xs, ys, f(i, x, y, acc))
	    | foldf (_, [], [], acc) = acc
	    | foldf _ = raise UnequalLengths
	  in
	    foldf (0, l1, l2, init)
	  end

    fun foldri f init (l1, l2) = let
          fun lp (i, x::xs, y::ys) = f (i, x, y, lp (i+1, xs, ys))
	    | lp (_, _, _) = init
	  in
	    lp (0, l1, l2)
	  end

    fun foldriEq f init (l1, l2) = let
          fun lp (i, x::xs, y::ys) = f (i, x, y, lp (i+1, xs, ys))
	    | lp (_, [], []) = init
	    | lp _ = raise UnequalLengths
	  in
	    lp (0, l1, l2)
	  end

    fun unzipMap f l = let
	  fun mapf ([], l1, l2) = (List.rev l1, List.rev l2)
	    | mapf (x::xs, l1, l2) = let
		val (y1, y2) = f x
		in
		  mapf (xs, y1::l1, y2::l2)
		end
	  in
	    mapf (l, [], [])
	  end

    fun unzipMapi f l = let
	  fun mapf ([], _, l1, l2) = (List.rev l1, List.rev l2)
	    | mapf (x::xs, i, l1, l2) = let
		val (y1, y2) = f (i, x)
		in
		  mapf (xs, i+1, y1::l1, y2::l2)
		end
	  in
	    mapf (l, 0, [], [])
	  end

    fun find pred = let
	  fun findp ([], _) = NONE
	    | findp (_, []) = NONE
	    | findp (x::xs, y::ys) = if pred(x, y) then SOME(x, y) else findp(xs, ys)
	  in
	    findp
	  end

    fun findi pred (l1, l2) = let
	  fun findp (_, [], _) = NONE
	    | findp (_, _, []) = NONE
	    | findp (i, x::xs, y::ys) = if pred(i, x, y)
		then SOME(i, x, y)
		else findp(i+1, xs, ys)
	  in
	    findp (0, l1, l2)
	  end

  end (* structure ListPair *)

