(* pack-real64-little.sml
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * An implementation of the PACK_REAL API for Real64 that swaps the byte order
 * (i.e., implements PackReal64Little on big-endian machines).
 *)

structure PackReal64Swap : PACK_REAL =
  struct

    structure BV = InlineT.Word8Vector
    structure BA = InlineT.Word8Array
    structure RA = InlineT.Real64Array

    (* fast add avoiding the overflow test *)
    infix ++
    fun x ++ y = InlineT.Int.fast_add(x, y)

    type real = Real64Imp.real

    val bytesPerElem : int = 8

    val isBigEndian : bool = true  (* place holder *)

    val createW8Vec : int -> BV.vector = InlineT.cast Assembly.A.create_s

  (* convert a real to a byte vector *)
    fun toBytes (x : real) = let
	  val ra = Assembly.A.create_r 1
	  val ba : BA.array = InlineT.cast ra
	  val bv = createW8Vec bytesPerElem
	  in
	    RA.update (ra, 0, x);
	    BV.update (bv, 7, BA.sub(ba, 0));
	    BV.update (bv, 6, BA.sub(ba, 1));
	    BV.update (bv, 5, BA.sub(ba, 2));
	    BV.update (bv, 4, BA.sub(ba, 3));
	    BV.update (bv, 3, BA.sub(ba, 4));
	    BV.update (bv, 2, BA.sub(ba, 5));
	    BV.update (bv, 1, BA.sub(ba, 6));
	    BV.update (bv, 0, BA.sub(ba, 7));
	    bv
	  end

  (* convert a byte vector, whose length should be >= bytePerElem, to a real *)
    fun fromBytes bv = if BV.length bv < bytesPerElem
	  then raise General.Subscript
	  else let
	    val ra = Assembly.A.create_r 1
	    val ba : BA.array = InlineT.cast ra
	    in
	      BA.update (ba, 7, BV.sub(bv, 0));
	      BA.update (ba, 6, BV.sub(bv, 1));
	      BA.update (ba, 5, BV.sub(bv, 2));
	      BA.update (ba, 4, BV.sub(bv, 3));
	      BA.update (ba, 3, BV.sub(bv, 4));
	      BA.update (ba, 2, BV.sub(bv, 5));
	      BA.update (ba, 1, BV.sub(bv, 6));
	      BA.update (ba, 0, BV.sub(bv, 7));
	      RA.sub(ra, 0)
	    end

    fun subVec (bv, i) = if (i < 0) orelse (Core.max_length <= i)
	  then raise General.Subscript
	  else let
	    val i = bytesPerElem * i
	    in
	      if (BV.length bv < (i ++ bytesPerElem))
		then raise Subscript
		else let
		  val ra = Assembly.A.create_r 1
		  val ba : BA.array = InlineT.cast ra
		  in
		    BA.update (ba, 7, BV.sub(bv, i++0));
		    BA.update (ba, 6, BV.sub(bv, i++1));
		    BA.update (ba, 5, BV.sub(bv, i++2));
		    BA.update (ba, 4, BV.sub(bv, i++3));
		    BA.update (ba, 3, BV.sub(bv, i++4));
		    BA.update (ba, 2, BV.sub(bv, i++5));
		    BA.update (ba, 1, BV.sub(bv, i++6));
		    BA.update (ba, 0, BV.sub(bv, i++7));
		    RA.sub(ra, 0)
		  end
	  end

    fun subArr (ba, i) = subVec (InlineT.cast ba, i)

    fun update (ba, i, x) = if (i < 0) orelse (Core.max_length <= i)
	  then raise General.Subscript
	  else let
	    val i = bytesPerElem * i
	    in
	      if (BA.length ba < (i ++ bytesPerElem))
		then raise Subscript
		else let
		  val ra = Assembly.A.create_r 1
		  val ba' : BA.array = InlineT.cast ra
		  in
		    BA.update (ba', 7, BA.sub(ba, i++0));
		    BA.update (ba', 6, BA.sub(ba, i++1));
		    BA.update (ba', 5, BA.sub(ba, i++2));
		    BA.update (ba', 4, BA.sub(ba, i++3));
		    BA.update (ba', 3, BA.sub(ba, i++4));
		    BA.update (ba', 2, BA.sub(ba, i++5));
		    BA.update (ba', 1, BA.sub(ba, i++6));
		    BA.update (ba', 0, BA.sub(ba, i++7))
		  end
	  end

  end (* PackReal64Swap *)
