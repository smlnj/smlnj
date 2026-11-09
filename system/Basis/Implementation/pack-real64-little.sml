(* pack-real64-little.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure PackReal64Little : PACK_REAL =
  struct

    structure BV = InlineT.Word8Vector
    structure BA = InlineT.Word8Array
    structure W64 = InlineT.Word64

    (* fast add avoiding the overflow test *)
    infix ++
    fun x ++ y = InlineT.Int.fast_add(x, y)

    type real = Real64Imp.real

    val fromBits = InlineT.Real64.fromBits
    val toBits = InlineT.Real64.toBits

    (* create an uninitialized Word8Vector *)
    val createW8Vec : int -> BV.vector = InlineT.cast Assembly.A.create_s

    val bytesPerElem : int = 8

    val isBigEndian : bool = false

    fun toBytes r = let
          val w = toBits r
          val bv = createW8Vec bytesPerElem
          fun update (i, w) = BV.update (bv, i, InlineT.Word8.fromLarge w)
          in
(* NOTE: if we have a primop for writing a 64-bit word into a byte array, then
 * we can define a fast path for then `not InlineT.isBigEndian()` is true.
 *)
            update (7, W64.rshiftl(w, 0w56));
            update (6, W64.rshiftl(w, 0w48));
            update (5, W64.rshiftl(w, 0w40));
            update (4, W64.rshiftl(w, 0w32));
            update (3, W64.rshiftl(w, 0w24));
            update (2, W64.rshiftl(w, 0w16));
            update (1, W64.rshiftl(w, 0w8));
            update (0, w);
            bv
          end

    fun fromBytes bv = if BV.length bv < bytesPerElem
	  then raise Subscript
	  else let
            fun get i = InlineT.Word8.toLarge(BV.sub(bv, i))
(* NOTE: if we have a primop for reading a 64-bit word from a byte array, then
 * we can define a fast path for then `not InlineT.isBigEndian()` is true.
 *)
            val w = W64.orb(W64.lshift(get 7, 0w56),
                    W64.orb(W64.lshift(get 6, 0w48),
                    W64.orb(W64.lshift(get 5, 0w40),
                    W64.orb(W64.lshift(get 4, 0w32),
                    W64.orb(W64.lshift(get 3, 0w24),
                    W64.orb(W64.lshift(get 2, 0w16),
                    W64.orb(W64.lshift(get 1, 0w8),
                    get 0)))))))
            in
              fromBits w
            end

    fun subVec (bv, i) = if (i < 0) orelse (Core.max_length <= i)
	  then raise Subscript
	  else let
	    val base = bytesPerElem * i
	    in
	      if (BV.length bv < (base ++ bytesPerElem))
		then raise Subscript
		else let
                  fun get i = InlineT.Word8.toLarge(BV.sub(bv, base ++ i))
                  val w = W64.orb(W64.lshift(get 7, 0w56),
                          W64.orb(W64.lshift(get 6, 0w48),
                          W64.orb(W64.lshift(get 5, 0w40),
                          W64.orb(W64.lshift(get 4, 0w32),
                          W64.orb(W64.lshift(get 3, 0w24),
                          W64.orb(W64.lshift(get 2, 0w16),
                          W64.orb(W64.lshift(get 1, 0w8),
                          get 0)))))))
                  in
                    fromBits w
                  end
	    end

    fun subArr (ba, i) = if (i < 0) orelse (Core.max_length <= i)
	  then raise Subscript
	  else let
	    val base = bytesPerElem * i
	    in
	      if (BA.length ba < (base ++ bytesPerElem))
		then raise Subscript
		else let
                  fun get i = InlineT.Word8.toLarge(BA.sub(ba, base ++ i))
                  val w = W64.orb(W64.lshift(get 7, 0w56),
                          W64.orb(W64.lshift(get 6, 0w48),
                          W64.orb(W64.lshift(get 5, 0w40),
                          W64.orb(W64.lshift(get 4, 0w32),
                          W64.orb(W64.lshift(get 3, 0w24),
                          W64.orb(W64.lshift(get 2, 0w16),
                          W64.orb(W64.lshift(get 1, 0w8),
                          get 0)))))))
                  in
                    fromBits w
                  end
	    end

    fun update (ba, i, r) = if (i < 0) orelse (Core.max_length <= i)
	  then raise General.Subscript
	  else let
	    val base = bytesPerElem * i
	    in
	      if (BA.length ba < (base ++ bytesPerElem))
		then raise Subscript
		else let
                  fun update (i, w) = BA.update (ba, base ++ i, InlineT.Word8.fromLarge w)
                  val w = toBits r
                  in
                    update (7, W64.rshiftl(w, 0w56));
                    update (6, W64.rshiftl(w, 0w48));
                    update (5, W64.rshiftl(w, 0w40));
                    update (4, W64.rshiftl(w, 0w32));
                    update (3, W64.rshiftl(w, 0w24));
                    update (2, W64.rshiftl(w, 0w16));
                    update (1, W64.rshiftl(w, 0w8));
                    update (0, w)
                  end
            end

  end
