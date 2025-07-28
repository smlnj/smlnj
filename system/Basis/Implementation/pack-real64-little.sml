(* pack-real64-little.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (http://www.smlnj.org)
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

(* TODO: this function should be defined in InlineT *)
    (* bitcast a Word64.word to a Real64.real *)
    fun fromBits (b : Word64.word) : real = let
          val r : real ref = InlineT.cast(ref b)
          in
            !r
          end
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
            if InlineT.isBigEndian()
              then (
                (* big -> little *)
                update (7, W64.rshiftl(w, 0w56));
                update (6, W64.rshiftl(w, 0w48));
                update (5, W64.rshiftl(w, 0w40));
                update (4, W64.rshiftl(w, 0w32));
                update (3, W64.rshiftl(w, 0w24));
                update (2, W64.rshiftl(w, 0w16));
                update (1, W64.rshiftl(w, 0w8));
                update (0, w))
              else (
                (* little -> little *)
                update (0, W64.rshiftl(w, 0w56));
                update (1, W64.rshiftl(w, 0w48));
                update (2, W64.rshiftl(w, 0w40));
                update (3, W64.rshiftl(w, 0w32));
                update (4, W64.rshiftl(w, 0w24));
                update (5, W64.rshiftl(w, 0w16));
                update (6, W64.rshiftl(w, 0w8));
                update (7, w));
            bv
          end

    fun fromBytes bv = if BV.length bv < bytesPerElem
	  then raise Subscript
	  else let
            fun get i = InlineT.Word8.toLarge(BV.sub(bv, i))
            val w = if InlineT.isBigEndian()
                  then (* little -> big *)
                    W64.orb(W64.lshift(get 7, 0w56),
                    W64.orb(W64.lshift(get 6, 0w48),
                    W64.orb(W64.lshift(get 5, 0w40),
                    W64.orb(W64.lshift(get 4, 0w32),
                    W64.orb(W64.lshift(get 3, 0w24),
                    W64.orb(W64.lshift(get 2, 0w16),
                    W64.orb(W64.lshift(get 1, 0w8),
                    get 0)))))))
                  else (* little -> little *)
                    W64.orb(W64.lshift(get 0, 0w56),
                    W64.orb(W64.lshift(get 1, 0w48),
                    W64.orb(W64.lshift(get 2, 0w40),
                    W64.orb(W64.lshift(get 3, 0w32),
                    W64.orb(W64.lshift(get 4, 0w24),
                    W64.orb(W64.lshift(get 5, 0w16),
                    W64.orb(W64.lshift(get 6, 0w8),
                    get 7)))))))
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
                  val w = if InlineT.isBigEndian()
                        then (* little -> big *)
                          W64.orb(W64.lshift(get 7, 0w56),
                          W64.orb(W64.lshift(get 6, 0w48),
                          W64.orb(W64.lshift(get 5, 0w40),
                          W64.orb(W64.lshift(get 4, 0w32),
                          W64.orb(W64.lshift(get 3, 0w24),
                          W64.orb(W64.lshift(get 2, 0w16),
                          W64.orb(W64.lshift(get 1, 0w8),
                          get 0)))))))
                        else (* little -> little *)
                          W64.orb(W64.lshift(get 0, 0w56),
                          W64.orb(W64.lshift(get 1, 0w48),
                          W64.orb(W64.lshift(get 2, 0w40),
                          W64.orb(W64.lshift(get 3, 0w32),
                          W64.orb(W64.lshift(get 4, 0w24),
                          W64.orb(W64.lshift(get 5, 0w16),
                          W64.orb(W64.lshift(get 6, 0w8),
                          get 7)))))))
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
                  val w = if InlineT.isBigEndian()
                        then (* little -> big *)
                          W64.orb(W64.lshift(get 7, 0w56),
                          W64.orb(W64.lshift(get 6, 0w48),
                          W64.orb(W64.lshift(get 5, 0w40),
                          W64.orb(W64.lshift(get 4, 0w32),
                          W64.orb(W64.lshift(get 3, 0w24),
                          W64.orb(W64.lshift(get 2, 0w16),
                          W64.orb(W64.lshift(get 1, 0w8),
                          get 0)))))))
                        else (* little -> little *)
                          W64.orb(W64.lshift(get 0, 0w56),
                          W64.orb(W64.lshift(get 1, 0w48),
                          W64.orb(W64.lshift(get 2, 0w40),
                          W64.orb(W64.lshift(get 3, 0w32),
                          W64.orb(W64.lshift(get 4, 0w24),
                          W64.orb(W64.lshift(get 5, 0w16),
                          W64.orb(W64.lshift(get 6, 0w8),
                          get 7)))))))
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
                    if InlineT.isBigEndian()
                      then (
                        (* big -> little *)
                        update (7, W64.rshiftl(w, 0w56));
                        update (6, W64.rshiftl(w, 0w48));
                        update (5, W64.rshiftl(w, 0w40));
                        update (4, W64.rshiftl(w, 0w32));
                        update (3, W64.rshiftl(w, 0w24));
                        update (2, W64.rshiftl(w, 0w16));
                        update (1, W64.rshiftl(w, 0w8));
                        update (0, w))
                      else (
                        (* little -> little *)
                        update (0, W64.rshiftl(w, 0w56));
                        update (1, W64.rshiftl(w, 0w48));
                        update (2, W64.rshiftl(w, 0w40));
                        update (3, W64.rshiftl(w, 0w32));
                        update (4, W64.rshiftl(w, 0w24));
                        update (5, W64.rshiftl(w, 0w16));
                        update (6, W64.rshiftl(w, 0w8));
                        update (7, w))
                  end
            end

  end
