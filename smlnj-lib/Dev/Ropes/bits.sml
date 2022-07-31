local
  structure W = Word32
  fun toWord w = Word.fromLarge(W.toLarge w)
in

  (* compute the number of leading zeros in a word;
   * this code was translated from the C code in
   * Figure 5.10 (p.80) of
   *
   *	Hackers Delight
   *	by Henry S. Warren, Jr.
   *	Pearson Education Inc, 2003.
   *)
    fun nlz x = let
	  val y = W.~ (W.>> (x, 0w16))
	  val m = W.andb(W.>>(y, 0w16), 0w16)
	  val n = 0w16 - m
	  val x = W.>>(x, toWord m)
	  val y = x - 0wx100
	  val m = W.andb(W.>>(y, 0w16), 0w8)
	  val n = n + m
	  val x = W.<<(x, toWord m)
	  val y = x - 0wx1000
	  val m = W.andb(W.>>(y, 0w16), 0w4)
	  val n = n + m
	  val x = W.<<(x, toWord m)
	  val y = x - 0wx4000
	  val m = W.andb(W.>>(y, 0w16), 0w2)
	  val n = n + m
	  val x = W.<<(x, toWord m)
	  val y = W.>>(x, 0w14)
	  val m = W.andb(y, W.notb(W.>>(y, 0w1)))
	  in
	    W.toIntX (n + 0w2 - m)
	  end
    fun ceilLog2 x = (31 - nlz(W.fromInt(x-1))) + 1
end (* local *)
