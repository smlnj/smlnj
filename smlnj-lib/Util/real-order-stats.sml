(* real-order-stats.sml
 *
 *   Randomized linear-time selection from an unordered sample.
 *
 * Copyright (c) 2004 by The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)
structure RealOrderStats : sig

    (* WARNING: Each of the functions exported from this module
     * modifies its argument array by (partially) sorting it. *)

    (* select the i-th order statistic *)
    val select  : real array * int -> real
    val select' : real ArraySlice.slice * int -> real

    (* calculate the median:
     *    if N is odd, then this is the (floor(N/2))th order statistic
     *    otherwise it is the average of (N/2-1)th and (N/2)th *)
    val median  : real array -> real
    val median' : real ArraySlice.slice -> real

end = struct

    infix 8 $  val op $ = Unsafe.Array.sub
    infix 3 <- fun (a, i) <- x = Unsafe.Array.update (a, i, x)

    (* initialize random number generator *)
    val rand = Random.rand (123, 73256)

    (* select i-th order statistic from unsorted array with
     * starting point p and ending point r (inclusive): *)
    fun select0 (a: real array, p, r, i) =
	let fun x + y = Word.toIntX (Word.+ (Word.fromInt x, Word.fromInt y))
	    fun x - y = Word.toIntX (Word.- (Word.fromInt x, Word.fromInt y))
	    (* random partition: *)
	    fun rp (p, r) =
		let fun sw(i,j) = let val t=a$i in (a,i)<-a$j; (a,j)<-t end
		    val q = Random.randRange (p, r) rand
		    val qv = a$q
		    val _ = if q<>p then ((a,q)<-a$p; (a,p)<-qv) else ()
		    fun up i = if i>r orelse qv < a$i then i else up(i+1)
		    fun dn i = if i>=p andalso qv < a$i then dn(i-1) else i
		    fun lp (i, j) =
			let val (i, j) = (up i, dn j)
			in if i>j then let val q' = i-1 in sw(p,q'); (q',qv) end
			   else (sw(i,j); lp (i+1, j-1))
			end
		in lp (p+1, r) end
	    (* random select: *)
	    fun rs (p, r) =
		if p=r then a$r
		else let val (q, qv) = rp (p, r)
		     in if i=q then qv else if i<q then rs(p,q-1) else rs(q+1,r)
		     end
	in rs (p, r) end

    fun select (a, i) = select0 (a, 0, Array.length a - 1, i)
    fun select' (s, i) =
	let val (a, p, l) = ArraySlice.base s in select0 (a, p, p+l-1, p+i) end

    fun median0 (a, p, len) =
	let val mid = p + len div 2
	    val r = p + len - 1
	    val m0 = select0 (a, p, r, mid)
	    fun l(i,m) = if i>=mid then m else l(i+1, Real.max(a$i,m))
	in if len mod 2 = 1 then m0 else (l(p+1,a$p) + m0) / 2.0
	end

    fun median a = median0 (a, 0, Array.length a)
    fun median' s = median0 (ArraySlice.base s)
end
