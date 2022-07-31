(* probability.sml
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies.
 *
 * A representation of probabilities for branch prediction.
 *)

signature PROBABILITY =
  sig

    type prob

    exception BadProb

    val never : prob	(* 0% probability *)
    val unlikely : prob	(* very close to 0% *)
    val likely : prob	(* very close to 100% *)
    val always : prob	(* 100% probability *)

    val prob : (int * int) -> prob
    val fromFreq : int list -> prob list

    val + : (prob * prob) -> prob
    val - : (prob * prob) -> prob
    val * : (prob * prob) -> prob
    val / : (prob * int) -> prob
    val not : prob -> prob		(* not p == always - p *)

    val percent : int -> prob

  (* combine a conditional branch probability (trueProb) with a
   * prediction heuristic (takenProb) using Dempster-Shafer theory.
   *)
    val combineProb2 : {trueProb : prob, takenProb : prob} -> {t : prob, f : prob}

    val toReal : prob -> real
    val toString : prob -> string

  end

structure Probability :> PROBABILITY =
  struct

    open IntInf

    val zero = fromInt 0
    val one = fromInt 1
    val two = fromInt 2
    val hundred = fromInt 100
    fun eq (a, b) = (compare(a, b) = EQUAL)

  (* Probabilities are represented as positive rationals.  Zero is
   * represented as PROB(0w0, 0w0) and one is represented as
   * PROB(0w1, 0w1).  There are several invariants about PROB(n, d):
   *	1) n <= d
   *	2) if n = 0w0, then d = 0w0 (uniqueness of zero)
   *	3) if d = 0w1, then n = 0w1 (uniqueness of one)
   *)
    datatype prob = PROB of (IntInf.int * IntInf.int)

    exception BadProb

    val never = PROB(zero, one)
    val unlikely = PROB(one, fromInt 1000)
    val likely = PROB(fromInt 999, fromInt 1000)
    val always = PROB(one, one)

    fun gcd (m, n) = if eq(n, zero) then m else gcd(n, m mod n)

    fun normalize (n, d) =
	  if eq(n, zero) then never
	  else (case compare(n, d)
	     of LESS => let
		  val g = gcd(n, d)
		  in
		    if eq(g, one)
		      then PROB(n, d)
		      else PROB(n div g, d div g)
		  end
	      | EQUAL => always
	      | GREATER => raise BadProb
	  (* end case *))
	    
    fun prob (n, d) =
	  if Int.>(n, d) orelse Int.<(n, 0) orelse Int.<=(d, 0)
	    then raise Domain
	    else normalize(fromInt n, fromInt d)

    fun add (PROB(n1, d1), PROB(n2, d2)) = normalize(d2*n1 + d1*n2, d1*d2)

    fun sub (PROB(n1, d1), PROB(n2, d2)) = let
	  val n1' = d2*n1
	  val n2' = d1*n2
	  in
	    if (n1' < n2') then raise BadProb else normalize(n1'-n2', d1*d2)
	  end

    fun mul (PROB(n1, d1), PROB(n2, d2)) = normalize (n1*n2, d1*d2)

    fun divide (PROB(n, d), m) = if Int.<=(m, 0)
	  then raise BadProb
	  else if eq(n, zero) then never
	  else normalize(n, d * fromInt m)

    fun percent n =
	  if Int.<(n, 0) then raise BadProb
	  else normalize(fromInt n, hundred)

    fun fromFreq l = let
	  fun sum ([], tot) = tot
	    | sum (w::r, tot) = if Int.<(w, 0)
		then raise BadProb
		else sum(r, fromInt w + tot)
	  val tot = sum (l, zero)
	  in
	    List.map (fn w => normalize(fromInt w, tot)) l
	  end

    fun toReal (PROB(n, d)) =
	  if eq(n, zero) then 0.0
	  else if eq(d, one) then 1.0
	  else let
	    val sz = log2 d
	    val (n, d) = if Int.>=(sz, 30)
		  then let
		    val scale = pow(two, Int.-(sz, 30))
		    val n = n div scale
		    in
		      (if n > zero then n else one, d div scale)
		    end
		  else (n, d)
	    fun toReal n = Real.fromLargeInt(toLarge n)
	    in
	      toReal n / toReal d
	    end

    fun toString (PROB(n, d)) =
	  if eq(n, zero) then "0"
	  else if eq(d, one) then "1"
	  else concat [IntInf.toString n, "/", IntInf.toString d]

  (* combine a conditional branch probability (trueProb) with a
   * prediction heuristic (takenProb) using Dempster-Shafer theory.
   * The basic equations (from Wu-Larus 1994) are:
   *    t = trueProb*takenProb / d
   *	f = ((1-trueProb)*(1-takenProb)) / d
   * where
   *	d = trueProb*takenProb + ((1-trueProb)*(1-takenProb))
   *)
    fun combineProb2 {trueProb=PROB(n1, d1), takenProb=PROB(n2, d2)} = let
	(* compute sn/sd, where
	 *    sd/sn = (trueProb*takenProb) + (1-trueProb)*(1-takenProb)
	 *)
	  val d12 = d1*d2
	  val n12 = n1*n2
	  val (sn, sd) = let
		val n = d12 + two*n12 - (d2*n1) - (d1*n2)
		in
		  (d12, n)
		end
	(* compute the true probability *)
	  val t as PROB(tn, td) = normalize(n12*sn, d12*sd)
	(* compute the false probability *)
	  val f = PROB(td-tn, td)
	  in
	    {t = t, f = f}
	  end

    fun not (PROB(n, d)) = PROB(d-n, d)

    val op + = add
    val op - = sub
    val op * = mul
    val op / = divide

  end

