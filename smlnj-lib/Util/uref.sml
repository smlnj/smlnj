(* uref.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * UNIONFIND DATA STRUCTURE WITH PATH COMPRESSION AND RANKED UNION
 *
 * Author:
 *    Fritz Henglein
 *    DIKU, University of Copenhagen
 *    henglein@diku.dk
 *)

structure URef : UREF =
  struct

    datatype 'a urefC
      = ECR of 'a * int
      | PTR of 'a uref
    withtype 'a uref = 'a urefC ref

    fun find (p as ref (ECR _)) = p
      | find (p as ref (PTR p')) = let
	  val p'' = find p'
          in
	    p := PTR p''; p''
          end

    fun uRef x = ref (ECR(x, 0))

    fun !! p = (case !(find p)
	   of ECR (x, _) => x
	    | _ => raise Match
	  (* end case *))

    fun equal (p, p') = (find p = find p')

    fun update (p, x) = (case find p
	   of (p' as ref(ECR(_, r))) => p' := ECR(x, r)
	    | _ => raise Match
	  (* end case *))

    fun link (p, q) = let
	  val p' = find p
          val q' = find q
	  in
	    if (p' = q') then false else (p' := PTR q; true)
	  end

    fun unify f (p, q) = (case (find p, find q)
	   of (p' as ref(ECR(pc, pr)), q' as ref(ECR(qc, qr))) =>
		let
		val newC = f (pc, qc)
		in
		  if p' = q'
		    then (p' := ECR(newC, pr); false)
		    else (
		      if pr = qr
			then (q' := ECR(newC, qr+1); p' := PTR q')
		      else if pr < qr
			then (q' := ECR(newC, qr); p' := PTR q')
		      else ((* pr > qr *)
			p' := ECR(newC, pr);
			q':= PTR p');
		      true)
		end
	    | _ => raise Match
	  (* end case *))

    fun union (p, q) = let
	  val p' = find p
	  val q' = find q
	  in
	    if (p' = q')
	      then false
	      else (case (!p', !q')
		 of (ECR(pc, pr), ECR(qc, qr)) => (
		      if pr = qr
			then (q' := ECR(qc, qr+1); p' := PTR q')
		      else if pr < qr
			then p' := PTR q'
			else q':= PTR p';
		      true)
		  | _ => raise Match
		(* end case *))
	  end

  end
