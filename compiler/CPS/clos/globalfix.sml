(* globalfix.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature GLOBALFIX =
  sig
    val globalfix : CPS.function -> CPS.function list
  end

structure GlobalFix : GLOBALFIX =
  struct

    open CPS

    fun globalfix (fk, f, vl, cl, cexp) = let
	  fun gfix ce = (case ce
		 of FIX(fl,c) =>
		     let val (n,c') = gfix c
			 val l' = foldl
			   (fn((k,v,a,t,c),m) => let val (l,d) = gfix c in (k,v,a,t,d)::l@m end)
			     n fl
		      in (l',c')
		     end
		  | APP _ => ([],ce)
		  | SWITCH(v,c0,l) =>
		     let val (f,l') =
			   foldr (fn(c,(fl,cl)) => let val (f,d) = gfix c in (f@fl,d::cl) end)
			     ([],[]) l
		      in (f,SWITCH(v,c0,l'))
		     end
		  | RECORD(k,l,v,c) => let val (f,c') = gfix c
		      in (f,RECORD(k,l,v,c')) end
		  | SELECT(i,v,w,t,c) => let val (f,c') = gfix c
		      in (f,SELECT(i,v,w,t,c')) end
		  | OFFSET(i,v,w,c) => let val (f,c') = gfix c
		      in (f,OFFSET(i,v,w,c')) end
		  | SETTER(i,vl,c) => let val (f,c') = gfix c
		      in (f,SETTER(i,vl,c')) end
		  | LOOKER(i,vl,w,t,c) => let val (f,c') = gfix c
		      in (f,LOOKER(i,vl,w,t,c')) end
		  | ARITH(i,vl,w,t,c) => let val (f,c') = gfix c
		      in (f,ARITH(i,vl,w,t,c')) end
		  | PURE(i,vl,w,t,c) => let val (f,c') = gfix c
		      in (f,PURE(i,vl,w,t,c')) end
		  | BRANCH(i,args,c,e1,e2) => let
		      val (f1,e1') = gfix e1
		      val (f2,e2') = gfix e2
		      in
			(f1@f2, BRANCH(i,args,c,e1',e2'))
		      end
		  | RCC(k,l,p,vl,wtl,c) => let val (f,c') = gfix c
		      in (f,RCC(k,l,p,vl,wtl,c')) end
		(* end case *))
	  val (l, body) = gfix cexp
	  in
	    (fk, f, vl, cl, body) :: l
	  end

  end (* structure GlobalFix *)
