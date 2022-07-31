(* reorder.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Sethi-Ullman reordering of expression trees to minimize register usage
 *
 * See: Andrew W. Appel and  Kenneth J. Supowit,
 * Generalizations of the Sethi-Ullman algorithm for register allocation,
 * Software---Practice & Experience 17(6):417-421, 1987.
 *
 * In the expression (M,N) or (M N) it may be that N requires more
 * registers to compute than M, in which case it will be better
 * to compute   let val n=N and m=M in (M,N) end instead.
 *
 * This is no good if both M and N have side effects (read or write).
 * And it's not safe for space if N is the last use of some large
 * object, and M contains a function call that might allocate an
 * arbitrarily large amount.
 *
 * What does "last use" mean?
 *    1. SELECT(0,r) where the other fields of r might now be dead.
 *    2. boxed(r) where all the fields of r might now be dead.
 *    3. etc.
 * This is ONLY important if r is potentially of unbounded size.
 * Thus it doesn't apply to (for example) boxed floats, which are small.
 *
 * The property "possible read or write side effect" is called "side."
 * The property of "possible last use of some large object" is called "fetch."
 * The property of "possible unbounded allocation" is called "alloc."
 *)

signature REORDER =
sig
  val reorder : PLambda.lexp -> PLambda.lexp
end

structure Reorder : REORDER =
struct

local open Access PLambda
      structure P = Primop

in

fun bug s = ErrorMsg.impossible ("Reorder: "^s)

(*
datatype 'a info = I of {exp:'a,  (* The expression itself *)
		      regs:int,  (* how many registers needed for the
				    evaluation of this expression *)
		      side:bool, (* Does this expression read or write a ref
				    (conservative approximation) *)
		      fetch:bool,(* See explanation above *)
		      alloc:bool(* Might this expression allocate
				     more than a constant number of cells? *)
		      }

fun swap(I{side=true,...},I{side=true,...}) = false
      (* Don't interchange side effects *)
  | swap(I{fetch=true,...},I{alloc=true,...}) = false
      (* Don't move a SELECT(_,r) to the right of a big allocation,
       as this is not safe for space: If r is otherwise dead, we want
       its other fields to be garbage collected before allocating a lot*)
  | swap(I{regs=ra,...},I{regs=rb,...}) = ra < rb
      (* Evaluate the expression requiring more registers first,
         then hold its value in one register while evaluating the
	 other expression.  Minimizes max register usage.
      *)

fun inorder(a::(rest as b::_)) = if swap(a,b) then false else inorder rest
  | inorder _ = true

fun insert(a as (_,_), b::c) = if swap(#1 a, #1 b) then b::insert(a,c)
			                           else a::insert(b,c)
  | insert(a,nil) = a::nil

fun cost((I{regs,...},_)::rest) = Int.max(regs, 1+cost rest)
  | cost nil = 0

fun anyside ((I{side=true,...},_)::rest) = true
  | anyside (_::rest) = anyside rest
  | anyside nil = false

fun anyfetch ((I{fetch=true,...},_)::rest) = true
  | anyfetch (_::rest) = anyfetch rest
  | anyfetch nil = false

fun anyalloc ((I{alloc=true,...},_)::rest) = true
  | anyalloc (_::rest) = anyalloc rest
  | anyalloc nil = false

fun combine(l,do_it) =
 let fun g(I{exp=e1,side=s1,regs=r1,fetch=f1,alloc=a1}::rest, e,s,r,f,a) =
            g(rest, e1::e, s1 orelse s, Int.max(1+r, r1), f1 orelse f, a1 orelse a)
       | g(nil, e,s,r,f,a) = I{exp=do_it e, side=s, regs=r, fetch=f, alloc=a}
  in g(rev l, nil, false, 0, false, false)
 end

fun getexp (I{exp,...}) = exp

fun fetchprim P.BOXED = true
  | fetchprim P.UNBOXED = true
  | fetchprim P.LENGTH = true
  | fetchprim P.OBJLENGTH = true
  | fetchprim P.ASSIGN = true
  | fetchprim P.DEREF = false  (* The old ref cell might now be dead,
				 but not its contents! *)
  | fetchprim P.UPDATE = true  (* The "last use" in question
			         is the OLD contents of the array slot stored
				 into *)
  | fetchprim P.INLUPDATE = true
  | fetchprim P.UNBOXEDUPDATE = true
  | fetchprim P.SUBSCRIPT = true
  | fetchprim P.INLSUBSCRIPT = true
  | fetchprim P.SUBSCRIPTV = true
  | fetchprim (P.NUMSUBSCRIPT _) = true
  | fetchprim (P.NUMSUBSCRIPTV _) = true
  | fetchprim (P.NUMUPDATE _) = true
  | fetchprim (P.INLNUMSUBSCRIPT _) = true
  | fetchprim (P.INLNUMSUBSCRIPTV _) = true
  | fetchprim (P.INLNUMUPDATE _) = true
  | fetchprim P.GETTAG = true
  | fetchprim P.GETSPECIAL = true
  | fetchprim _ = false


fun sort(do_it, l) = if inorder l
     then combine(l, do_it)
     else let fun somevar (I{exp=VAR _,...}) = NONE
	        | somevar (I{exp=INT _,...}) = NONE
	        | somevar (I{exp=REAL _,...}) = NONE
	        | somevar (I{exp=STRING _,...}) = NONE
	        | somevar _ = SOME(LambdaVar.mkLvar())

	      val l' = map (fn x => (x, somevar x)) l

              val l'' = foldr insert [] l'

	      fun rename (_,SOME v) = VAR v
		| rename (I{exp,...},NONE) = exp

	      fun bind ((_,NONE), e) = e
		| bind ((I{exp,...},SOME v),e) = LET(v, SVAL exp, e)

	   in I{regs= cost l'',
	        side = anyside l'',
	        fetch = anyfetch l'',
	        alloc = anyalloc l'',
	        exp = foldr bind (do_it (map rename l')) l''
	       }
	  end

val many = 12   (* how many regs to charge a function call *)

val rec lpsv : value -> value info =
  fn e as VAR _ => I{regs=0, side=false, exp=e, fetch=false, alloc=false}
   | e as INT _ => I{regs=0, side=false, fetch=false, alloc=false, exp=e}
   | e as WORD _ => I{regs=0, side=false, fetch=false, alloc=false, exp=e}
   | e as INT32 _ => I{regs=0, side=false, fetch=false, alloc=false, exp=e}
   | e as WORD32 _ => I{regs=0, side=false, fetch=false, alloc=false, exp=e}
   | e as REAL _ => I{regs=0, side=false, fetch=false, alloc=false, exp=e}
   | e as STRING _ => I{regs=0, side=false, fetch=false, alloc=false, exp=e}
   | e as PRIM(i,t,_) => I{regs=0,side=false,fetch=false,alloc=false,exp=e}
   | _ => bug "unexpected case in lpsv"

and loop : lexp -> lexp info =
  fn e as SVAL sv =>
        let val I{regs, side, exp, fetch, alloc} = lpsv sv
         in I{regs=regs, side=side, exp=SVAL exp, fetch=fetch, alloc=alloc}
        end
   | e as ETAG _ => I{regs=1, side=true, fetch=false, alloc=true, exp=e}

   | FN(v,t,e) => I{regs=1, side=false, fetch=false, alloc=false,
		    exp= FN(v,t,getexp(loop e))}
   | FIX(vl,t,el,e) =>
          let val I{regs,side,exp,fetch,alloc} = loop e
           in I{regs=regs+1,side=side,fetch=fetch,alloc=alloc,
		exp=FIX(vl,t,el,exp)}
          end
   | APP(p as PRIM(i,t,_), b) =>
          let val I{regs,side,fetch,alloc,exp=e1} = lpsv b
	   in I{regs=Int.max(1,regs),side=not(P.purePrimop i),
	        alloc=false, fetch=fetchprim i, exp=APP(p, e1)}
	  end
   | LET(v, b, a) =>
	  let val I{regs=ra,side=sa,exp=ea,fetch=fa,alloc=aa} =loop a
	      val I{regs=rb,side=sb,exp=eb,fetch=fb,alloc=ab} =loop b
	   in I{regs=Int.max(rb,1+ra),side=sa orelse sb,
	        fetch= fa orelse fb, alloc=aa orelse ab,
	        exp=LET(v,eb,ea)}
	  end
   | APP(a,b) =>
          let val I{exp=e1,...} =
		  sort(fn [x,y]=>APP(x,y), map lpsv [a,b])
	   in I{regs=many,side=true,exp=e1,fetch=true,alloc=true}
          end
   | SWITCH(v0,sign,l,d) =>
          let val I{regs,side,exp,fetch,alloc}= lpsv v0
	      fun combine((c,e),(r,s,f,a,el)) =
		    let val I{regs,side,exp,fetch,alloc}=loop e
		     in (Int.max(r,regs),s orelse side,f orelse fetch,
			a orelse alloc,(c,exp)::el)
		    end
              val (lr,ls,lf,la,l') = foldr combine (regs,side,fetch,alloc,[]) l

           in case d
               of SOME d' =>
		   let val (lr,ls,lf,la,[((),de)]) =
		                     combine(((),d'),(lr,ls,lf,la,nil))
		    in I{regs=lr,side=ls,fetch=lf,alloc=la,
			 exp=SWITCH(exp,sign,l',SOME de)}
		   end
                | NONE => I{regs=lr,side=ls,fetch=lf,alloc=la,
		  	    exp=SWITCH(exp,sign,l',NONE)}
          end

   | CON(c,ts,v) => let val I{regs,side,exp,fetch,alloc} = lpsv v
	          in I{regs=Int.max(1,regs), side=side, fetch=fetch, alloc=alloc,
		       exp=CON(c,ts,exp)} (* close enuf *)
		 end
   | DECON(c,ts,v) => let val I{regs,side,exp,fetch,alloc} = lpsv v
	          in I{regs=Int.max(regs,1), side=side, fetch=true,alloc=alloc,
		       exp=DECON(c,ts,exp)}
		 end
   | RECORD l => sort(fn x => RECORD x, map lpsv l)
   | SRECORD l => sort(fn x => SRECORD x, map lpsv l)
   | VECTOR (l, t) => sort(fn x => VECTOR (x,t), map lpsv l)
   | SELECT(i,e) => let val I{regs,side,exp,fetch,alloc} = lpsv e
                     in I{regs=Int.max(1,regs),side=side,fetch=true,alloc=alloc,
			  exp=SELECT(i,exp)}
                    end
   | RAISE(e,t) =>  let val I{regs,side,exp,fetch,alloc} = lpsv e
                     in I{regs=Int.max(1,regs),side=true,fetch=fetch,alloc=alloc,
			  exp=RAISE(exp,t)}
                    end
   | HANDLE(a,b) => let val I{regs=ra,side=sa,exp=ea,fetch=fa,alloc=aa} =
	                                                          loop a
                        val I{regs=rb,side=sb,exp=eb,fetch=fb,alloc=ab} =
			                                          lpsv b
                     in I{regs=ra,side=sa orelse sb,
			  fetch=fa orelse fb, alloc = aa orelse ab,
			  exp=HANDLE(ea,eb)}
                    end
   | WRAP(t,c,e) => let val I{regs,side,exp,fetch,alloc} = lpsv e
                   in I{regs=regs, side=side, fetch=true, alloc=alloc,
                        exp=WRAP(t,c,exp)}
                  end
   | UNWRAP(t,c,e) => let val I{regs,side,exp,fetch,alloc} = lpsv e
                     in I{regs=regs, side=side, fetch=true, alloc=alloc,
                          exp=UNWRAP(t,c,exp)}
                    end
   | _ => bug "unsupported lambda expression in loop"

*)
val reorder = fn x => bug "reorder not implemented" (* getexp (loop x) *)

end (* toplevel local *)
end (* structure Reorder *)

