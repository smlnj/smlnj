(* unrebind.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(****************************************************************************
 *                                                                          *
 * "Alpha conversion": the closure converter introduces duplicate bindings  *
 * at function arguments (the free variables of known functions) and at     *
 * SELECT's and OFFSET's from closures.  This function restores unique      *
 * bindings, and also eliminates OFFSET's of 0 (which are introduced as     *
 * a side effect of trying to improve lazy display).  It assumes that a     *
 * FIX has no free variables.                                               *
 *                                                                          *
 ****************************************************************************)

signature UNREBIND =
  sig
    val unrebind : CPS.function -> CPS.function
  end

structure UnRebind : UNREBIND = struct

local
  open CPS
in

fun bug s = ErrorMsg.impossible ("UnRebind: " ^ s)

fun unrebind (fk,v,args,cl,ce) =
let fun rename rebind(VAR v) =
	  let fun f nil = VAR v
		| f ((w:LambdaVar.lvar, v')::t) = if v=w then v' else f t
	   in f rebind
          end
      | rename _ x = x

    fun f (kind,l,args,cl,b) =
      let val (args',rebind') =
            foldr (fn (v,(args',rebind')) =>
		       let val v' = LambdaVar.dupLvar v
			in  (v'::args',(v, VAR v')::rebind')
		       end)
	        (nil,nil) args
       in (kind,l,args',cl,g rebind' b)
      end

    and g (rebind: (lvar * value) list) =
      let val rename = rename rebind
	  val rec h =
	       fn RECORD(kind,vl,w,e) =>
                    let val w' = LambdaVar.dupLvar w
                     in RECORD(kind, map (fn(v,p) => (rename v,p)) vl,
                               w', g ((w, VAR w')::rebind) e)
                    end
		| OFFSET(0,v,w,e) => g ((w,rename v)::rebind) e
		| OFFSET(i,v,w,e) => bug "unexpected none-zero OFFSET"
(*
                    let val w' = LambdaVar.dupLvar w
		     in OFFSET(i, rename v, w', g ((w, VAR w')::rebind) e)
		    end
*)
		| SELECT(i,v,w,t,e) =>
		    let val w' = LambdaVar.dupLvar w
		     in SELECT(i, rename v, w', t, g((w, VAR w')::rebind) e)
		    end
		| APP(f,vl) => APP(rename f,map rename vl)
		| FIX(l,e) => FIX(map f l,h e)
		| SWITCH(v,c,el) => SWITCH(rename v,c,map h el)
		| BRANCH(i,vl,c,e1,e2) => BRANCH(i,map rename vl,c, h e1, h e2)
		| SETTER(i,vl,e) => SETTER(i,map rename vl,h e)
		| LOOKER(i,vl,w,t,e) => LOOKER(i,map rename vl,w,t,h e)
		| ARITH(i,vl,w,t,e) => ARITH(i,map rename vl,w,t,h e)
		| PURE(i,vl,w,t,e) => PURE(i,map rename vl,w,t,h e)
		| RCC(k,l,p,vl,wtl,e) => RCC(k, l, p, map rename vl, wtl, h e)
       in  h
      end

 in (fk,v,args,cl,g nil ce)
end (* unrebind *)

end (* local *)

end (* structure UnRebind *)


