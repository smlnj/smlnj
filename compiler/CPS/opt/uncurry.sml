(* uncurry.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

functor Uncurry(MachSpec : MACH_SPEC) : ETASPLIT  =
struct

local
  structure LD = LtyDef
  structure LB = LtyBasic
  structure LV = LambdaVar
  open CPS
in

fun bug s = ErrorMsg.impossible ("Uncurry: " ^ s)

fun freein v =
  let fun try(VAR w) = v=w
	| try(LABEL w) = v=w
	| try _ = false

      fun any(w :: rest) = try w orelse any rest
	| any nil = false

      fun any1((w,_)::rest) = try w orelse any1 rest
	| any1 nil = false

      val rec g =
	 fn APP(f,args) => try f orelse any args
	  | SWITCH(v,c,l) => try v orelse List.exists g l
	  | RECORD(_,l,w,ce) => any1 l orelse g ce
	  | SELECT(_,v,w,_,ce) => try v orelse g ce
	  | OFFSET(_,v,w,ce) => try v orelse g ce
	  | (SETTER(_,vl,e) |
	     LOOKER(_,vl,_,_,e) |
	     ARITH(_,vl,_,_,e) |
	     PURE(_,vl,_,_,e) |
	     RCC(_,_,_,vl,_,e)) => any vl orelse g e
	  | BRANCH(_,vl,c,e1,e2) => any vl orelse g e1 orelse g e2
	  | FIX(fl, e) => List.exists (g o #5) fl  orelse  g e
   in g
  end

fun etasplit {function=(fkind,fvar,fargs,ctyl,cexp), click} =
let

val debug = !Control.CG.debugcps (* false *)
fun debugprint s = if debug then Control.Print.say s else ()
fun debugflush() = if debug then Control.Print.flush() else ()

val defaultArrow = LD.ltc_parrow (LB.ltc_void, LB.ltc_void)

fun extendLty(t,[]) = t
  | extendLty(t,a) = defaultArrow

(* count the number of GP and FP registers needed for a list of lvars *)
val unboxedfloat = MachSpec.unboxedFloats

fun isFltCty (FLTt _) = unboxedfloat
  | isFltCty _ = false

val numCSgpregs = MachSpec.numCalleeSaves
val numCSfpregs = MachSpec.numFloatCalleeSaves
val maxgpregs = MachSpec.numRegs - numCSgpregs - 1
val maxfpregs = MachSpec.numFloatRegs - numCSfpregs - 2

fun checklimit(cl) =
  let fun h(FLTt _::r, m, n) = if unboxedfloat then h(r,m,n+1) else h(r,m+1,n)
        | h(_::r, m, n) = h(r,m+1,n)
        | h([], m, n) = (m <= maxgpregs) andalso (n <= maxfpregs)
   in h(cl, 0, 0)
  end

fun copyLvar v = LV.dupLvar v

val rec reduce =
   fn RECORD(k,vl,w,e) => RECORD(k, vl, w, reduce e)
    | SELECT(i,v,w,t,e) => SELECT(i, v, w, t, reduce e)
    | OFFSET(i,v,w,e) => OFFSET(i, v, w, reduce e)
    | APP(f,vl) => APP(f, vl)
    | SWITCH(v,c,el) => SWITCH(v, c,map reduce el)
    | BRANCH(i,vl,c,e1,e2) => BRANCH(i, vl, c, reduce e1, reduce e2)
    | LOOKER(i,vl,w,t,e) => LOOKER(i, vl, w, t, reduce e)
    | ARITH(i,vl,w,t,e) => ARITH(i, vl, w, t, reduce e)
    | PURE(i,vl,w,t,e) => PURE(i, vl, w, t, reduce e)
    | RCC(k,l,p,vl,wtl,e) => RCC(k, l, p, vl, wtl, reduce e)
    | SETTER(i,vl,e) => SETTER(i, vl, reduce e)
    | FIX(l,e) =>
       let fun uncurry(fd as (CONT,_,_,_,_)) = [reduce_body(fd)]
	     | uncurry(fd as
		       (fk,f,k::vl,ct::cl,body as FIX([(gk,g,ul,cl',body2)],
						      APP(VAR c,[VAR g'])))) =
                if k=c andalso g=g' (* andalso userfun(g) *)
                   andalso  not (freein k body2)
		   andalso not (freein g body2)   (* g not recursive *)
   		   andalso checklimit(cl@cl')
   		   then let val ul' = map copyLvar ul
			    and vl' = map copyLvar vl
			    val k'= copyLvar k
			    and g'= copyLvar g
			    val f' = LV.mkLvar()
			in click "u";
			    (NO_INLINE_INTO,f,k'::vl',ct::cl,
			     FIX([(gk,g',ul',cl',APP(VAR f',
						     map VAR (ul' @ vl')))],
				 APP(VAR(k'),[VAR g'])))
			    ::uncurry(fk,f',ul@vl,cl'@cl,body2)
			end
                     else [reduce_body(fd)]
   	     | uncurry fd = [reduce_body(fd)]

	   and reduce_body (fk,f,vl,cl,e) = (fk,f,vl,cl,reduce e)

        in FIX(foldr (fn (fd,r) => (uncurry fd) @ r) [] l,
	       reduce e)
       end

 in debugprint "Uncurry: ";
    debugflush();
    (fkind, fvar, fargs, ctyl, reduce cexp) before debugprint "\n"
end

end (* toplevel local *)
end (* functor Uncurry *)


