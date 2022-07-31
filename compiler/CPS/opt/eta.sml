(* eta.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(***********************************************************************
 *                                                                     *
 *   The function eta is an eta reducer for cps expressions.  It is    *
 *   guaranteed to reach an eta normal form in at most two passes.  A  *
 *   high-level description of the algorithm follows.                  *
 *                                                                     *
 *   eta essentially takes two arguments, a cps expression and an      *
 *   environment mapping variables to values.  (In practice, the       *
 *   environment is a global variable.)  The environment is used to    *
 *   keep track of the eta reductions performed.  The algorithm can be *
 *   explained by the two key clauses below (written in pseudo-cps     *
 *   notation):                                                        *
 *                                                                     *
 *   [FIX]   eta(env, *let* f[x1,...,xN] = M1                          *
 *                    *in*  M2)                                        *
 *                                                                     *
 *         --> let M1' = eta(env,M1)                                   *
 *             in  if M1' = g[x1,...,xN]                               *
 *                 then eta(env[f := g],M2)                            *
 *                 else *let* f[x1,...,xN] = M1'                       *
 *                      *in*  eta(env,M2)                              *
 *             end                                                     *
 *                                                                     *
 *   [APP]   eta(env,f[v1,...,vN])                                     *
 *                                                                     *
 *         --> env(f)[env(v1),...,env(vN)]                             *
 *                                                                     *
 *   In the [FIX] case of function definition, we first eta reduce the *
 *   body M1 of the function f, then see if f is itself an eta         *
 *   redex f[x1,...,xN] = g[x1,...,xN].  If so, we will use g for f    *
 *   elsewhere in the cps expression.                                  *
 *                                                                     *
 *   The [APP] case shows where we must rename variables.              *
 *                                                                     *
 *   This would get all eta redexes in one pass, except for the        *
 *   following problem.  Consider the cps code below:                  *
 *                                                                     *
 *          *let* f[x1,...,xN] = M1                                    *
 *          *and* g[y1,...,yN] = f[x1,...,xN]                          *
 *          *in*  M2                                                   *
 *                                                                     *
 *   Suppose M1 does not reduce to an application h[x1,...,xN].        *
 *   If we naively reduce the expression as above, first reducing      *
 *   the body M1 of f, then the body of g, then M2, we would get:      *
 *                                                                     *
 *        let M1' = eta(env,M1)                                        *
 *        in  *let* f[x1,...,xN] = M1'                                 *
 *            *in*  eta(env[g := f],M2)                                *
 *        end                                                          *
 *                                                                     *
 *   The problem with this is that M1 might have contained occurrences *
 *   of g.  Thus g may appear in M1'.  There are a number of ways to   *
 *   handle this:                                                      *
 *                                                                     *
 *    1) Once we perform an eta reduction on any function in a FIX, we *
 *       must go back and re-reduce any other functions of the FIX     *
 *       that we previously reduced;                                   *
 *    2) We do not go back to other functions in the FIX, but instead  *
 *       make a second pass over the output of eta.                    *
 *                                                                     *
 *   As (1) can lead to quadratic behaviour, we implemented (2).       *
 *                                                                     *
 *                                                                     *
 *   A final note: we recognize more than just                         *
 *         f[x1,...,xN] = g[x1,...,xN]                                 *
 *   as an eta reduction.  We regard the function definition           *
 *         f[x1,...,xN] = SELECT[1,v,g,g[x1,...,xN]]                   *
 *   as an eta redex as well, and so we reduce                         *
 *      eta(env,*let* f[x1,...,xN] = SELECT[i,v,g,g[x1,...,xN]]        *
 *              *in*  M1)                                              *
 *      --> SELECT(i,v,g,eta(env[f := g],M1))                          *
 *   This is implemented with the selectapp function below.            *
 *                                                                     *
 ***********************************************************************)

signature ETA = sig
    val eta : {function: CPS.function,
	       click: string -> unit} -> CPS.function
end (* signature ETA *)

structure Eta : ETA =
struct

local open CPS
      structure LV = LambdaVar
      structure Set = struct
	type intset = LV.Set.set ref
	fun new() = ref LV.Set.empty
	fun add set i = set := LV.Set.add(!set, i)
	fun mem set i =  LV.Set.member(!set, i)
	fun rmv set i = set := LV.Set.delete(!set, i)
      end

in

fun eta {function=(fkind,fvar,fargs,ctyl,cexp),
	 click} =
let

val debug = !Control.CG.debugcps (* false *)
fun debugprint s = if debug then Control.Print.say s else ()
fun debugflush() = if debug then Control.Print.flush() else ()

fun map1 f (a,b) = (f a, b)
fun member(i : LV.lvar, a::b) = i=a orelse member(i,b)
  | member(i,[]) = false
fun same(v::vl, (VAR w)::wl) = v=w andalso same(vl,wl)
  | same(nil,nil) = true
  | same _ = false
fun sameName(x,VAR y) = LV.sameName(x,y)
  | sameName(x,LABEL y) = LV.sameName(x,y)
  | sameName _ = ()
exception M2
val m : value LV.Tbl.hash_table = LV.Tbl.mkTable(32, M2)
val name = LV.Tbl.lookup m
fun rename(v0 as VAR v) = (rename(name v) handle M2 => v0)
  | rename(v0 as LABEL v) = (rename(name v) handle M2 => v0)
  | rename x = x
fun newname x = (sameName x; LV.Tbl.insert m x)

local val km : Set.intset =  Set.new()
in
fun addvt (v, CNTt) = Set.add km v
  | addvt _ = ()
fun addft (CONT, v, _, _, _) = Set.add km v
  | addft _ = ()
fun isCont v = Set.mem km v
end  (* local *)

val id = (fn x => x)
val doagain = ref false
val rec pass2 =
   fn RECORD(k,vl,w,e) => RECORD(k, map (map1 rename) vl, w, pass2 e)
    | SELECT(i,v,w,t,e) => SELECT(i, v, w, t, pass2 e)
    | OFFSET(i,v,w,e) => OFFSET(i, v, w, pass2 e)
    | APP(f,vl) => APP(rename f, map rename vl)
    | SWITCH(v,c,el) => SWITCH(v, c,map pass2 el)
    | BRANCH(i,vl,c,e1,e2) =>
          BRANCH(i, map rename vl, c, pass2 e1, pass2 e2)
    | LOOKER(i,vl,w,t,e) => LOOKER(i,map rename vl, w, t, pass2 e)
    | ARITH(i,vl,w,t,e) => ARITH(i,map rename vl, w, t, pass2 e)
    | PURE(i,vl,w,t,e) => PURE(i,map rename vl, w, t, pass2 e)
    | SETTER(i,vl,e) => SETTER(i,map rename vl, pass2 e)
    | RCC(k,l,p,vl,wtl,e) => RCC (k, l, p, map rename vl, wtl, pass2 e)
    | FIX(l,e) =>
	FIX(map (fn (fk,f,vl,cl,body) => (fk,f,vl,cl,pass2 body)) l,
	    pass2 e)

val rec reduce =
   fn RECORD(k,vl,w,e) => RECORD(k, map (map1 rename) vl, w, reduce e)
    | SELECT(i,v,w,t,e) => (addvt(w,t); SELECT(i, v, w, t, reduce e))
    | OFFSET(i,v,w,e) => OFFSET(i, v, w, reduce e)
    | APP(f,vl) => APP(rename f, map rename vl)
    | SWITCH(v,c,el) => SWITCH(v, c,map reduce el)
    | BRANCH(i,vl,c,e1,e2) =>
          BRANCH(i, map rename vl, c, reduce e1, reduce e2)
    | LOOKER(i,vl,w,t,e) =>
          (addvt(w, t); LOOKER(i,map rename vl, w, t, reduce e))
    | ARITH(i,vl,w,t,e) =>
          (addvt(w, t); ARITH(i,map rename vl, w, t, reduce e))
    | PURE(i,vl,w,t,e) =>
          (addvt(w, t); PURE(i,map rename vl, w, t, reduce e))
    | SETTER(i,vl,e) => SETTER(i,map rename vl, reduce e)
    | RCC (k,l,p,vl,wtl,e) =>
          (app addvt wtl; RCC (k, l, p, map rename vl, wtl, reduce e))
    | FIX(l,e) =>
       let val _ = app addft l
           fun eta_elim nil = (nil,id,false)
             | eta_elim((fk as NO_INLINE_INTO,f,vl,cl,body)::r) =
                 let val (r',h,leftover) = eta_elim r
                     val body' = reduce body
                  in ((fk,f,vl,cl,body')::r',h,true)
                 end
	     | eta_elim((fk,f,vl,cl,body)::r) =
                 let val (r',h,leftover) = eta_elim r
                     fun rightKind (VAR v | LABEL v) =
                           ((fk = CONT) = (isCont v))
                       | rightKind _ = false

                     fun selectapp(SELECT(i,VAR w,v,t,e)) =
                           (case selectapp e
                             of NONE => NONE
                              | SOME(h',u) =>
                                 if not (member(w,f::vl)) then
                                    SOME(fn ce => SELECT(i,VAR w,v,t,h' ce),u)
                                 else NONE)
                       | selectapp(APP(g,wl)) =
			   let val g' = rename g
                               val z = (case g' of VAR x => member(x,f::vl)
                                                 | LABEL x => member(x,f::vl)
                                                 | _ => false)

                            in if ((not z) andalso (same(vl,wl))
                                           andalso (rightKind g')) then
			         SOME(fn ce => ce, g')
			       else NONE
                           end
                       | selectapp _ = NONE
                     val _ = ListPair.app addvt (vl, cl)
		     val body' = reduce body
                 in  case selectapp(body')
		       of NONE => ((fk,f,vl,cl,body')::r',h,true)
			| SOME(h',u) => (if leftover then doagain := true
					 else ();
					 click "e";
					 newname(f,u);
					 (r',h' o h,leftover))
                 end
        in  case (eta_elim l)
             of ([],h,_) => h(reduce e)
   	      | (l',h,_) => h(FIX(l',reduce e))
        end

in  (* body of eta *)
    debugprint "Eta: ";
    debugflush();
    let val cexp' = reduce cexp
    in  debugprint "\n";
	debugflush();
	if not(!doagain) then (fkind, fvar, fargs, ctyl, cexp')
	else (debugprint "Eta: needed second pass\n";
	      debugflush();
	      (fkind, fvar, fargs, ctyl, pass2 cexp'))
    end
end (* fun eta *)

end (* toplevel local *)
end (* structure Eta *)

