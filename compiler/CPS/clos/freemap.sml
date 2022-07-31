(* freemap.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * NOTE: this code is not used anymore.
 *)

signature FREEMAP =
  sig
    val freevars: CPS.cexp -> CPS.lvar list
    val freemap : (CPS.lvar * CPS.lvar list -> unit)
		  -> (CPS.cexp -> CPS.lvar list)
    val cexp_freevars: (CPS.lvar->CPS.lvar list)
                       -> CPS.cexp -> CPS.lvar list
    val freemapClose : CPS.cexp -> ((CPS.lvar -> CPS.lvar list) *
			               (CPS.lvar -> bool) *
			               (CPS.lvar -> bool))
  end

structure FreeMap : FREEMAP = struct

local
  open CPS SortedList
  structure Intset = struct
    fun new() = ref IntRedBlackSet.empty
    fun add set i = set := IntRedBlackSet.add(!set, i)
    fun mem set i =  IntRedBlackSet.member(!set, i)
    fun rmv set i = set := IntRedBlackSet.delete(!set, i)
  end
in

fun clean l =
  let fun vars(l, VAR x :: rest) = vars(x::l, rest)
	| vars(l, _::rest) = vars(l,rest)
	| vars(l, nil) = uniq l
   in vars(nil, l)
  end

val enter = fn (VAR x,y) => enter(x,y) | (_,y) => y
val error = ErrorMsg.impossible


(*
 * freevars
 *    -- Given a CPS expression, the function "freevars" does a top-down
 *       traverse on the CPS expression and returns the set of free variables
 *       in the CPS expression.
 *)
val rec freevars =
  fn APP(v,args) => enter(v,clean args)
   | SWITCH(v,c,l) => enter(v,foldmerge (map freevars l))
   | RECORD(_,l,w,ce) => merge(clean (map #1 l), rmv(w, freevars ce))
   | SELECT(_,v,w,_,ce) => enter(v, rmv(w, freevars ce))
   | OFFSET(_,v,w,ce) => enter(v, rmv(w, freevars ce))
   | SETTER(_,vl,e) => merge(clean vl, freevars e)
   | (LOOKER(_,vl,w,_,e) |
      ARITH(_,vl,w,_,e) |
      PURE(_,vl,w,_,e) |
      RCC(_,vl,w,_,e)) => merge(clean vl, rmv(w, freevars e))
   | BRANCH(_,vl,c,e1,e2) => merge(clean vl, merge(freevars e1, freevars e2))
   | FIX(fl,e) =>
	let fun g(_,f,vl,_,ce) = difference(freevars ce, uniq vl)
	 in difference(foldmerge (freevars e :: map g fl), uniq(map #2 fl))
	end

(*
 * freemap
 *    -- This function is used only in those post-globalfix phases. For each
 *       newly bound lvar in the CPS expression, a set of lvars which live
 *       beyond this lvar are identified. A function is applied to this pair
 *       then.
 *)
fun freemap add =
let (* Doesn't apply "add" to the rebound variables of a branch *)
    fun setvars (w,free) = let val g = rmv(w,free)
			    in add(w,g); g
			   end
    val rec freevars =
	 fn APP(v,args) => enter(v,clean args)
	  | SWITCH(v,c,l) => enter(v,foldmerge (map freevars l))
	  | RECORD(_,l,w,ce) => merge(clean (map #1 l),setvars(w,freevars ce))
	  | SELECT(_,v,w,_,ce) => enter(v, setvars(w, freevars ce))
	  | OFFSET(_,v,w,ce) => enter(v, setvars(w, freevars ce))
	  | SETTER(_,vl,e) => merge(clean vl, freevars e)
	  | (LOOKER(_,vl,w,_,e) |
	     ARITH(_,vl,w,_,e) |
	     PURE(_,vl,w,_,e) |
	     RCC(_,vl,w,_,e)) => merge(clean vl, setvars(w, freevars e))
	  | BRANCH(_,vl,c,e1,e2) =>
		let val s = merge(clean vl,merge(freevars e1, freevars e2))
                 in add(c,s); s
                end
	  | FIX _ => error "FIX in Freemap.freemap"
 in freevars
end

(*
 * cexp_freevars
 *	-- To be used in conjunction with FreeMap.freemap.
 *	   Consequently, raises an exception for FIX. Only used
 *         in those post-globalfix phases.
 *)
fun cexp_freevars lookup cexp =
    let val rec f =
	fn RECORD(_,vl,w,_) => merge(clean(map #1 vl), lookup w)
	 | SELECT(_,v,w,_,_) => enter(v, lookup w)
	 | OFFSET(_,v,w,_) => enter(v, lookup w)
	 | APP(f,vl) =>  clean (f::vl)
	 | FIX _ => error "FIX in Freemap.cexp_freevars"
	 | SWITCH(v,c,cl) =>
	       enter(v, foldmerge (map f cl))
         | SETTER(_,vl,e) => merge(clean vl, f e)
	 | LOOKER(_,vl,w,_,e) => merge(clean vl, lookup w)
	 | ARITH(_,vl,w,_,e) => merge(clean vl, lookup w)
	 | PURE(_,vl,w,_,e) => merge(clean vl, lookup w)
	 | RCC(_,vl,w,_,e) => merge(clean vl, lookup w)
	 | BRANCH(_,vl,c,e1,e2) => merge(clean vl,merge(f e1, f e2))
    in f cexp
    end


(*
 * freemapClose
 *    -- Produces a free variable mapping at each function binding.
 *       The mapping includes the functions bound at the FIX, but
 *       not the arguments of the function.
 *       Only used in the closure phase.
 *)
fun freemapClose ce =
let exception Freemap
    val vars : lvar list IntHashTable.hash_table =
	IntHashTable.mkTable(32, Freemap)
    val escapes = Intset.new()
    val escapesP = Intset.mem escapes
    fun escapesM(VAR v) = Intset.add escapes v
      | escapesM _ = ()
    val known = Intset.new()
    val knownM = Intset.add known
    val rec freevars =
	 fn FIX(l,ce) =>
		let val functions = uniq(map #2 l)
		    (* MUST be done in this order due to side-effects *)
		    val freeb = freevars ce
		    val freel =
			foldr (fn ((_,v,args,_,body),freel) =>
			       (let val l = remove(uniq args,freevars body)
				in  IntHashTable.insert vars (v,l);
				    l::freel
				end))
			      [] l
		in  app (fn v => if escapesP v then () else knownM v)
		        functions;
		    remove(functions,foldmerge(freeb::freel))
		end
	  | APP(v,args) => (app escapesM args;
			    enter(v, clean args))
	  | SWITCH(v,c,l) => foldmerge (clean[v]::(map freevars l))
	  | RECORD(_,l,w,ce) => (app (escapesM o #1) l;
			       merge(clean (map #1 l), rmv(w,freevars ce)))
	  | SELECT(_,v,w,_,ce) => enter(v,rmv(w,freevars ce))
	  | OFFSET(_,v,w,ce) => enter(v,rmv(w,freevars ce))
	  | LOOKER(_,vl,w,_,ce) => (app escapesM vl;
				 merge(clean vl, rmv(w,freevars ce)))
	  | ARITH(_,vl,w,_,ce) => (app escapesM vl;
				merge(clean vl, rmv(w,freevars ce)))
	  | PURE(_,vl,w,_,ce) => (app escapesM vl;
			       merge(clean vl, rmv(w,freevars ce)))
	  | SETTER(_,vl,ce) => (app escapesM vl; merge(clean vl, freevars ce))
	  | RCC(_,vl,w,_,ce) => (app escapesM vl;
				 merge (clean vl, rmv(w, freevars ce)))
	  | BRANCH(_,vl,c,e1,e2) =>
	          (app escapesM vl;
		   merge(clean vl,merge(freevars e1, freevars e2)))
in  freevars ce;
    (IntHashTable.lookup vars, Intset.mem escapes, Intset.mem known)
end

(* temporary, for debugging
val phase = Stats.doPhase(Stats.makephase "Compiler 078 Freemap")
val freemap = phase freemap
val freemapClose = phase freemapClose
val freevars = phase freevars
*)

end (* local *)

end (* structure FreeMap *)


