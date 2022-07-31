(* flatten.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature FLATTEN =
  sig

    val flatten : {function: CPS.function, click: string -> unit} -> CPS.function

  end (* signature FLATTEN *)

functor Flatten(MachSpec : MACH_SPEC) : FLATTEN =
struct

local open CPS
      structure LT = LtyExtern
      structure LV = LambdaVar
      structure CG = Control.CG

in

val say = Control.Print.say
fun bug s = ErrorMsg.impossible ("Flatten: " ^ s)

datatype arity = BOT
	       | UNK  (* an arg seen that isn't a known record *)
	       | COUNT of int * bool (* int is # of record fields;
		                        bool is whether any arguments
                                        were unknown records *)
	       | TOP

datatype info = FNinfo of {arity: arity list ref,
			   alias: lvar option ref,
			   escape: bool ref}
	      | ARGinfo of int ref (* the highest-numbered field selected *)
	      | RECinfo of int (* number of fields *)
	      | MISCinfo

fun flatten {function=(fkind,fvar,fargs,ctyl,cexp), click} =
let

val clicks = ref 0

val maxfree = MachSpec.numRegs
val debug = !Control.CG.debugcps (* false *)
fun debugprint s = if debug then Control.Print.say(s) else ()
fun debugflush() = if debug then Control.Print.flush() else ()

(* Note that maxfree has already been reduced by 1 (in CPScomp)
   on most machines to allow for an arithtemp *)
val maxregs = maxfree - MachSpec.numCalleeSaves

local exception UsageMap
      val m: info LV.Tbl.hash_table = LV.Tbl.mkTable(128, UsageMap)
      val umap = LV.Tbl.lookup m
in
    fun get i = umap i handle UsageMap => MISCinfo
    val enter = LV.Tbl.insert m
end

fun select(VAR v,i) =
        (case get v
	  of ARGinfo(biggestSEL as ref j) => biggestSEL := Int.max(i,j)
	   | _ => ())
  | select(LABEL v, i) = select(VAR v, i)
  | select _ = ()

fun escape(VAR v) = (case get v
                      of FNinfo{escape=r,...} => r := true
		       | _ => ())
  | escape(LABEL v) = escape(VAR v)
  | escape _ = ()

fun field(v, SELp(i,_)) = select(v,i)
  | field(v, _) = escape v

val botlist = if !CG.flattenargs then map (fn _ => BOT)
				 else map (fn _ => TOP)

fun enterFN (_,f,vl,_,cexp) =
      (enter(f,FNinfo{arity=ref(botlist vl),alias=ref NONE,escape=ref false});
       app (fn v => enter(v,ARGinfo(ref ~1))) vl)

local exception Found
in
fun findFetch(v,k) body =
      (* find whether field k of variable v is guaranteed to exist *)
  let fun f (RECORD(_, fields,_,e)) = (app g fields; f e)
	| f (SELECT(i,VAR v',w,_,e)) =
               if v=v' andalso i=k then raise Found else f e
	| f (SELECT(_,_,_,_,e)) = f e
	| f (OFFSET(_,_,_,e)) = f e
	| f (FIX(_,e)) = f e
	| f (BRANCH(_,_,_,e1,e2)) = findFetch(v,k) e1 andalso findFetch(v,k) e2
	| f (LOOKER(_,_,_,_,e)) = f e
	| f (SETTER(_,_,e)) = f e
	| f (ARITH(_,_,_,_,e)) = f e
	| f (PURE(_,_,_,_,e)) = f e
	| f (RCC(_,_,_,_,_,e)) = f e
	| f (SWITCH(_,_,el)) = not(List.exists (not o findFetch(v,k)) el)
	| f _ = false
      and g (VAR v',SELp(i,_)) = if v=v' andalso i=k then raise Found else ()
	| g _  = ()
  in  f body handle Found => true
  end
end (* local *)

fun checkFlatten(_,f,vl,_,body) =
 case get f
  of FNinfo{arity as ref al, alias, escape} =>
  let fun loop(v::vl,a::al,headroom) =
	   (case (a,get v)
	      of (COUNT(c,some_non_record_actual),ARGinfo(ref j)) =>
		     if j > ~1  (* exists a select of the formal parameter *)
		        andalso headroom-(c-1) >= 0
		        andalso
			(not (some_non_record_actual orelse !escape)
			 orelse !CG.extraflatten
			        andalso j=c-1 andalso findFetch(v,j) body)
		     then a::loop(vl,al,headroom-(c-1))
		     else TOP::loop(vl,al,headroom)
	       | _ =>
		     TOP::loop(vl,al,headroom))
	| loop _ = nil

      val a' = loop(vl,al,maxregs-1-length(al))
   in arity := a';
      if List.exists (fn COUNT _ => true | _ => false) a'
	  then (alias := SOME(LV.dupLvar f); click "F"; clicks := !clicks+1)
          else ()
  end
   | _ => () (* impossible *)


(**************************************************************************)
(* pass1: gather usage information on the variables in a cps expression.  *)
(**************************************************************************)
val rec pass1 =
 fn RECORD(_,vl,w,e) => (enter(w,RECinfo (length vl)); app field vl; pass1 e)
  | SELECT (i,v,w,_,e) => (select(v,i); pass1 e)
  | OFFSET (i,v,w,e) => (escape v; pass1 e)
  | SWITCH(v,c,el) => (escape v; app pass1 el)
  | BRANCH(i,vl,c,e1,e2) => (app escape vl; pass1 e1; pass1 e2)
  | SETTER(i,vl,e) => (app escape vl; pass1 e)
  | LOOKER(i,vl,w,_,e) => (app escape vl; pass1 e)
  | ARITH(i,vl,w,_,e) => (app escape vl; pass1 e)
  | PURE(i,vl,w,_,e) => (app escape vl; pass1 e)
  | RCC(_,_,_,vl,wtl,e) => (app escape vl; pass1 e)
  | APP(VAR f, vl) =>
      let fun loop (t::r,vl0 as (VAR v)::vl,n) =
		    (case (t,get v)
		      of (BOT,RECinfo sz) =>
			     loop(COUNT(sz,false)::r,vl0,n)
		       | (BOT,_) => UNK::loop(r,vl,n+1)
		       | (UNK,RECinfo sz) =>
			     loop(COUNT(sz,true)::r,vl0,n)
		       | (UNK,_) => UNK::loop(r,vl,n+1)
		       | (COUNT(a,_),RECinfo sz) =>
			     if a = sz then t::loop(r,vl,n+1)
			     else TOP::loop(r,vl,n+1)
		       | (COUNT(a,_),_) =>
			     COUNT(a,true)::loop(r,vl,n+1)
		       | _ => TOP::loop(r,vl,n+1))
	    | loop (_::r, _::vl,n) = TOP::loop(r,vl,n+1)
	    | loop _ = nil
      in  app escape vl;
	  case get f
	    of FNinfo{arity as ref al,...} => arity := loop(al,vl,0)
	     | _ => ()
      end
  | APP(f, vl) => app escape vl
  | FIX(l, e) => (app enterFN l;
		  app (fn (_,_,_,_,body) => pass1 body) l;
		  pass1 e;
		  app checkFlatten l)

val rec reduce =
  fn RECORD (k,vl,w,e) => RECORD(k,vl, w, reduce e)
   | SELECT(i,v,w,t,e) => SELECT(i,v,w,t,reduce e)
   | OFFSET(i,v,w,e) => OFFSET(i,v,w,reduce e)
   | SWITCH(v,c,el) => SWITCH(v,c,map reduce el)
   | LOOKER(i,vl,w,t,e) => LOOKER(i,vl,w,t,reduce e)
   | SETTER(i,vl,e) => SETTER(i,vl,reduce e)
   | ARITH(i,vl,w,t,e) => ARITH(i,vl,w,t,reduce e)
   | PURE(i,vl,w,t,e) => PURE(i,vl,w,t,reduce e)
   | RCC(k,l,p,vl,wtl,e) => RCC(k,l,p,vl,wtl,reduce e)
   | BRANCH(i,vl,c,e1,e2) => BRANCH(i,vl,c,reduce e1,reduce e2)
   | APP(f as VAR fv, vl) =>
        (case get fv
	  of FNinfo{arity=ref al,alias=ref(SOME f'),...} =>
	      let fun loop(COUNT(cnt,_)::r,v::vl,args) =
		  let fun g(i,args) =
			  if i=cnt then loop(r,vl,args)
			  else let val z = LV.mkLvar()
			       in SELECT(i, v, z, CPSUtil.BOGt, g(i+1,(VAR z)::args))
			       end
		  in  g(0,args)
		  end
		    | loop(_::r,v::vl,args) = loop(r,vl,v::args)
		    | loop(_,_,args) = APP(VAR f', rev args)
	      in loop(al,vl,nil)
	      end
	   | _ => APP(f,vl))
   | APP(f,vl) => APP(f,vl)
   | FIX(l,e) =>
      let fun vars (0, l, l') = (l, l')
	    | vars (i, l, l') = vars(i-1, LV.mkLvar()::l, CPSUtil.BOGt::l')
	  fun newargs(COUNT(j,_) :: r,v::vl,_::cl) =
		let val (new,ncl) = vars(j, nil, nil)
		    val (vl',cl',bt') = newargs(r,vl,cl)
		    fun bodytransform body =
			     RECORD(RK_RECORD,
				    map (fn x =>(VAR x, OFFp 0)) new,
				    v,body)
		in  (new @ vl', ncl @ cl',bodytransform o bt')
		end
	    | newargs(_::r,v::vl,ct::cl) =
		let val (vl',cl',bt') = newargs(r,vl,cl)
		in  (v::vl',ct::cl',bt')
		end
	    | newargs _ = ([],[],fn b=>b)
	  fun process_args((fdef as (fk,f,vl,cl,body))::rest) =
	      (case get f
                of FNinfo{arity=ref al,alias=ref(SOME f'),...} =>
		    let val (nargs,ncl,bt) = newargs(al,vl,cl)
			val wl = map LV.dupLvar vl
		    in
			(fk,f,wl,cl,APP(VAR f,map VAR wl))::
			(fk,f',nargs,ncl,bt body) :: process_args rest
		    end
	         | _ => fdef :: process_args rest)
	    | process_args nil = nil
	  fun reduce_body (fk,f,vl,cl,body) = (fk,f,vl,cl,reduce body)
      in  FIX(map reduce_body (process_args l), reduce e)
      end

 fun fprint (function, s : string) =
     (say "\n"; say s; say "\n \n"; PPCps.printcps0 function)

   val _ = (debugprint "Flatten: ";  debugflush())
   val _ = if debug then fprint ((fkind, fvar, fargs, ctyl, cexp), "Before flatten:") else ()
   val _ = pass1 cexp;
   val cexp' = if !clicks>0 then reduce cexp else cexp
   val _ = if (debug) then
if (!clicks>0) then fprint ((fkind, fvar, fargs, ctyl, cexp'), "After flatten:")
               else say "No flattening this time.\n"
	     else ()
   val _ = debugprint "\n"
in (fkind, fvar, fargs, ctyl, cexp')
end

end (* toplevel local *)
end (* functor Flatten *)
