(* contract.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(*
Transformations performed by the contracter:

TRANSFORMATION:                       Click:   Compiler.Control.CG flag:
------------------------------------------------------------------------
Inlining functions that are used once   e      betacontract
Cascaded inlining of functions          q
The IF-idiom                            E      ifidiom
Unify BRANCHs                           z      branchfold
Constant folding:
 SELECTs from known RECORDs             d
 Handler operations                    ijk     handlerfold
 SWITCH expressions                     h      switchopt
 ARITH expressions              FGHIJKLMNOPQX  arithopt
 PURE expressions          RSTUVWYZ0123456789  arithopt
 BRANCH expressions                   nopvw    comparefold

Dead variable elimination:         [down,up]       [down,up]
 RECORDs                              [b,B1,B2]    [deadvars,deadup]
 SELECTs                              [c,s]        [deadvars,deadup]
 Functions                            [g,f]
 LOOKERs                              [m,*]        [deadvars,deadup]
 PUREs                                [m,*]        [deadvars,deadup]
 Arguments                            [D, ]        [dropargs, ]

Conversion Primops:
 testu					U(n)
 test					T(n)
 copy					C(n)
 extend					X(n)
 trunc					R(n)
*)

signature CONTRACT = sig
  val contract : {function: CPS.function,
                  click: string -> unit,
                  last: bool,
                  size: int ref}
                  -> CPS.function
end (* signature CONTRACT *)

structure Contract : CONTRACT =
struct

open CPS
structure LB = LtyBasic
structure LV = LambdaVar
structure CA = ConstArith

structure CG = Control.CG

val say = Control.Print.say
fun bug s = ErrorMsg.impossible ("Contract: " ^ s)

fun inc (ri as ref i) = (ri := i+1)
fun dec (ri as ref i) = (ri := i-1)

(* integer types/values *)
local
  val tt = {sz = Target.defaultIntSz, tag = true}
  fun bt sz = {sz = sz, tag = false}
in
val tagIntTy = NUMt tt
fun tagInt n = NUM{ival = n, ty = tt}
fun tagInt' n = tagInt(IntInf.fromInt n)	(* will be unused *)
fun boxIntTy sz = NUMt(bt sz)
fun boxInt {ival, ty} = NUM{ival = ival, ty = bt ty}
end

(* get the size of an integer operation *)
fun sizeOfKind (P.INT sz) = sz
  | sizeOfKind (P.UINT sz) = sz
  | sizeOfKind (P.FLOAT _) = bug "sizeOfKind(FLOAT _)"

exception ConstFold

fun map1 f (a,b) = (f a, b)

fun sameName (x, VAR y) = LV.sameName(x,y)
  | sameName (x, LABEL y) = LV.sameName(x,y)
  | sameName _ = ()

fun complain(t1,t2,s) =
  (say (s^"  ____ Type conflicting while contractions =====> \n    ");
   say (LB.lt_print t1); say "\n and   \n    "; say (LB.lt_print t2);
   say "\n \n";
   say "_____________________________________________________ \n")

fun checklty s (t1,t2) =  ()
(*  -- LT.INT, etc. ?
  let fun g (LT.INT, LT.INT) = ()
        | g (LT.INT32, LT.INT32) = ()
        | g (LT.BOOL, LT.BOOL) = ()
        | g (LT.INT, LT.BOOL) = ()
        | g (LT.BOOL, LT.INT) = ()
        | g (LT.REAL, LT.REAL) = ()
        | g (LT.SRCONT, LT.SRCONT) = ()
        | g (LT.BOXED, LT.BOXED) = ()
        | g (LT.RBOXED, LT.RBOXED) = ()
        | g (LT.INT, LT.RECORD nil) = ()
        | g (LT.RECORD nil, LT.INT) = ()
        | g (LT.BOXED, LT.RBOXED) = ()         (* this is temporary *)
        | g (LT.RBOXED, LT.BOXED) = ()         (* this is temporary *)
        | g (LT.ARROW(t1,t2),LT.ARROW(t1',t2')) =
             (g(LT.out t1,LT.out t1'); g(LT.out t2, LT.out t2'))
        | g (LT.RECORD l1,LT.RECORD l2) =
             ListPair.appEq g (map LT.out l1, map LT.out l2)
        | g (LT.CONT t1,LT.CONT t2) = g(LT.out t1,LT.out t2)
        | g (t1,t2) = complain(LT.inj t1, LT.inj t2,"CTR *** "^s)
  in  g(LT.out t1, LT.out t2)
  end
*)

fun equalUptoAlpha(ce1,ce2) =
  let fun equ pairs =
        let fun same(VAR a, VAR b) =
	          let fun look((x,y)::rest) = a=x andalso b=y orelse look rest
		        | look nil = false
		  in  a=b orelse look pairs
		  end
              | same(LABEL a, LABEL b) = same(VAR a, VAR b)
              | same(NUM i, NUM j) = (#ty i = #ty j) andalso (#ival i = #ival j)
              | same(REAL a, REAL b) = (#ty a = #ty b) andalso RealLit.same(#rval a, #rval b)
              | same(STRING a, STRING b) = a=b
	      | same(a,b) = false
	    fun sameField ((a, ap : accesspath), (b, bp)) = (ap = bp) andalso same(a, b)
	    fun samewith p = equ (p::pairs)
	    fun samewith' args =
		equ (ListPair.foldr (fn ((w, _), (w', _), l) => (w,w')::l)
				    pairs args)
            fun all2 f (e::r,e'::r') = f(e,e') andalso all2 f (r,r')
              | all2 f (nil,nil) = true
              | all2 f _ = false
            val rec sameexp =
	     fn (SELECT(i,v,w,_,e),SELECT(i',v',w',_,e')) =>
		   i=i' andalso same(v,v') andalso samewith(w,w') (e,e')
              | (RECORD(k,vl,w,e),RECORD(k',vl',w',e')) =>
		   (k = k')
		   andalso ListPair.allEq sameField (vl, vl')
                   andalso samewith (w,w') (e,e')
              | (OFFSET(i,v,w,e),OFFSET(i',v',w',e')) =>
		   i=i' andalso same(v,v') andalso samewith(w,w') (e,e')
              | (SWITCH(v,c,el),SWITCH(v',c',el')) =>
		   same(v,v') andalso all2 (samewith(c,c')) (el,el')
	      | (APP(f,vl),APP(f',vl')) =>
                   same(f,f') andalso all2 same (vl,vl')
              | (FIX(l,e),FIX(l',e')) => (* punt! *) false
	      | (BRANCH(i,vl,c,e1,e2),BRANCH(i',vl',c',e1',e2')) =>
		   i=i' andalso all2 same (vl,vl')
		   andalso samewith(c,c') (e1,e1')
		   andalso samewith(c,c') (e2,e2')
	      | (LOOKER(i,vl,w,_,e),LOOKER(i',vl',w',_,e')) =>
		   i=i' andalso all2 same (vl,vl') andalso samewith(w,w')(e,e')
	      | (SETTER(i,vl,e),SETTER(i',vl',e')) =>
		   i=i' andalso all2 same (vl,vl') andalso sameexp(e,e')
	      | (ARITH(i,vl,w,_,e),ARITH(i',vl',w',_,e')) =>
		   i=i' andalso all2 same (vl,vl') andalso samewith(w,w')(e,e')
	      | (PURE(i,vl,w,_,e),PURE(i',vl',w',_,e')) =>
		   i=i' andalso all2 same (vl,vl') andalso samewith(w,w')(e,e')
	      | (RCC(k,l,p,vl,wtl,e),RCC(k',l',p',vl',wtl',e')) =>
		(* We don't need to compare protocol info:  The protocols are
		 * the same iff the functions and arguments are the same. *)
		k = k' andalso l = l' andalso
                all2 same (vl,vl') andalso samewith'(wtl,wtl')(e,e')
	      | _ => false
        in  sameexp
        end
  in  equ nil (ce1,ce2)
  end

datatype info = datatype ContractPrim.info
(*
  = FNinfo of {
	args: lvar list,
	body : cexp option ref,
	specialuse: int ref option ref,
	liveargs : bool list option ref
      }
  | RECinfo of record_kind * (value * accesspath) list
  | SELinfo of int * value * cty
  | OFFinfo of int * value
  | WRPinfo of P.numkind * value				(* P.wrap of a value *)
  | IFIDIOMinfo of {body : (lvar * cexp * cexp) option ref}
  | MISCinfo of cty
*)
datatype result = datatype ContractPrim.result

fun contract {function=(fkind,fvar,fargs,ctyl,cexp), click, last, size=cpssize} =
let

val deadup = !Control.CG.deadup
val CGbetacontract = !Control.CG.betacontract
val debug = !Control.CG.debugcps (* false *)
fun debugprint s = if debug then Control.Print.say(s) else ()
fun debugflush() = if debug then Control.Print.flush() else ()

local exception UsageMap
in  val m : {info: info, used : int ref, called : int ref}
		LV.Tbl.hash_table =
	        LV.Tbl.mkTable(128, UsageMap)
    val get = fn i => LV.Tbl.lookup m i
	        handle UsageMap => bug ("UsageMap on " ^ LV.prLvar i)
    val enter = LV.Tbl.insert m
    fun rmv i = ignore (LV.Tbl.remove m i) handle _ => ()
end

fun use (VAR v) = inc(#used(get v))
  | use (LABEL v) = inc(#used(get v))
  | use _ = ()
fun use_less (VAR v) = if deadup then dec(#used(get v)) else ()
  | use_less (LABEL v) = if deadup then dec(#used(get v)) else ()
  | use_less _ = ()
fun usedOnce v = !(#used(get v)) = 1
fun used v = !(#used(get v)) > 0

fun call(VAR v) =
    let val {called,used,...} = get v
    in  inc called; inc used
    end
  | call(LABEL v) = call(VAR v)
  | call _ = ()
fun call_less(VAR v) = if deadup then
                         let val {called,used,...} = get v
			 in  dec called; dec used
			 end
		       else ()
  | call_less(LABEL v) = call_less(VAR v)
  | call_less _ = ()
fun call_and_clobber(VAR v) =
    let val {called,used,info} = get v
    in  inc called; inc used;
	case info
	  of FNinfo{body,...} => body := NONE
	   | _ => ()
    end
  | call_and_clobber(LABEL v) = call(VAR v)
  | call_and_clobber _ = ()

(* functions to add information about variables to the map *)
fun enterREC(w,kind,vl) = enter(w,{info=RECinfo(kind,vl), called=ref 0,used=ref 0})
fun enterMISC (w,ct) = enter(w,{info=MISCinfo ct, called=ref 0, used=ref 0})
val miscBOG = MISCinfo CPSUtil.BOGt
fun enterMISC0 w = enter(w,{info=miscBOG, called=ref 0, used=ref 0})
fun enterWRP (w, kind, u) = enter(w, {info=WRPinfo(kind, u), called=ref 0, used=ref 0})

fun enterFN (_,f,vl,cl,cexp) =
      (enter(f,{called=ref 0,used=ref 0,
		info=FNinfo{args=vl,
			    body=ref(if CGbetacontract then SOME cexp
				     else NONE),
			    specialuse=ref NONE,
			    liveargs=ref NONE}});
       ListPair.appEq enterMISC (vl, cl))

(* for `w = CAST arg`, we treat it as identity if arg is interesting *)
fun enterCAST (w, arg) = (case get arg
       of {info=MISCinfo _, ...} => enterMISC0 w
	| {info, ...} => enter(w, {info=info, called=ref 0, used=ref 0})
      (* end case *))

(*********************************************************************
   checkFunction: used by pass1(FIX ...) to decide
   (1) whether a function will be inlined for the if idiom;
   (2) whether a function will drop some arguments.
 *********************************************************************)
fun checkFunction(_,f,vl,_,_) =
 (case get f
    of {called=ref 2,used=ref 2,
	info=FNinfo{specialuse=ref(SOME(ref 1)),
		    body as ref(SOME(BRANCH(_,_,c,a,b))),...},...} =>
	   if not (!CG.ifidiom) then body:=NONE
	   else (* NOTE: remapping f *)
	        enter(f,{info=IFIDIOMinfo{body=ref(SOME(c,a,b))},
			 called=ref 2, used=ref 2})
     | {called=ref c,used=ref u,info=FNinfo{liveargs,...}} =>
	   if u<>c (* escaping function *)
	       orelse not(!CG.dropargs) then ()
	   else liveargs := SOME(map used vl)
     | _  => ())


(**************************************************************************)
(* pass1: gather usage information on the variables in a cps expression,  *)
(* and make a few decisions about whether to inline functions:            *)
(*        (1) If Idiom                                                    *)
(*        (2) NO_INLINE_INTO                                              *)
(**************************************************************************)
fun pass1 cexp = let
      fun p1 noInline cexp = (case cexp
	     of RECORD(kind,vl,w,e) => (
		  enterREC(w,kind,vl); app (use o #1) vl; p1 noInline e)
	      | SELECT (i,v,w,ct,e) => (
		  enter(w,{info=SELinfo(i,v,ct), called=ref 0, used=ref 0});
		  use v; p1 noInline e)
	      | OFFSET (i,v,w,e) => (
		  enter(w,{info=OFFinfo(i,v), called=ref 0, used=ref 0});
		  use v; p1 noInline e)
	      | APP(f, vl) => (
		  if noInline
		    then call_and_clobber f
		    else call f;
		  app use vl)
	      | FIX(l, e) => (
		  app enterFN l;
		  app (fn (NO_INLINE_INTO,_,_,_,body) => p1 (not last) body
			| (_,_,_,_,body) => p1 noInline body) l;
		  p1 noInline e;
		  app checkFunction l)
	      | SWITCH(v,c,el) => (
		  use v; enterMISC0 c; app (p1 noInline) el)
	      | BRANCH(i, vl, c, e1, e2) => (
		(* check for "if idiom" *)
		  case (e1, e2)
		   of (APP(VAR f1, [NUM{ival = 1, ...}]), APP(VAR f2, [NUM{ival = 0, ...}])) => (
			case get f1
			 of { info=FNinfo{
				  body=ref(SOME(BRANCH(
				      P.CMP{oper=P.NEQ,...},
				      [NUM{ival = 0, ...}, VAR w2],
				      _,_,_))),
			          args=[w1], specialuse, ...
				},
			      ...
			    } => (* Handle IF IDIOM *)
			      if f1=f2 andalso w1=w2
				then let
				  val {used,...} = get w1
				  in
				    specialuse := SOME used
				  end
				else ()
			  | _ => ()
			(* end case *))
		    | _ => ()
		  (* end case *);
		  app use vl; enterMISC0 c; p1 noInline e1; p1 noInline e2)
	      | SETTER(i,vl,e) => (
		  app use vl; p1 noInline e)
	      | LOOKER(i,vl,w,_,e) => (
		  app use vl; enterMISC0 w; p1 noInline e)
	      | ARITH(i,vl,w,_,e) => (
		  app use vl; enterMISC0 w; p1 noInline e)
	      | PURE(P.CAST, [VAR x], w, _, e) => (
		  inc(#used(get x)); enterCAST(w, x); p1 noInline e)
	      | PURE(P.WRAP kind, [u], w, _, e) => (
		  use u; enterWRP(w, kind, u); p1 noInline e)
	      | PURE(i,vl,w,_,e) => (
		  app use vl; enterMISC0 w; p1 noInline e)
	      | RCC(k,l,p,vl,wtl,e) => (
		  app use vl; app (enterMISC0 o #1) wtl; p1 noInline e)
	    (* end case *))
      in
	p1 false cexp
      end
(*DEBUG*)handle ex => (say "****** pass1 ******\n"; PPCps.prcps cexp; raise ex)

local
  exception Beta
  val m2 : value LV.Tbl.hash_table = LV.Tbl.mkTable(32, Beta)
  val mapm2 = LV.Tbl.lookup m2
in

fun ren(v0 as VAR v) = (ren(mapm2 v) handle Beta => v0)
  | ren(v0 as LABEL v) = (ren(mapm2 v) handle Beta => v0)
  | ren x = x

fun newname (vw as (v,w)) = let
      val {used=ref u, called=ref c, ...} = get v
      fun f (VAR w') = let
	    val {used, called, ...} = get w'
	    in
	      used := !used + u; called := !called + c
	    end
	| f (LABEL w') = f (VAR w')
	| f _ = ()
      in
	if deadup then f (ren w) else ();
	rmv v; sameName vw; LV.Tbl.insert m2 vw
      end

end (* local *)

fun newnames(v::vl, w::wl) = (newname(v,w); newnames(vl,wl))
  | newnames _ = ()


(*********************************************************************)
(* drop_body: used when dropping a function to adjust the usage      *)
(* counts of the free variables of the function.                     *)
(* This should match up closely with pass1 above.                    *)
(*********************************************************************)
local val use_less = use_less o ren
      val call_less = call_less o ren
in
fun drop_body (APP(f,vl)) = (call_less f; app use_less vl)
  | drop_body (SELECT(_,v,_,_,e)) = (use_less v; drop_body e)
  | drop_body (OFFSET(_,v,_,e)) = (use_less v; drop_body e)
  | drop_body (RECORD(_,vl,_,e)) = (app (use_less o #1) vl; drop_body e)
  | drop_body (FIX(l,e)) = (app (drop_body o #5) l; drop_body e)
  | drop_body (SWITCH(v,_,el)) = (use_less v; app drop_body el)
  | drop_body (BRANCH(_,vl,_,e1,e2)) = (app use_less vl; drop_body e1; drop_body e2)
  | drop_body (SETTER(_,vl,e)) = (app use_less vl; drop_body e)
  | drop_body (LOOKER(_,vl,_,_,e)) = (app use_less vl; drop_body e)
  | drop_body (ARITH(_,vl,_,_,e)) = (app use_less vl; drop_body e)
  | drop_body (PURE(_,vl,_,_,e)) = (app use_less vl; drop_body e)
  | drop_body (RCC(_,_,_,vl,_,e)) = (app use_less vl; drop_body e)
end (* local *)

fun setter (P.UPDATE, [_, _, NUM{ty={tag=true, ...}, ...}]) = P.UNBOXEDUPDATE
  | setter (P.UPDATE, _) = P.UPDATE
  | setter (P.ASSIGN, [_, NUM{ty={tag=true, ...}, ...}]) = P.UNBOXEDASSIGN
  | setter (i, _) = i

fun sameLvar(lvar, VAR lv) = lv = lvar
  | sameLvar _ = false

(* precondition for fusing fixed-size conversions *)
fun cvtPreCondition (n:int, n2, x, v2) =
      n = n2 andalso usedOnce x andalso sameLvar(x, ren v2)
(* precondition for fusing IntInf conversions *)
fun cvtInfPrecondition (x, v2) =
      usedOnce x andalso sameLvar(x, ren v2)
(* precondition for fusing a TEST_INF conversion to produce a TEST.  If the source is
 * 64-bits and we are on a 32-bit target, then we cannot do the fusion, since we
 * do not have a handle on the Core function that actually does the 64-bit
 * test.
 *)
fun test64On32 64 = Target.is64
  | test64On32 _ = true

(* contraction for primops *)
val arith = ContractPrim.arith
val pure = ContractPrim.pure get
val branch = ContractPrim.branch get

fun reduce cexp = (g NONE cexp)

and g hdlr = let
      fun g' cexp = (case cexp
	     of RECORD (kind,vl,w,e) => let
		  val {used,...} = get w
		  val vl' = map (map1 ren) vl
		  in
		    if !used=0 andalso !CG.deadvars
		      then (click "b"; app (use_less o #1) vl'; g' e)
		      else let
		      (* Check to see if this record is recreating an existing record.
		       * We need to be careful that the existing record has the same
		       * kind as this record (as well as the same size and content).
		       *)
			fun objInfo (VAR z) = (case (#info (get z))
			       of SELinfo(_,_,PTRt(RPT k)) => (SOME RK_RECORD, k)
				| SELinfo(_,_,PTRt(FPT k)) => (NONE, k)
				| MISCinfo(PTRt(RPT k)) => (SOME RK_RECORD, k)
				| MISCinfo(PTRt(FPT k)) => (NONE, k)
				| RECinfo(kind, l) => (SOME kind, length l)
				| _ => (NONE, ~1)
			      (* end case *))
			  | objInfo _ = (NONE, ~1)
			fun samevar (VAR x, VAR y) = (x=y)
			  | samevar _ = false
			fun check1 ((VAR z)::r, j, a) = (case (get z)
			       of {info=SELinfo(i,b,_),...} =>
				    if ((i=j) andalso (samevar(ren b,a)))
				      then check1(r,j+1,a)
				      else NONE
				| _ => NONE
			      (* end case *))
			  | check1 (_::r, j, _) = NONE
			  | check1 ([], j, a) = (case objInfo a
			       of (SOME kind', n) => if (kind = kind') andalso (n = j)
				    then SOME a
				    else NONE
				| (NONE, _) => NONE
			      (* end case *))
			fun check ((VAR z)::r) = (case (get z)
			       of {info=SELinfo(0, a, _),...} => check1(r, 1, ren a)
				| _ => NONE)
			  | check _ = NONE
			val vl'' = map #1 vl'
			in
			  case check vl''
			   of NONE => let
			        val e' = g' e
				in
				  if !used=0 andalso deadup
				    then (click "B1"; app use_less vl''; e')
				    else RECORD(kind, vl', w, e')
				end
			     | SOME z => (
				newname(w,z); click "B2"; (*** ? ***)
				app use_less vl''; g' e)
		       end
		  end
	      | SELECT(i,v,w,t,e) => let
		  val {used,...} = get w
		  val v' = ren v
		  in
		    if !used=0 andalso !CG.deadvars
		      then (
			click "c"; (* could rmv w here *)
			use_less v';
			g' e)
		      else let
			val z = if !CG.selectopt
			      then (case v'
				 of VAR v'' => (case get v''
				       of {info=RECinfo(_, vl),...} => (let
					    val z = #1(List.nth(vl,i))
					    val z' = ren z
					    in
					      case z'
					       of REAL _ => NONE
						| _  => SOME z'
					    end handle Subscript => NONE)
					| _ => NONE
				      (* end case *))
				  | _ => NONE
				(* end case *))
			      else NONE
			in
			  case z
			   of NONE => let
				val e' = g' e
				in
				  if !used=0 andalso deadup
				    then (click "s"; use_less v'; e')
				    else SELECT(i,v',w,t,e')
				end
			    | SOME z' => (
				newname(w, z');
				click "d"; (* could rmv w here *)
				use_less v';
				g' e)
			  (* end case *)
			end
		  end
	      | OFFSET(i,v,w,e) => OFFSET(i,ren v,w,g' e)
	      | APP(f, vl) => let
		  val vl' = map ren vl
		  val f' = ren f
		  fun newvl NONE = vl'
		    | newvl (SOME live) = let
		        fun z(a::al,false::bl) = z(al,bl)
			  | z(a::al,true::bl) = a::z(al,bl)
			  | z _ = nil
			in
			(* This code may be obsolete.  See the comment in the FIX case below. *)
			  case z(vl',live)
			    of nil => [tagInt 0]
			     | vl'' => vl''
			end
		  fun trybeta fv = let
		        val {used=ref u,called=ref c,info} = get fv
		        in
			  case info
			   of FNinfo{args,body,liveargs,...} => if c<>1 orelse u<>1
				  then APP(f',newvl(!liveargs))
				  else (
				    case body
				     of ref(SOME b) => (
					  newnames(args, vl');
					  call_less f';
					  app use_less vl';
					  body := NONE;
					  g' b)
				      | _ => APP(f',newvl(!liveargs))
				    (* end case *))
			    | _ => APP(f',vl')
		        end
		  in
		    case f'
		     of VAR fv => trybeta fv
		      | LABEL fv => trybeta fv
		      | _ => APP(f',vl')
		    (* end case *)
		  end
	      | FIX(l,e) => let
		  fun getinfo (x as (fk,f,vl,cl,b)) = let
			val {used,called,info,...} = get f
			in
			  case info
			   of FNinfo{liveargs=ref(SOME live),...} => let
				fun z (a::al, false::bl) = z(al, bl)
				  | z (a::al, true::bl) = a::z(al, bl)
				  | z _ = nil
				val vl' = z(vl, live)
				val cl' = z(cl, live)
				val drop = foldr (fn (a,b) => if a then b else b+1) 0 live
				fun dropclicks(n) =
				      if n > 0 then (click "D"; dropclicks(n-1)) else ()
				val (vl'', cl'') = (case vl'
				       of nil => let val x = LV.mkLvar()
					   in  dropclicks(drop - 1);
					       enterMISC0 x;
					       ([x], [tagIntTy])
					   end
					| _ => (dropclicks(drop); (vl',cl'))
				      (* end case *))
				in
				  ((fk,f,vl'',cl'',b),used,called,info)
				end
			     | _ => (x,used,called,info)
			end
		  fun keep (_, used, called, info) = (case (!called, !used, info)
			 of (_, 0, FNinfo{body as ref(SOME b), ...}) => (
			      click "g";
			      body:=NONE;
			      drop_body b;
			      false)
			  | (_, 0, FNinfo{body=ref NONE,...}) => (click "g"; false)
			  | (1, 1, FNinfo{body=ref(SOME _),...}) =>
				 (* NOTE: this is an optimistic click.
				    The call could disappear before we
				    get there; then the body would
				    not be cleared out, dangerous. *)
			      (click "e"; false)
			  | (_, _, IFIDIOMinfo{body=ref b,...}) => (click "E"; false)
			  | _ => true
			(* end case *))
		  fun keep2 (_, used, _, info) = (case (!used, info)
			 of (0, FNinfo{body as ref(SOME b),...}) =>
				(* All occurrences were lost *)
				(click "f";
				 body:=NONE;
				 drop_body b;
				 false)
			  | (0, FNinfo{body=ref NONE,...}) =>
				(* We performed a cascaded inlining *)
				(click "q"; false)
			  | (_, FNinfo{body,...}) => (body:=NONE; true)
			  | _ => true
			(* end case *))
		  fun keep3 ((_,_,_,_,b), used, _, info) = (case (!used, info)
			 of (0,FNinfo _) =>
				(* All occurrences were lost *)
				(click "f";
				 drop_body b;
				 false)
			  | _ => true
			(* end case *))
		  fun reduce_body ((fk, f, vl, cl, body), used, called, info) =
			((fk, f, vl, cl, reduce body), used, called, info)
		  val l1 = map getinfo l
		  val l2 = List.filter keep l1
		  val e' = g' e
		  val l3 = List.filter keep2 l2
		  val l4 = map reduce_body l3
		  in
		    case (List.filter keep3 l4)
		     of nil => e'
		      | l5 => FIX(map #1 l5, e')
		  end
	      | SWITCH(v, c, el) => (case ren v
		   of v' as NUM{ival, ty={tag=true, ...}} => if !CG.switchopt
		       then let
			 val i = IntInf.toInt ival
			 fun f (e::el, j) = (if i=j then () else drop_body e; f(el, j+1))
			   | f ([], _) = ()
			 in
			   click "h";
			   f(el, 0);
			   newname(c, tagInt 0);
			   g' (List.nth(el,i))
			 end
		       else SWITCH(v', c, map g' el)
		    | v' => SWITCH(v',c, map g' el)
		  (* end case *))
	      | LOOKER(P.GETHDLR, _, w, t, e) => if !CG.handlerfold
		  then (case hdlr
		     of NONE => if used w
			  then LOOKER(P.GETHDLR,[],w,t,g (SOME(VAR w)) e)
			  else (click "i"; g' e)
		      | SOME w' => (click "j"; newname(w,w'); g' e)
		    (* end case *))
		  else LOOKER(P.GETHDLR,[],w,t,g (SOME(VAR w)) e)
	      | SETTER(P.SETHDLR,[v],e) => let
		  val v' = ren v
		  val e' = g (SOME v') e
		  fun sameVar (VAR x, VAR y) = x = y
		    | sameVar _ = false
		  in
		    if !CG.handlerfold
		      then (case hdlr
			 of SOME v'' => if sameVar (v', v'')
			      then (click "k"; use_less v''; e')
			      else SETTER(P.SETHDLR,[v'],e')
			  | _ => SETTER(P.SETHDLR,[v'],e')
			(* end case *))
		      else SETTER(P.SETHDLR,[v'],e')
		  end
              | SETTER(i,vl,e) => let
      		  val vl' = map ren vl
		  in
		    SETTER(setter (i, vl'), vl', g' e)
		  end
	      | LOOKER(i,vl,w,t,e) => let
		  val vl' = map ren vl
		  val {used,...} = get w
		  in
		    if !used=0 andalso !CG.deadvars
		      then (click "m"; app use_less vl'; g' e)
		      else let
			val e' = g' e
			in
			  if !used=0 andalso deadup
			    then (click "*"; app use_less vl'; e')
			    else LOOKER(i, vl', w, t, e')
			end
		  end
	    (***** TEST *****)
	      | ARITH(P.TEST{from=m, to=n}, [v], x, t, e) => let
		  fun skip () = doArith (P.TEST{from=m, to=n}, [v], x, t, e)
		  in
		    case e
		     of ARITH(P.TEST{from=n2, to=p}, [v2], x2, t2, e2) =>
			  if cvtPreCondition (n, n2, x, v2)
			    then ARITH(P.TEST{from=m, to=p}, [ren v], x2, t2, g' e2)
			    else skip ()
		      | _ => skip ()
		    (* end case *)
		  end
	      | ARITH(P.TEST{from=m, to=n}, [v, f], x, t, e) => let
		(* this case is for m=64 on 32-bit systems *)
		  fun skip () = doArith (P.TEST{from=m, to=n}, [v, f], x, t, e)
		  in
		    case e
		     of ARITH(P.TEST{from=n2, to=p}, [v2], x2, t2, e2) =>
			  if cvtPreCondition (n, n2, x, v2)
			    then ARITH(P.TEST{from=m, to=p}, [ren v, ren f], x2, t2, g' e2)
			    else skip ()
		      | _ => skip ()
		    (* end case *)
		  end
	    (***** TESTU : word -> int *****)
	      | ARITH(P.TESTU{from=m, to=n}, [v], x, t, e) => let
		  fun skip () = doArith (P.TESTU{from=m, to=n}, [v], x, t, e)
		  in
		    case e
		     of ARITH(P.TEST{from=n2, to=p}, [v2], x2, t2, e2) =>
			  if cvtPreCondition (n, n2, x, v2)
			    then ARITH(P.TESTU{from=m, to=p}, [ren v], x2, t2, g' e2)
			    else skip ()
		      | _ => skip ()
		    (* end case *)
		  end
	      | ARITH(P.TESTU{from=m, to=n}, [v, f], x, t, e) => let
		(* this case is for m=64 on 32-bit systems *)
		  fun skip () = doArith (P.TESTU{from=m, to=n}, [v, f], x, t, e)
		  in
		    case e
		     of ARITH(P.TEST{from=n2, to=p}, [v2], x2, t2, e2) =>
			  if cvtPreCondition (n, n2, x, v2)
			    then ARITH(P.TESTU{from=m, to=p}, [ren v, ren f], x2, t2, g' e2)
			    else skip ()
		      | _ => skip ()
		    (* end case *)
		  end
	    (***** TEST_INF : intinf -> int *****)
	      | ARITH(P.TEST_INF n, [v, f], x, t, e) => let
		  fun skip () = ARITH(P.TEST_INF n, [ren v, ren f], x, t, g' e)
		  in
		    case e
		     of ARITH(P.TEST{from=n2, to=p}, [v2], x2, t2, e2) =>
			  if cvtPreCondition (n, n2, x, v2)
			    then ARITH(P.TEST_INF p, [ren v, ren f], x2, t2, g' e2)
			    else skip ()
		      | ARITH(P.TEST{from=n2, to=p}, [v2, f2], x2, t2, e2) => (* 64 on 32 bit *)
			  if cvtPreCondition (n, n2, x, v2)
			    then (
			      use_less f2;
			      ARITH(P.TEST_INF p, [ren v, ren f], x2, t2, g' e2))
			    else skip ()
		      | _ => skip ()
		    (* end case *)
		  end
	    (***** other ARITH cases *****)
	      | ARITH arg => doArith arg
	    (***** COPY : word -> int *****)
	      | PURE(P.COPY{from=m, to=n}, [v], x, t, e) => let
		  fun mkCOPY (from, to, x, t, e) =
			if (from = to)
			  then (newname(x, ren v); g' e)
			  else doPure (P.COPY{from=from, to=to}, [v], x, t, e)
		  fun mkEXTEND (from, to, v, x, t, e) = if (from = to)
			then (newname(x, v); g' e)
			else doPure (P.EXTEND{from=from, to=to}, [v], x, t, e)
		  fun skip () = mkCOPY(m, n, x, t, e)
		  in
		    case e
		     of ARITH(P.TEST{from=n2, to=p}, [v2], x2, t2, e2) =>
			  if not (cvtPreCondition (n, n2, x, v2))
			    then skip ()
			  else if (m < p)
			    then mkCOPY(m, p, x2, t2, e2)
			    else ARITH(P.TESTU{from=m, to=p}, [ren v], x2, t2, g' e2)
		      | ARITH(P.TESTU{from=n2, to=p}, [v2], x2, t2, e2) =>
			  if not (cvtPreCondition (n, n2, x, v2))
			    then skip ()
			  else if (m < p)
			    then mkCOPY(m, p, x2, t2, e2)
			    else ARITH(P.TESTU{from=m, to=p}, [ren v], x2, t2, g' e2)
		      | PURE(P.COPY{from=n2, to=p}, [v2], x2, t2, e2) =>
			  if cvtPreCondition (n, n2, x, v2)
			    then mkCOPY(m, p, x2, t2, e2)
			    else skip ()
		      | PURE(P.EXTEND{from=n2, to=p}, [v2], x2, t2, e2) =>
			  if not (cvtPreCondition (n, n2, x, v2))
			    then skip ()
			    else mkEXTEND(m, p, ren v, x2, t2, e2)
		      | PURE(P.TRUNC{from=n2, to=p}, [v2], x2, t2, e2) =>
			  if not (cvtPreCondition (n, n2, x, v2))
			    then skip ()
			  else if (m <= p)
			    then mkCOPY(m, p, x2, t2, e2)
			    else PURE(P.TRUNC{from=m, to=p}, [ren v], x2, t2, g' e2)
		      | PURE(P.COPY_INF n2, [v2, f], x2, t2, e2) =>
			  if cvtPreCondition (n, n2, x, v2)
			    then PURE(P.COPY_INF m, [ren v, ren f], x2, t2, g' e2)
			    else skip ()
		      | PURE(P.EXTEND_INF n2, [v2, f], x2, t2, e2) =>
			  if not (cvtPreCondition (n, n2, x, v2))
			    then skip ()
			  else if (m = n)
			    then PURE(P.EXTEND_INF m, [ren v, ren f], x2, t2, g' e2)
			    else PURE(P.COPY_INF m, [ren v, ren f], x2, t2, g' e2)
		      | _ => skip ()
		    (* end case *)
		  end
	    (***** EXTEND *****)
	      | PURE(P.EXTEND{from=m, to=n}, [v], x, t, e) => let
		  fun mkEXTEND (from, to, x, t, e) = if (from = to)
			then (newname(x, ren v); g' e)
			else doPure( P.EXTEND{from=from, to=to}, [v], x, t, e)
		  fun skip () = doPure (P.EXTEND{from=m, to=n}, [v], x, t, e)
		  in
		    case e
		     of ARITH(P.TEST{from=n2, to=p}, [v2], x2, t2, e2) =>
			  if not (cvtPreCondition (n, n2, x, v2))
			    then skip ()
			  else if (p >= n)
			    then mkEXTEND(m, p, x2, t2, e2)
			    else ARITH(P.TEST{from=m, to=p}, [v2], x2, t2, g' e2)
		      | ARITH(P.TESTU{from=n2, to=p}, [v2], x2, t2, e2) =>
			  if not (cvtPreCondition (n, n2, x, v2))
			    then skip ()
			  else if (p > n)
			    then PURE(P.EXTEND{from=m, to=n}, [ren v], x2, t2, g' e2)
			    else ARITH(P.TEST{from=m, to=p}, [v2], x2, t2, g' e2)
		      | PURE(P.COPY{from=n2, to=p}, [v2], x2, t2, e2) =>
			  if not (cvtPreCondition (n, n2, x, v2))
			    then skip ()
			  else if (n2 = p)
			    then mkEXTEND(m, p, x2, t2, e2)
			    else skip ()
		      | PURE(P.EXTEND{from=n2, to=p}, [v2], x2, t2, e2) =>
			  if cvtPreCondition (n, n2, x, v2)
			    then mkEXTEND(m, p, x2, t2, e2)
			    else skip ()
		      | PURE(P.TRUNC{from=n2, to=p}, [v2], x2, t2, e2) =>
			  if not (cvtPreCondition (n, n2, x, v2))
			    then skip ()
			  else if (p >= m)
			    then mkEXTEND(m, p, x2, t2, e2)
			    else PURE(P.TRUNC{from=m, to=p}, [ren v], x2, t2, g' e2)
		      | PURE(P.EXTEND_INF n2, [v2, f], x2, t2, e2) =>
			  if cvtPreCondition (n, n2, x, v2)
			    then PURE(P.EXTEND_INF m, [ren v, ren f], x2, t2, g' e2)
			    else skip ()
		      | _ => skip ()
		    (* end case *)
		  end
	    (***** TRUNC : int -> word *****)
	      | PURE(P.TRUNC{from=m, to=n}, [v], x, t, e) => let
		  fun skip () = doPure (P.TRUNC{from=m, to=n}, [v], x, t, e)
		  in
		    case e
		     of PURE(P.TRUNC{from=n2, to=p}, [v2], x2, t2, e2) =>
			  if cvtPreCondition (n, n2, x, v2)
			    then PURE(P.TRUNC{from=m, to=p}, [ren v], x2, t2, g' e2)
			    else skip ()
		      | _ => skip ()
		    (* end case *)
		  end
	    (***** COPY_INF : word -> intinf *****)
	      | PURE(P.COPY_INF m, [v, f], x, t, e) => let
		  fun skip () = PURE(P.COPY_INF m, [ren v, ren f], x, t, g' e)
		  in
		    case e
		     of ARITH(P.TEST_INF p, [v2, f2], x2, t2, e2) =>
			  if not (cvtInfPrecondition (x, v2))
			    then skip ()
			  else if (p > m)
			    then (
			      use_less f; use_less f2;
			      PURE(P.COPY{from=m, to=p}, [ren v], x2, t2, g' e2))
			  else if test64On32 m
			    then (
			      use_less f; use_less f2;
			      ARITH(P.TESTU{from=m, to=p}, [ren v], x2, t2, g' e2))
			    else skip ()
		      | PURE(P.TRUNC_INF p, [v2, f2], x2, t2, e2) =>
			  if not (cvtInfPrecondition (x, v2))
			    then skip ()
			    else if (p >= m)
			      then (
				use_less f; use_less f2;
				PURE(P.COPY{from=m, to=p}, [ren v], x2, t2, g' e2))
			    else if test64On32 m
			      then (
				use_less f; use_less f2;
				PURE(P.TRUNC{from=m, to=p}, [ren v], x2, t2, g' e2))
			      else skip ()
		      | _ => skip ()
		    (* end case *)
		  end
	    (***** EXTEND_INF *****)
	      | PURE(P.EXTEND_INF m, [v, f], x, t, e) => let
		  fun mkEXTEND (from, to, v, x, t, e) = if (from = to)
			then (newname(x, v); g' e)
			else PURE(P.EXTEND{from=from, to=to}, [v], x, t, g' e)
		  fun skip () = PURE(P.EXTEND_INF m, [ren v, ren f], x, t, g' e)
		  in
		    case e
		     of ARITH(P.TEST_INF p, [v2, f2], x2, t2, e2) =>
			  if not (cvtInfPrecondition (x, v2))
			    then skip ()
			    else if (p >= m)
			      then (
				use_less f; use_less f2;
				mkEXTEND(m, p, ren v, x2, t2, e2))
			    else if test64On32 m
			      then (
				use_less f; use_less f2;
				ARITH(P.TEST{from=m, to=p}, [ren v], x2, t2, g' e2))
			      else skip ()
		      | PURE(P.TRUNC_INF p, [v2, f2], x2, t2, e2) =>
			  if not (cvtInfPrecondition (x, v2))
			    then skip ()
			    else (
			      use_less f; use_less f2;
			      if (p < m)
				then PURE(P.TRUNC{from=m, to=p}, [ren v], x2, t2, g' e2)
				else mkEXTEND(m, p, ren v, x2, t2, e2))
		      | _ => skip ()
		    (* end case *)
		  end
	    (***** TRUNC_INF *****)
	      | PURE(P.TRUNC_INF n, [v, f], x, t, e) => let
		  fun skip () = PURE(P.TRUNC_INF n, [ren v, ren f], x, t, g' e)
		  in
		    case e
		     of PURE(P.TRUNC{from=n2, to=p}, [v2], x2, t2, e2) =>
			  if cvtPreCondition (n, n2, x, v2)
			    then PURE(P.TRUNC_INF p, [ren v, ren f], x, t, g' e)
			    else skip ()
		      | _ => skip ()
		    (* end case *)
		  end
	    (***** other PURE cases *****)
	      | PURE arg => doPure arg
	    (***** RCC *****)
	      | RCC(k,l,p,vl,wtl,e) =>
		(* leave raw C calls alone *)
		  RCC (k, l, p, map ren vl, wtl, g' e)
	    (***** BRANCH *****)
	      | BRANCH(rator, vl, c, e1, e2) => let
		  val vl' = List.map ren vl
		  fun skip () = if !CG.branchfold andalso equalUptoAlpha(e1, e2)
			  then (
			    click "z";
			    app use_less vl';
			    newname(c,tagInt 0);
			    drop_body e2;
			    g' e1)
			  else BRANCH(rator, vl', c, g' e1, g' e2)
		  fun getifidiom f = (case ren f
			 of VAR v => (case get v
			       of {info=IFIDIOMinfo{body},...} => SOME body
				| _ => NONE
			      (* end case *))
			  | _ => NONE
			(* end case *))
		(* first we try to contract the condition *)
		  val cond = if !CG.comparefold then branch (rator, vl') else NONE
		  in
		    case cond
		     of NONE => (case (e1, e2)
			   of (APP(VAR f, [NUM{ival=1, ...}]), APP(VAR f', [NUM{ival=0, ...}])) =>
				if f=f'
				  then (case getifidiom(VAR f)
				     of SOME(body as ref(SOME(c', a, b))) => (
					(* handle "if idiom" *)
					  newname(c', VAR c);
					  body := NONE;
					  g' (BRANCH(rator, vl, c, a, b))) (* NOTE: could use vl' here instead of vl. *)
				      | _ => skip()
				    (* end case *))
				  else skip()
			    | _ => skip()
			  (* end case *))
		      | SOME b => (
			   List.app use_less vl';
			   newname(c, tagInt 0);
			   if b then (drop_body e2; g' e1) else (drop_body e1; g' e2))
		    (* end case *)
		  end
	    (* end case *))
    (***** other ARITH cases *****)
      and doArith (rator, vl, w, t, e) = let
	    val vl' = List.map ren vl
	    in
	      if !CG.arithopt
		then (case arith(rator, vl')
		   of Val v => (
			List.app use_less vl';
			newname (w, v);
			g' e)
		    | Arith(rator', vl'') => (
			List.app use_less vl';
			List.app use vl'';
			ARITH(rator', vl'', w, t, g' e))
		    | Pure(rator', vl'') => (
			List.app use_less vl';
			List.app use vl'';
			PURE(rator', vl'', w, t, g' e))
		    | None => ARITH(rator, vl', w, t, g' e)
		  (* end case *))
		else ARITH(rator, vl', w, t, g' e)
	    end
    (***** other PURE cases *****)
      and doPure (rator, vl, w, t, e) = let
	    val vl' = List.map ren vl
	    val {used, ...} = get w
	    fun rest () = let
		  val e' = g' e
		  in
		    if !used=0 andalso deadup
		      then (List.app use_less vl'; click "*"; e')
		      else PURE(rator, vl', w, t, e')
		  end
	    in
	      if !used=0 andalso !CG.deadvars
		then (click "m"; List.app use_less vl'; g' e)
	      else if !CG.arithopt
		then (case pure(rator, vl')
		   of Val v => (
			List.app use_less vl';
			newname(w, v);
			g' e)
		    | Pure(rator', vl'') => (
			List.app use_less vl';
			List.app use vl'';
			PURE(rator', vl'', w, t, g' e))
		    | _ => rest()
		 (* end case *))
		else rest()
	    end
      in
(*DEBUG*)
        fn cexp => ((g' cexp)
	     handle ex => (say "****** g' ******\n"; PPCps.prcps cexp; raise ex))
      end (* g *)

in  debugprint "Contract: "; debugflush();
    enterMISC0 fvar; app enterMISC0 fargs;
    pass1 cexp;
    cpssize := LV.Tbl.numItems m;
    let val cexp' = reduce cexp
    in  debugprint "\n";
	if debug
	    then (debugprint "After contract: \n";
		  PPCps.prcps cexp')
	else ();
	(fkind, fvar, fargs, ctyl, cexp')
    end
end

end (* structure Contract *)
