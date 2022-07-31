(* loopify.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature LOOPIFY =
sig
    val loopify : FLINT.prog -> FLINT.prog
end


structure Loopify :> LOOPIFY =
struct
local
    structure O  = Option
    structure LV = LambdaVar
    structure M  = LV.Map
    structure S  = LV.Set
    structure OU = OptUtils
    structure LT = Lty
    structure FR = FunRecMeta
    structure LK = LtyKernel
    structure F  = FLINT
    structure CTRL = FLINT_Control
in

val say = Control_Print.say
fun bug msg = ErrorMsg.impossible ("Loopify: "^msg)
val cplv = LambdaVar.dupLvar

type al = F.value list list
datatype info = I of {tails : al ref, calls: al ref, icalls: al ref,
		      tcp: bool ref, parent: LV.lvar}
exception NotFound

fun loopify (prog as (progkind,progname,progargs,progbody)) = let

    val m : info LambdaVar.Tbl.hash_table = LambdaVar.Tbl.mkTable(128, NotFound)

    (* tails: number of tail-recursive calls
     * calls: number of other calls
     * icalls: non-tail self-recursive subset of `calls'
     * tcp: always called in tail-position
     * parent: enclosing function *)
    fun new (f,known,parent) =
	let val i = I{tails=ref [], calls=ref [], icalls=ref [],
		      tcp=ref known, parent=parent}
	in LambdaVar.Tbl.insert m (f,i); i end

    fun get f = LambdaVar.Tbl.lookup m f

(* collect tries to determine what calls are tail recursive.
 * If a function f is always called in tail position in a function g,
 * then all tail calls to g from f are indeed tail recursive. *)
(* tfs:  we are currently in tail position relative to those functions
 * p:  englobing function *)
fun collect p tfs le = let
    val loop = collect p tfs
in case le
    of F.RET _ => ()
     | F.LET(_,body,le) => (collect p S.empty body; loop le)
     | F.FIX([({isrec=(NONE | SOME(_,FR.LK_TAIL)),known,...},f,_,body)],le) =>
       let val I{tcp,calls,icalls,...} = new(f, known, p)
	   val _ = loop le
	   val necalls = length(!calls)
       in  collect f (if !tcp then S.add(tfs, f) else S.singleton f) body;
	   icalls := List.take(!calls, length(!calls) - necalls)
       end
     | F.FIX(fdecs,le) =>
       let (* create the new entries in the map *)
	   val fs = map (fn (fk as {known,...},f,_,body) =>
			 (fk, f, body, new(f, false, p)))
			fdecs
	   fun cfun ({isrec,...}: FR.fkind, f, body, I{calls,icalls,...}) =
	       let val necalls = length(!calls)
	       in collect f (S.singleton f) body;
		  icalls := List.take(!calls, length(!calls) - necalls)
	       end
       in loop le;
	  app cfun fs
       end
     | F.APP(F.VAR f,vs) =>
       (let val I{tails,calls,tcp,parent,...} = get f
	in if S.member(tfs, f) then tails := vs::(!tails)
	   else (calls := vs::(!calls);
		 if S.member(tfs, parent) then () else tcp := false)
	end handle NotFound => ())
     | F.TFN((_,_,_,body),le) => (collect p S.empty body; loop le)
     | F.TAPP _ => ()
     | F.SWITCH(v,ac,arms,def) =>
       let fun carm (_,body) = loop body
       in app carm arms; case def of SOME le => loop le | _ => ()
       end
     | (F.CON(_,_,_,_,le) | F.RECORD(_,_,_,le) |
	F.SELECT(_,_,_,le) | F.PRIMOP(_,_,_,le)) => loop le
     | F.RAISE _ => ()
     | F.HANDLE(le,v) => collect p S.empty le
     | F.BRANCH(_,_,le1,le2) => (loop le1; loop le2)

     | F.APP _ => bug "weird F.APP in collect"
end

(* (intended as a `foldr' argument).
 * `filt' is the bool list indicating if the arg is kept
 * `func' is the list of arguments for the FIX
 * `call' is the list of arguments for the APP
 * `free' is the list of resulting free variables *)
fun drop_invariant ((v,t),actuals,(filt,func,call,free)) =
    if !CTRL.dropinvariant andalso List.all (FlintUtil.valueIsVar v) actuals
      then
	(* drop the argument: the free list is unchanged *)
	(false::filt, func, call, (v,t)::free)
      else
	(* keep the argument: create a new var (used in the call)
	 * which will replace the old in the free vars *)
	let val nv = cplv v
	in (true::filt, (v,t)::func, (F.VAR nv)::call, (nv,t)::free)
	end

(* m: int intmap	renaming for function calls
 * tf:(int,int) list	the current functions (if any) and their tail version
 * le:			you get the idea *)
fun lexp m tfs le = let
    val loop = lexp m tfs
in case le
    of F.RET _ => le
     | F.LET(lvs,body,le) => F.LET(lvs, lexp m [] body, loop le)
     | F.FIX(fdecs,le) =>
       let fun cfun (fk: FR.fkind as {isrec=SOME(ltys,FR.LK_UNKNOWN),cconv,...},
		     f,args,body) =
	       let val I{tcp=ref tcp,icalls=ref icalls,tails=ref tails,...} =
		       get f
		   val tfs = (if tcp then tfs else [])
	       (* cpsopt uses the following condition:
		*     escape = 0 andalso !unroll_call > 0
		*    	    andalso (!call - !unroll_call > 1
		*    		     orelse List.exists (fn t=>t) inv)
		* `escape = 0': I don't quite see the need for it, though it
		*     probably won't change much since etasplit should have
		*     made "everything" known already.
		* `!call - !unroll_call > 1 orelse List.exists (fn t=>t) inv)':
		*     loopification is only useful if there is more than one
		*     external call or if there are loop invariants.
		*     Note that we deal with invariants elsewhere, so it's
		*     not a good reason to loopify here. *)
	       (*** rationale behind the restrictions: ***
		* `icallnb = 0': loopification is pointless and will be
		*     undone by fcontract.
		* `C.callnb fi <= icallnb + 1': if there's only one external
		*     call, loopification will probably (?) not be of much use
		*     and the same benefit would be had by just moving f.  *)
	       in if null icalls andalso null tails
		  then (fk, f, args, lexp m tfs body)
		  else
		      let val cconv' =
			      case cconv
			       of (FR.CC_FCT | FR.CC_FUN(LT.FF_FIXED)) => cconv
				| FR.CC_FUN(LT.FF_VAR(f1,f2)) =>
				  FR.CC_FUN(LT.FF_VAR(true,f2))

			  (* figure out what arguments of the tail loop
			   * are invariants and create the corresponding
			   * function args, call args, filter
			   * function for the actual calls, ... *)
			  val (tfs',atfun,atcall,args,ft) =
			      if null tails then (tfs,[],[],args,f) else let
				  val ft = cplv f
				  val actuals = OU.transpose tails
				  val (fcall,afun,acall,afree) =
				      ListPair.foldr drop_invariant
						     ([],[],[],[])
						     (args, actuals)
			      in ((f,ft,fcall)::tfs,
				  afun, acall, afree, ft)
			      end

			  (* Do the same for the non-tail loop.  *)
			  val (nm,alfun,alcall,args,fl) =
			      if null icalls then (m,[],[],args,f) else let
				  val fl = cplv f
				  val actuals = OU.transpose icalls
				  val (fcall,afun,acall,afree) =
				      ListPair.foldr drop_invariant
						     ([],[],[],[])
						     (args, actuals)
			      in (M.insert(m, f, (fl, fcall)),
				  afun, acall, afree, fl)
			      end

			  (* make the new body.  *)
			  val nbody = lexp nm tfs' body

			  (* wrap into a tail loop if necessary.  *)
			  val nbody =
			      if null tails then nbody else
				  F.FIX([({isrec=SOME(ltys, FR.LK_TAIL),
					   known=true, inline=FR.IH_SAFE,
					   cconv=cconv'}, ft, atfun,
					  nbody)],
				    F.APP(F.VAR ft, atcall))

			  (* wrap into a non-tail loop if necessary.  *)
			  val nbody =
			      if null icalls then nbody else
				  F.FIX([({isrec=SOME(ltys, FR.LK_LOOP),
					   known=true, inline=FR.IH_SAFE,
					   cconv=cconv'}, fl, alfun,
					  nbody)],
				    F.APP(F.VAR fl, alcall))

		      in (fk, f, args, nbody)
		      end
	       end
	     | cfun (fk as {inline=FR.IH_UNROLL,isrec=SOME _,...},f,args,body) =
	       let val I{tcp=ref tcp,...} = get f
	       in (fk, f, args, lexp m (if tcp then tfs else []) body)
	       end
	     | cfun (fk,f,args,body) =
	       let val I{tcp=ref tcp,...} = get f
	       in (fk, f, args, lexp m (if tcp then tfs else []) body)
	       end
       in F.FIX(map cfun fdecs, loop le)
       end
     | F.APP(F.VAR f,vs) =>
       (case List.find (fn (ft,ft',filt) => ft = f) tfs
	 of SOME(ft, ft', filt) => F.APP(F.VAR ft', OU.filter filt vs)
	  | NONE =>
	    (case M.find(m,f)
	      of SOME(fl, filt) =>
		   F.APP(F.VAR fl, OU.filter filt vs)
               | NONE => le
            (*esac*)))
     | F.TFN((tfk,f,args,body),le) => F.TFN((tfk, f, args, loop body), loop le)
     | F.TAPP(f,tycs) => le
     | F.SWITCH(v,ac,arms,def) =>
       let fun carm (con,le) = (con, loop le)
       in F.SWITCH(v, ac, map carm arms, O.map loop def)
       end
     | F.CON(dc,tycs,v,lv,le) => F.CON(dc, tycs, v, lv, loop le)
     | F.RECORD(rk,vs,lv,le) => F.RECORD(rk, vs, lv, loop le)
     | F.SELECT(v,i,lv,le) => F.SELECT(v, i, lv, loop le)
     | F.RAISE(v,ltys) => le
     | F.HANDLE(le,v) => F.HANDLE(lexp m [] le, v)
     | F.BRANCH(po,vs,le1,le2) => F.BRANCH(po, vs, loop le1, loop le2)
     | F.PRIMOP(po,vs,lv,le) => F.PRIMOP(po, vs, lv, loop le)

     | F.APP _ => bug "unexpected APP"
end

in
    collect progname S.empty progbody;
    (progkind, progname, progargs, lexp M.empty [] progbody)
end

end
end


