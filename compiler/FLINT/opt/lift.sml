(* lift.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature LIFT =
  sig
      val typeLift: FLINT.prog -> FLINT.prog
      val wellFormed: FLINT.prog -> bool
  end

structure Lift : LIFT =
struct

local
  structure A  = Access
  structure LV = LambdaVar
  structure LE = LtyExtern
  structure DI = DebIndex
  structure PT = PrimTyc
  structure LT = Lty
  structure FR = FunRecMeta
  structure LB = LtyBasic
  structure LD = LtyDef
  structure LK = LtyKernel
  structure PL = PLambda
  structure CTRL = FLINT_Control
  open FLINT
in

(*****  Utility functions *****)

exception PartialTypeApp
exception VarNotFound
exception LiftTypeUnknown
exception DonotLift
exception FTABLE
exception LiftCompileError
exception VENV
exception FENV
exception abstract

fun bug s = ErrorMsg.impossible ("Lift: " ^ s)


val mkv = LV.mkLvar

val wellfixed = ref true
val welltapped = ref true
val tappLifted = ref 0

type depth = int
type tdepth = int
type num = int
type abstract = bool

type var = (LT.lty * LV.lvar list * depth * tdepth * abstract * num)
type venv = var LambdaVar.Tbl.hash_table

type freevar = (LV.lvar * LT.lty)
type fenv = (freevar LambdaVar.Tbl.hash_table) list

datatype Ltype = Tfn | Tapp
type header =  (Ltype * LV.lvar * lexp) list

datatype env = Ienv of venv * fenv

(* Utility functions *)

val ABS = true
val NOABS = false
val fkfct = {isrec=NONE, known=false, inline=FR.IH_SAFE, cconv=FR.CC_FCT}

fun adjust(t, ntd, otd) = LB.lt_adj(t, otd, ntd)

fun findEnv(v, Ienv(venv,fenvs)) =
    (LambdaVar.Tbl.lookup venv v)
    handle _ => (print (LambdaVar.prLvar v); bug "findEnv: var not found" )

fun getVar (v, Ienv(venv,fenv :: fenvs), t, td, td') =
    ((let
	val (v', nt') = (LambdaVar.Tbl.lookup fenv v)
    in  (v', nt', nil)
    end) handle _ => let val v' = mkv()
	                 val nt' = adjust(t, td, td')
			 val _ = LambdaVar.Tbl.insert fenv (v, (v', nt'))

		     in  (v', nt', [v])
		     end )
  | getVar _ = bug "unexpected freevariableEnv in getVar"

fun newVar(v, env, td) =
    let
	val (t, vs, td', d', abs, _) = findEnv(v,env)
	val exp = if (abs andalso (d' > 0) andalso (td' < td)) then
	              let
			  val (v', t', fv) = getVar(v, env, t, td, td')
		      in
			  (v', t', fv)
		      end
		  else
		      (v, adjust(t, td, td'), nil)
    in
	exp
    end


fun pushFenv (Ienv(venv,fenvs)) =
    let val nt = LambdaVar.Tbl.mkTable(32,FTABLE)
    in  Ienv(venv, nt::fenvs)
    end

fun popFenv (Ienv(venv, fenv::fenvs)) = Ienv(venv,fenvs)
  | popFenv _ = raise LiftCompileError

fun addEnv (Ienv(venv,fenvs), vs, ts, fvs, td, d, abs) =
    let
	fun f (v, t) = LambdaVar.Tbl.insert venv (v, (t, fvs, td, d, abs, 0))
	fun zip([], [], acc) = acc
	  | zip (a::r, a'::r', acc) = zip (r, r', (a, a')::acc)
	  | zip _ = raise LiftCompileError
    in
	map f (zip (vs, ts, nil))
    end

fun rmEnv(Ienv(venv,fenvs), v) =
    ignore (LambdaVar.Tbl.remove venv v) handle _ => ()


fun getFreeVar(fvs, Ienv(venv, fenv::fenvs)) =
    let
	fun f(v) = (LambdaVar.Tbl.lookup fenv v)
	    handle _ => bug "freevar not found"
    in
	map f fvs
    end
  | getFreeVar _ = bug "unexpected freevariableEnv in getFreeVar"


fun writeLambda([], exp) = exp
  | writeLambda(fvs, exp) =
    let fun g(fvs', exp') =
	    let val newvar = mkv()
		val fund = {isrec = NONE, cconv = FR.CC_FUN(Lty.FF_VAR(true,true)), known = false,
				  inline = FR.IH_SAFE }
	    in  FIX([(fund, newvar, fvs', exp')], RET [VAR newvar])
	    end
    in
	if ( (List.length fvs) <= 9) then
	    g(fvs, exp)
	else
	    let
		fun f(x,e) = ([x], e)
	    in
		foldr (g o f) exp fvs
	    end
    end


fun writeApp(v, vs) =
    if ( (List.length vs) <= 9 ) then
	APP(v, vs)
    else
	let fun f([], e) = let val newvar = mkv()
			   in  (RET [VAR newvar], newvar)
			   end
	      | f(v::vs, e) = let val (e', v') = f(vs, e)
				  val newvar = mkv()
			      in  (LET([v'], APP(VAR newvar,[v]), e'), newvar)
			      end
	    val (e',v') = f(List.tl vs, RET [])
	in
	    LET([v'], APP(v, [List.hd vs]), e')
	end


fun writeHeader(hd, exp) =
    let
	fun f ((Tapp, v, e), e') = LET([v], e, e')
	  | f ((Tfn, v, TFN(e, e')), e'') = TFN(e,e'')
	  | f _ = bug "unexpected header in writeHeader"
	val hds = foldr f exp hd
    in
	hds
    end


(* The way renaming is done is that if rename is true and d > 0
   and td < td' then change var  *)

fun initInfoEnv () =
    let val venv : venv = LambdaVar.Tbl.mkTable(32, VENV)
	val fenv = LambdaVar.Tbl.mkTable(32, FENV)
    in
	Ienv (venv, [fenv])
    end


fun wellFormed (fdec : fundec) =
    case fdec of
	(fk as {cconv = FR.CC_FCT, ...}, v, vts, e) =>
	let
	    fun formed (RET _, d) = true
	      | formed (LET(vs, e1, e2), d) = formed(e1, d) andalso formed(e2, d)
	      | formed (APP(v, vs), d) = true
	      | formed (TAPP(v, ts), d) = (case d of 0 => true
						   | _ => false )
	      | formed (RECORD(rk, vs, v, e), d) = formed(e, d)
	      | formed (SELECT(v, i, l, e), d) = formed(e, d)
	      | formed (RAISE _, d) = true
	      | formed (HANDLE(e, v), d) = formed(e, d)
	      | formed (BRANCH(pr, vs, e1, e2), d) = formed(e1, d) andalso formed(e2, d)
	      | formed (PRIMOP(pr, vs, l, e), d) = formed(e, d)
	      | formed (SWITCH(v, a, ces, eopt), d) =
		let val b1 = case eopt of NONE => true
					| SOME e => formed(e, d)
		    fun f(c,e) = (e,d)
		    val es = map f ces
		    val b2 = map formed es
		    val b = foldr (fn (x,y) => x andalso y) b1 b2
		in
		    b
		end
	      | formed (CON(dc, ts, v, l, e), d) = formed(e, d)
	      | formed (TFN((tfk, l, ts, e1), e2), d) =
		formed(e1, d) andalso formed(e2, d)
	      | formed (FIX(fds, e), d) =
		let
		    val b1 = formed(e, d)
		    val b2 = case fds of
			({cconv = FR.CC_FCT, ...}, l, vs, e')::r => map formed [(e', d)]
		      | _ => let fun f (v1, v2, v3, v4) = (v4, d + 1)
				 val es = map f fds
				 val b' = map formed es
			     in
				 b'
			     end
		    val b = foldr (fn (x,y) => x andalso y) b1 b2
		in
		    b
		end
	in
	    formed(e, 0)
	end
      | _ => bug "non FCT program in Lift"


fun lift (e, env, td, d, ad, rename) =
    let
	fun comb((v,t,fv,hd), (l1,l2,l3,l4)) =  (v::l1, t::l2, fv@l3,hd@l4)

	fun ltInst(lt, ts) =
	    ( case LE.lt_inst(lt, ts) of
		[x] => x
	      | _ => bug "unexpected case in ltInst" )

	fun arglty(lt, ts) =
	    let
		val (_, atys, _) = LD.ltd_arrow(ltInst(lt, ts))
	    in
		case atys of
		    [x] => x
		  | _ => bug "unexpected case in arglty"
	    end

	fun reslty(lt, ts) =
	    let
		val (_, _, rtys) = LD.ltd_arrow(ltInst(lt, ts))
	    in
		case rtys of
		    [x] => x
		  | _ => bug "unexpected case in reslty"
	    end

	fun loopcv env var v = let
	    val (v', t, fv) = newVar(v, env, td) (* Not checking for poly *)
	in
	    (var v', t, fv, nil)   (* Check whether this is t or t' *)
	end

	fun loopc env v = let
	    fun c t = (v, t, [], [])
	in
	    case v
	     of VAR v' => loopcv env VAR v'
	      | INT{ty, ...} => c (LB.ltc_num ty)
	      | WORD{ty, ...} => c (LB.ltc_num ty)
	      | REAL _  => c LB.ltc_real		(* REAL32: FIXME *)
	      | STRING _ => c LB.ltc_string
	end

	fun lpacc env (A.LVAR v) =
	    let val (v', _, fv, _) = loopcv env (fn v => v) v
	    in  (A.LVAR v', fv)
	    end
	  | lpacc env (A.PATH(a,i)) =
	    let val (a', fvs) = lpacc env a
	    in  (A.PATH(a',i), fvs)
	    end
	  | lpacc env a = (a, nil)

	fun lpcon env (A.EXN a) =
	    let val (a', fv) = lpacc env a
	    in  (A.EXN a', fv)
	    end
	  | lpcon env (A.SUSP NONE) = (A.SUSP NONE, nil)
	  | lpcon env (A.SUSP (SOME (a', a''))) =
	    let
		val (a1, fv1) = lpacc env a'
		val (a2, fv2) = lpacc env a''
	    in
		(A.SUSP(SOME (a', a'')), fv1 @ fv2)
	    end
	  | lpcon env a = (a, nil)

       fun loope(RET vs, env, d, ad) =
	   let
	       val vls = map (loopc env) vs
	       val (vs, ts, fvs, hd) = foldr comb (nil, nil, nil, nil) vls
	   in
	       (RET vs, ts, fvs, hd)
	   end
	 | loope (LET(vs, e1, e2), env, d, ad) =
	   let
	       val (e', ts, fvs, hd) = loope(e1, env, d, ad)
	       val _ = addEnv(env, vs, ts, fvs, td, d, ABS)
	       val (e'', ts', fvs', hd') = loope(e2, env, d, ad)
	   in
	       (LET(vs, e', e''), ts', fvs@fvs', hd@hd')
	   end
	 | loope (APP(v1,vs), env, d, ad) =
	   let
	       val (v1', t, fvs, hd) = loopc env v1
	       val vls = map (loopc env) vs
	       val (vs', ts', fvs', hd') = foldr comb (nil, nil, nil, nil) vls
	       val nt = #2(LE.ltd_fkfun t) handle LD.DeconExn => bug "loope"
	   in
	       (APP(v1', vs'), nt, fvs@fvs', hd@hd')
	   end
	 | loope (e as TAPP(v,tycs), env as Ienv(venv,fenvs), d, ad) =
	   let
	       val (v', nt', fv', hd) = loopc env v (* fv' and hd are nil *)
	       val nt = LE.lt_inst (nt', tycs)
	       val len1 = List.length tycs
	   in
	       case d of
		   0 => (e, nt, fv', hd)
		 | _ => case v of
		       VAR v'' =>
			       let
				   val (t', fvs', len2, vd, _, _) =
				       (LambdaVar.Tbl.lookup venv v'')
				       handle _ =>
				          bug "Tapp var not found"
			       in
	                           if ((len1 = len2) orelse (vd = 0))then
	                             let
					   val newvar = mkv()
					   val hd' = (Tapp, newvar, TAPP(v,tycs))
					   fun f(x) = loopc env (VAR x)
					   val (exp, fvs) = case fvs' of
					       [] => (RET([VAR newvar]), nil)
				    	      | _ => let val fvs'' = map f fvs'
					                 val (r1, r2, r3, r4) = foldr comb (nil,nil,nil,nil) fvs''
				                     in
					                 (writeApp(VAR newvar, r1), r3)
				                     end
			             in
					  ( tappLifted := !tappLifted + 1;
				            (exp, nt, fv'@fvs, [hd']) )
			             end
	                          else
				      ( welltapped := false;
				        tappLifted := 0;
				        raise PartialTypeApp )
			       end
			 | _ => (e, nt, fv', hd)
	   end
	 | loope (e as TFN((tfk,v,tvs,e1),e2), env as Ienv(venv,fenvs), d, ad) =
	   (case d of
	       0 =>
		   let
		       val (e1', nt', fv', hd') = lift(e1, env, DI.next td, d, ad, true)
		       val ks = map (fn (t,k) => k) tvs
		       val nt = LD.ltc_poly(ks, nt')

	           (*  Hack for Tapp.Stores the number of tvs instead of td  *)

		       val _ = addEnv(env, [v], [nt], fv', (List.length tvs), d, NOABS)

		       val (e2', nt'', fv'', hd'') = loope(e2, env, d, ad)
		   in
		       (TFN((tfk,v,tvs,e1'),e2'), nt'', fv'@fv'', hd'@hd'')
		   end
	     | _ =>
		   let
		       val env' = pushFenv(env)
		       val (e1', nt', fvs, hd) = lift(e1, env', DI.next td, d, DI.next ad, true)
		       val freevars = getFreeVar(fvs, env')
		       val ks = map (fn (t,k) => k) tvs
		       val nt = LD.ltc_poly(ks, nt')

	          (* Hack for Tapp. Stores the number of tvs *)

		       val _ = addEnv(env, [v], [nt], fvs, (List.length tvs), d, NOABS)

		       val(e2', nt'', fvs', hd') = loope(e2, env, d, ad)
		       val exp = writeLambda(freevars, e1')
		       val exp' = writeHeader(hd, exp)
		       val hd = (Tfn, v, TFN((tfk,v,tvs,exp'),RET [])) :: hd'
		   in
		       (e2', nt'', fvs', hd)
		   end )
         | loope(SWITCH(v, a, cels, eopt), env, d, ad) =
	    let
		val (v', nt, fv, hd) = loopc env v
		fun f(c,e) =
		    let
			val _ =
			    case c of
				PL.DATAcon((_, _, lt), ts, v) =>
				addEnv(env, [v], [arglty(lt,ts)], nil, td, d, ABS)
			      | _ => [()]
			val (e', nt', fvs, hds) = loope(e, env, d, ad)
		    in
			((c,e'), nt', fvs, hds)
		    end
		val ls = map f cels
		val (cels', nt', fvs', hds') = foldr comb (nil,nil,nil,nil) ls
		val (exp, t, f, h) =
		case eopt of
		    NONE => (SWITCH(v',a,cels',eopt), List.hd nt', fv@fvs', hd@hds')
		  | SOME(eopt') =>
			let
			    val (eopt'', nt'', fvs'', hd'') = loope(eopt', env, d, ad)
			in
			    (SWITCH(v',a,cels',SOME(eopt'')), List.hd nt', fv@fvs'@fvs'', hd@hds'@hd'')
			end

	    in
		(exp, t, f, h)
	    end
	 | loope (CON(dcons,tcs,vl,v,e), env, d, ad) =
	    let
		val (s, cr, lt) = dcons
		val (cr', fv) = lpcon env cr
		val nt = reslty(lt, tcs)

		val (vl', nt', fvs', hd') = loopc env vl

		val _ = addEnv(env, [v], [nt], nil, td, d, true)
		val (e'', nt'', fvs'', hd'') = loope(e, env, d, ad)
	     in
		(CON((s, cr', lt),tcs,vl',v,e''), nt'', fv@fvs'@fvs'', hd'@hd'')
	     end
	 | loope (RECORD(rk,vls,v,e), env, d, ad) =
	     let
		 val ls = map (loopc env) vls
		 val (vls', nt', fvs', hd') = foldr comb (nil,nil,nil,nil) ls
		 val nt = LE.ltc_rkind(rk, nt')

		 val _ = addEnv(env, [v], [nt], fvs', td, d, true)
		 val (e', nt'', fvs'', hd'') = loope(e, env, d, ad)
	     in
		 (RECORD(rk, vls', v, e'), nt'', fvs'@fvs'', hd'@hd'')
	     end
	 | loope (SELECT(v,i,l,e), env, d, ad) =
	     let val (v', nt', fvs', hd') = loopc env v
		 val nt = LE.ltd_rkind(nt', i)
		 val _ = addEnv(env, [l], [nt], fvs', td, d, true)
		 val (e', nt'', fvs'', hd'') = loope(e, env, d, ad)
	     in
		 (SELECT(v',i,l,e'), nt'', fvs'@fvs'', hd'@hd'')
	     end
	 | loope (RAISE(v,ls), env, d, ad) =
	     let val (v', nt', fvs', hd') = loopc env v
	     in  (RAISE(v',ls), ls, fvs', hd')
	     end
	 | loope (HANDLE(e,v), env, d, ad) =
	     let val (v', nt', fvs', hd') = loopc env v
		 val (e', nt'', fvs'', hd'') = loope(e, env, d, ad)
	     in
		 (HANDLE(e',v'), nt'', fvs'@fvs'', hd'@hd'')
	     end
	 | loope (BRANCH(pr,vl,e1,e2), env, d, ad) =
	     let val ls = map (loopc env) vl
		 val (vls', nt', fvs', hd') = foldr comb (nil,nil,nil,nil) ls
		 val (e1', nt'', fvs'', hd'') = loope(e1, env, d, ad)
		 val (e2', nt''', fvs''', hd''') = loope(e2, env, d, ad)
	     in
		 (BRANCH(pr,vls',e1',e2'), nt''', fvs'@fvs''@fvs''', hd'@hd''@hd''')
	     end
	 | loope (PRIMOP(pr,vl,l,e), env, d, ad) =
	     let
		 val ls = map (loopc env) vl
		 val (vls', nt', fvs', hd') = foldr comb (nil,nil,nil,nil) ls
		 val (_, _, lt, ts) = pr
		 val nt = reslty(lt, ts)

		 val _ = addEnv(env, [l], [nt], fvs', td, d, ABS)
		 val (e', nt'', fvs'', hd'') = loope(e, env, d, ad)
	     in
		 (PRIMOP(pr,vls',l,e'), nt'', fvs'@fvs'', hd'@hd'')
	     end
	 | loope(e as FIX([({cconv = FR.CC_FCT, ...}, v, lvs, e1)],e2), env, d, ad) =
	   let
	       val vs = map #1 lvs
	       val ts = map #2 lvs
	       val _ = if d > 0 then
		          wellfixed := false
		       else
			   ()
	       val _ = addEnv(env, vs, ts, nil, td, 0, NOABS)
	       val (e', nt', fvs', hd') = loope(e1, env, 0, DI.next ad)
	       val nt = LE.ltc_fkfun(fkfct, ts, nt')
	       val _ = addEnv(env, [v], [nt], fvs', td, 0, NOABS)
	       val (e'', nt'', fvs'', hd'') = loope(e2, env, d, ad)
	   in
	       (FIX([(fkfct, v, lvs, e')], e''), nt'', fvs'@fvs'', hd'@hd'')
	   end
	 | loope(e as FIX([(fk, v, lvs, e1)], e2), env, d, ad) =
	   (case fk of
	       {isrec = NONE, cconv = FR.CC_FUN _, ...} =>
	       let
		   val vs = map #1 lvs
		   val ts = map #2 lvs
		   val _ = addEnv(env, vs, ts, nil, td, DI.next d, ABS)
		   val (e', nt', fvs', hd') = loope(e1, env, DI.next d, DI.next ad)
		   val nt = LE.ltc_fkfun(fk, ts, nt')
		   val abs = if d > 0 then true else false
		   val _ = addEnv(env, [v], [nt], fvs', td, d, ABS)
		   val (e'', nt'', fvs'', hd'') = loope(e2, env, d, ad)
		   val ne' = FIX([(fk,v,lvs,e')], e'')
		   val (ne,hd) = case d of
		       0 => (writeHeader(hd'@hd'', ne'), nil)
		     | _ => (ne', hd'@hd'')
	       in (ne, nt'', fvs'@fvs'', hd)
	       end
	     | {isrec = SOME(rts,_), cconv = FR.CC_FUN _, ...} =>
	       let
		   val vs = map (#1) lvs
		   val ts = map (#2) lvs
		   val _ = addEnv(env, [v], [LE.ltc_fkfun(fk, ts, rts)], nil,
td, DI.next d, ABS)
		   val _ = addEnv(env, vs, ts, nil, td, DI.next d, ABS)
		   val (e', nt', fvs', hd') = loope(e1, env, DI.next d, DI.next ad)

		   (* Check to see that the new value is inserted *)

		   val _ = addEnv(env, [v], [LE.ltc_fkfun(fk, ts, rts)], nil,
td, d, ABS)
		   (* The depth is changed for correct behaviour *)

		   val (e'', nt'', fvs'', hd'') = loope(e2, env, d, ad)
		   val ne' = FIX([(fk,v,lvs,e')], e'')
		   val (ne,hd) = case d of
		       0 => (writeHeader(hd'@hd'', ne'), nil)
		     | _ => (ne', hd'@hd'')
	       in (ne, nt'', fvs'@fvs'', hd)
	       end
	     | _ => bug "unexpected fundec in main loop" )
	 | loope(e as FIX(fds, e2), env, d, ad) =
	   let
	       fun h d' ((fk as {isrec = SOME(rts,_), ...}, f, lvs, e1):fundec) =
		   addEnv(env, [f], [LE.ltc_fkfun(fk, map #2 lvs, rts)], nil, td, d', ABS)
		 | h d fk = bug "unexpected non-recursive fkind in loop"
	       fun g((fk, f, lvs, e):fundec) =
		   let
		       val _ = addEnv(env, map #1 lvs, map #2 lvs, nil, td, DI.next d, ABS)
		       val (e', nt', fvs', hd') = loope(e, env, DI.next d, DI.next ad)
		   in
		       ( (fk, f, lvs, e'), [LE.ltc_fkfun(fk, map #2 lvs, nt')], fvs', hd')
		   end
	       val _ = map (h (DI.next d)) fds
	       val rets = map g fds
	       val (fds, nts, fvs, hds) = foldr comb (nil,nil,nil,nil) rets

	       (* Check to see that the correct value is inserted *)

	       val _ = map (h d) fds
	       val (e'', nt'', fvs'', hd'') = loope(e2, env, d, ad)
	       val ne' = FIX(fds, e'')
	   in
	       case d of
		   0 => (writeHeader(hds@hd'', ne'), nt'', fvs@fvs'', nil)
		 | _ => (ne', nt'', fvs@fvs'', hds@hd'')
	   end

       in loope(e, env, d, ad)
       end


fun typeLift fdec:fundec =
    (* if !Control.CG.lifttype then *)
	case fdec of
	    (fk as {cconv = FR.CC_FCT, ...}, v, vts, e) =>
		let
		    val env = initInfoEnv()
		    val d = 0 (* DI.top ?? *)
		    val td = 0 (* DI.top ?? *)
		    val ad = 0 (* DI.top ?? *)
		    val rename = false
		    val vs = map #1 vts
		    val ts = map #2 vts
		    val _ = addEnv(env, vs, ts, nil, td, d, NOABS)
		    val (ne, _, _, _) = ( lift(e, env, td, d, ad, rename) )
					   handle PartialTypeApp =>
					   ( print "\n*** No Typelifting ";
					     print " Partial Type App ***\n";
					             (e, nil, nil, nil) )
		    val _ = if !wellfixed then
			       ()
			    else
				() (* print "\n *** Functor at d > 0 *** \n" *)
	            val _ = if !CTRL.saytappinfo then
		    	       (print "\n *** No. of Tapps lifted ";
		    	       print (" " ^ (Int.toString (!tappLifted)) ^ " \n") )
			    else
				()
		in
		    ( tappLifted := 0;
		      wellfixed := true;
		      welltapped := true;
		      (fk, v, vts, ne) )
		end
	  | _ => bug "non FCT program in Lift"
    (* else fdec *)

end (* top local *)
end (* structure Lift *)


