(* chkflint.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* FLINT Type Checker *)

signature CHKFLINT = sig

(** which set of the typing rules to use while doing the typecheck *)
type typsys (* currently very crude *)

val checkTop : FLINT.fundec * typsys -> bool
val checkExp : FLINT.lexp * typsys -> bool

end (* signature CHKFLINT *)

structure ChkFlint : CHKFLINT =
struct

(** which set of the typing rules to use while doing the typecheck *)
type typsys = bool (* currently very crude *)

local
  structure LT = Lty
  structure FR = FunRecMeta
  structure LK = LtyKernel
  structure LD = LtyDef
  structure LB = LtyBasic
  structure LE = LtyExtern
  structure LV = LambdaVar
  structure DA = Access
  structure DI = DebIndex
  structure PF = PrintFlint
  structure PO = Primop
  structure S  = LV.Set
  structure PL = PLambda
  open FLINT

fun bug s = ErrorMsg.impossible ("ChkFlint: "^s)
val say = Control_Print.say
val anyerror = ref false

(****************************************************************************
 *                         BASIC UTILITY FUNCTIONS                          *
 ****************************************************************************)

fun foldl2 (f,a,xs,ys,g) = let
  val rec loop =
   fn (a, nil, nil) => a
    | (a, x::xs, y::ys) => loop (f (x,y,a), xs, ys)
    | (a, xs', _) => g (a, xs', length xs, length ys)
  in loop (a,xs,ys) end

fun simplify (le,0) = RET [STRING "<...>"]
  | simplify (le,n) =
      let fun h le = simplify (le, n-1)
          fun h1 (fk,v,args,le) = (fk, v, args, h le)
          fun h2 (tfk,v,tvs,le) = (tfk, v, tvs, h le)
       in case le
           of LET (vs,e1,e2) => LET (vs, h e1, h e2)
            | FIX (fdecs,b) => FIX (map h1 fdecs, h b)
            | TFN (tdec,e) => TFN (h2 tdec, h e)
            | SWITCH (v,l,dc,opp) =>
                let fun g (c,x) = (c, h x)
                    val f = fn SOME y => SOME (h y) | NONE => NONE
                 in SWITCH (v, l, map g dc, f opp)
                end
	    | CON (dc,tcs,vs,lv,le) => CON (dc, tcs, vs, lv, h le)
	    | RECORD (rk,vs,lv,le) => RECORD (rk, vs, lv, h le)
	    | SELECT (v,n,lv,le) => SELECT (v, n, lv, h le)
            | HANDLE (e,v) => HANDLE (h e, v)
	    | PRIMOP (po,vs,lv,le) => PRIMOP (po, vs, lv, h le)
            | _ => le
      end (* end of simplify *)

(** utility functions for printing *)
val tkPrint = say o LB.tk_print
val tcPrint = say o LB.tc_print
val ltPrint = say o LB.lt_print
fun lePrint le = PF.printLexp (simplify (le, 3))
val svPrint = PF.printValue

(* error : lexp * (unit -> 'a) -> 'a *)
fun error (le, errfn) =
    (anyerror := true;
     say "\n************************************************************\
         \\n**** FLINT type checking failed: ";
     errfn () before (say "\n** term:\n"; lePrint le))

(* errMsg : lexp * string * 'a -> 'a *)
fun errMsg (le,s,r) = error (le, fn () => (say s; r))

fun catchExn f (le,g) =
  f () handle ex => error
    (le, fn () => g () before say ("\n** exception " ^ exnName ex ^ " raised"))

(*** a hack for type checkng ***)
fun laterPhase postReify = postReify

fun check phase envs lexp = let
    (* imperative table -- keeps track of already bound variables,
     * so we can tell if a variable is re-bound (which should be
     * illegal).  Note that lvars and tvars actually share the same
     * namespace!   --league, 11 April 1998
     *)
  val definedLvars = ref S.empty
  fun lvarDef le (lvar: LV.lvar) =
      if S.member(!definedLvars, lvar) then
          errMsg (le, ("lvar " ^ (LambdaVar.prLvar lvar) ^ " redefined"), ())
      else
          definedLvars := S.add(!definedLvars, lvar)

  val ltEquiv = LK.lt_eqv
  val ltTAppChk =
    if !FLINT_Control.checkKinds then LE.lt_inst_chk_gen()
    else fn (lt,ts,_) => LE.lt_inst(lt,ts)

  fun constVoid _ = LB.ltc_void
  val (ltString,ltExn,ltEtag,ltVector,ltWrap,ltBool) =
    if laterPhase phase then
      (LB.ltc_string, LB.ltc_void, constVoid, constVoid, constVoid,
       LB.ltc_void)
    else
      (LB.ltc_string, LB.ltc_exn, LB.ltc_etag, LD.ltc_tyc o LB.tcc_vector,
       LD.ltc_tyc o LD.tcc_box, LB.ltc_bool)

  fun prMsgLt (s,lt) = (say s; ltPrint lt)

  fun prList f s t = let
    val rec loop =
     fn [] => say "<empty list>\n"
      | [x] => (f x; say "\n")
      | x::xs => (f x; say "\n* and\n"; loop xs)
    in say s; loop t end

  fun print2Lts (s,s',lt,lt') = (prList ltPrint s lt; prList ltPrint s' lt')

  fun ltMatch (le,s) (t,t') =
    if ltEquiv (t,t') then ()
    else error
      (le, fn () =>
	      (prMsgLt (s ^ ": Lty conflict\n** types:\n", t);
	       prMsgLt ("\n** and\n", t')))

  fun ltsMatch (le,s) (ts,ts') =
    foldl2 (fn (t,t',_) => ltMatch (le,s) (t,t'),
	    (), ts, ts',
	    fn (_,_,n,n') => error
	       (le,
		fn () => print2Lts
	          (concat [s, ": type list mismatch (", Int.toString n, " vs ",
			   Int.toString n', ")\n** expected types:\n"],
		   "** actual types:\n",
		   ts, ts')))

  local
    fun ltFnAppGen opr (le,s,msg) (t,ts) =
      catchExn
        (fn () => let val (xs,ys) = opr (LE.ltd_fkfun t)
                   in ltsMatch (le,s) (xs,ts); ys
                  end)
	(le, fn () => (prMsgLt (s ^ msg ^ "\n** type:\n", t); []))
  in
  fun ltFnApp (le,s) =
      ltFnAppGen (fn x => x) (le,s,": Applying term of non-arrow type")
  fun ltFnAppR (le,s) =
      ltFnAppGen (fn (x,y) => (y,x)) (le,s,": Rev-app term of non-arrow type")
  end

  fun ltTyApp (le,s) (lt,ts,kenv) =
    catchExn
      (fn () => ltTAppChk (lt,ts,kenv))
      (le,
       fn () =>
	  (prMsgLt (s ^ ": Kind conflict\n** function Lty:\n", lt);
	   prList tcPrint "\n** argument Tycs:\n" ts;
	   []))

  fun ltArrow (le,s) (cconv,alts,rlts) =
    (case cconv
      of FR.CC_FCT => LD.ltc_fct (alts,rlts)
       | FR.CC_FUN raw =>
           (catchExn
             (fn () => LD.ltc_arrow (raw,alts,rlts))
             (le,
              fn () =>
              (print2Lts
   	        (s ^ ": deeply polymorphic non-functor\n** parameter types:\n",
  	         "** result types:\n",
	         alts, rlts);
	      LB.ltc_void))))

  (* typeInEnv : LD.tkindEnv * LB.ltyEnv * DI.depth -> lexp -> lty list *)
  fun typeInEnv (kenv,venv,d) = let
    fun extEnv (lv,lt,ve) = LB.ltInsert (ve,lv,lt,d)
    fun bogusBind (lv,ve) = extEnv (lv,LB.ltc_void,ve)
    fun typeIn venv' = typeInEnv (kenv,venv',d)
    fun typeWith (v,t) = typeIn (LB.ltInsert (venv,v,t,d))
    fun mismatch (le,s) (a,r,n,n') = errMsg
	(le,
	 concat [s, ": binding/result list mismatch\n** expected ",
		 Int.toString n, " elements, got ", Int.toString n'],
	 foldl bogusBind a r)

    fun typeof le =
	let fun typeofVar lv =
		(case LB.ltLookup (venv,lv,d)
		   of NONE =>
		        errMsg (le, "Unbound Lvar " ^ LV.lvarName lv, LB.ltc_void)
		    | SOME lty => lty)

	    fun typeofVal (VAR lv) = typeofVar lv
	      | typeofVal (INT{ty, ...}) = LB.ltc_num ty
	      | typeofVal (WORD{ty, ...}) = LB.ltc_num ty
	      | typeofVal (REAL _) = LB.ltc_real
	      | typeofVal (STRING _) = LB.ltc_string
	      (* REAL64: need more cases *)

	    fun typeofFn ve (_,lvar,vts,eb) = let
		  fun split ((lv,t), (ve,ts)) = (
			lvarDef le lv;
			(LB.ltInsert (ve,lv,t,d), t::ts))
		  val (ve',ts) = foldr split (ve,[]) vts
		  in
		    lvarDef le lvar;
		    (ts, typeIn ve' eb)
		  end

      (* There are lvars hidden in Access.conrep, used by dcon.
       * These functions just make sure that they are defined in the
       * current environemnent; we don't bother to typecheck them properly
       * because supposedly conrep will go away...
       *)
      fun checkAccess (DA.LVAR v) = ignore (typeofVar v)
        | checkAccess (DA.PATH (a,_)) = checkAccess a
        | checkAccess _ = ()

      fun checkConrep (DA.EXN a) =
              checkAccess a
        | checkConrep (DA.SUSP (SOME (a1,a2))) =
              (checkAccess a1;
               checkAccess a2)
        | checkConrep _ =
              ()

      fun chkSnglInst (fp as (le,s)) (lt,ts) =
	if null ts then lt
	else case ltTyApp fp (lt,ts,kenv)
	   of [] => LB.ltc_unit
	    | [lt] => lt
	    | lts => errMsg
		(le,
		 concat [s, ": inst yields ", Int.toString (length lts),
			 " results instead of 1"],
		 LB.ltc_void)
      fun typeWithBindingToSingleRsltOfInstAndApp (s,lt,ts,vs,lv) e = let
	val fp = (le,s)
	val lt = case ltFnApp fp (chkSnglInst fp (lt,ts), map typeofVal vs)
	   of [lt] => lt
            | _ => errMsg
               (le,
                concat [s, ": primop/dcon must return single result type "],
                LB.ltc_void)
(*
	    | [] => LB.ltc_unit
	    | lts => LD.ltc_tuple lts
	            (*** until PRIMOPs start returning multiple results... ***)
*)
	in typeWith (lv,lt) e
	end

      fun matchAndTypeWith (s,v,lt,lt',lv,e) =
	(ltMatch (le,s) (typeofVal v, lt); typeWith (lv, lt') e)

      in case le
       of RET vs => map typeofVal vs
	| LET (lvs,e,e') =>
          (app (lvarDef le) lvs;
           typeIn (foldl2 (extEnv, venv, lvs,
                           typeof e, mismatch (le,"LET"))) e')
	| FIX ([],e) =>
	  (say "\n**** Warning: empty declaration list in FIX\n"; typeof e)
	| FIX ((fd as (fk as {isrec=NONE,cconv,...},
                       lv, _, _)) :: fds', e) => let
	    val (alts,rlts) = typeofFn venv fd
	    val lt = ltArrow (le,"non-rec FIX") (cconv,alts,rlts)
	    val ve = extEnv (lv,lt,venv)
	    val venv' =
	      if null fds' then ve
	      else errMsg
		(le,
		 "multiple bindings in FIX, not all recursive",
		 foldl (fn ((_,lv,_,_), ve) => bogusBind (lv,ve)) ve fds')
	    in typeIn venv' e
	    end
	| FIX (fds,e) => let
            val isfct = false
	    fun extEnv (({cconv=FR.CC_FCT, ...}, _, _, _), _) =
                  bug "unexpected case in extEnv"
              | extEnv (({isrec,cconv,...}, lv, vts, _) : fundec, ve) =
	      case (isrec, isfct)
	       of (SOME (lts,_), false) => let
		    val lt = ltArrow (le,"FIX") (cconv,
                                                 map #2 vts, lts)
		    in LB.ltInsert (ve,lv,lt,d)
		    end
		| _ => let
		    val msg =
		      if isfct then "recursive functor "
		      else "a non-recursive function "
		    in errMsg (le, "in FIX: " ^ msg ^ LV.lvarName lv, ve)
		    end
	    val venv' = foldl extEnv venv fds
	    fun chkDcl (({isrec = NONE, ...}, _, _, _) : fundec) = ()
	      | chkDcl (fd as ({isrec = SOME (lts,_), ...}, _, _, _)) = let
		in ltsMatch (le,"FIX") (lts, #2 (typeofFn venv' fd))
		end
	    in
	      app chkDcl fds;
	      typeIn venv' e
	    end
	| APP (v,vs) => ltFnApp (le,"APP") (typeofVal v, map typeofVal vs)
	| TFN ((tfk,lv,tks,e), e') => let
            fun getkind (tv,tk) = (lvarDef le tv; tk)
	    val ks = map getkind tks
	    val lts = typeInEnv (LT.tkInsert (kenv,ks), venv, DI.next d) e
	    in
                lvarDef le lv;
                typeWith (lv, LD.ltc_poly (ks,lts)) e'
	    end
	| TAPP (v,ts) => ltTyApp (le,"TAPP") (typeofVal v, ts, kenv)
	| SWITCH (_,_,[],_) => errMsg (le,"empty SWITCH",[])
	| SWITCH (v, _, ce::ces, lo) => let
	    val selLty = typeofVal v
	    fun g lt = (ltMatch (le,"SWITCH branch 1") (lt,selLty); venv)
	    fun brLts (c,e) = let
	      val venv' = case c
		 of PL.DATAcon ((_,conrep,lt), ts, v) => let
                      val _ = checkConrep conrep
		      val fp = (le,"SWITCH DECON")
		      val ct = chkSnglInst fp (lt,ts)
		      val nts = ltFnAppR fp (ct, [selLty])
		      in
                          lvarDef le v;
                          foldl2 (extEnv, venv, [v], nts, mismatch fp)
		      end
		  | PL.INTcon{ty, ...} => g (LB.ltc_num ty)
		  | PL.WORDcon{ty, ...} => g (LB.ltc_num ty)
		  | PL.STRINGcon _ => g ltString
	      in typeIn venv' e
	      end
	    val ts = brLts ce
	    fun chkBranch (ce,n) =
	      (ltsMatch (le, "SWITCH branch " ^ Int.toString n) (ts, brLts ce);
	       n+1)
	    in
	      foldl chkBranch 2 ces;
	      case lo
	       of SOME e => ltsMatch (le,"SWITCH else") (ts, typeof e)
		| NONE => ();
	      ts
	    end
	| CON ((_,conrep,lt), ts, u, lv, e) =>
            (checkConrep conrep;
             lvarDef le lv;
             typeWithBindingToSingleRsltOfInstAndApp ("CON",lt,ts,[u],lv) e)
	| RECORD (rk,vs,lv,e) => let
	    val lt = case rk
	       of FR.RK_VECTOR t => let
		    val lt = LD.ltc_tyc t
		    val match = ltMatch (le,"VECTOR")
		    in
		      app (fn v => match (lt, typeofVal v)) vs;
		      ltVector t
		    end
		| FR.RK_TUPLE =>
		  if null vs then LB.ltc_unit
		  else let
		    fun chkMono v = let val t = typeofVal v
			in
			  if LD.ltp_fct t orelse LD.ltp_poly t then
			    error (le, fn () =>
			        prMsgLt
				  ("RECORD: poly type in mono record:\n",t))
			  else ();
			  t
			end
		    in LD.ltc_tuple (map chkMono vs)
		    end
		| FR.RK_STRUCT => LD.ltc_str (map typeofVal vs)
	    in
                lvarDef le lv;
                typeWith (lv,lt) e
	    end
	| SELECT (v,n,lv,e) => let
	    val lt = catchExn
		(fn () => LE.lt_select (typeofVal v, n, "chkflint.sml#393"))
		(le,
		 fn () =>
		    (say "SELECT from wrong type or out of range"; LB.ltc_void))
	    in
                lvarDef le lv;
                typeWith (lv,lt) e
	    end
	| RAISE (v,lts) => (ltMatch (le,"RAISE") (typeofVal v, ltExn); lts)
	| HANDLE (e,v) => let val lts = typeof e
	    in ltFnAppR (le,"HANDLE") (typeofVal v, lts); lts
	    end
	| BRANCH ((_,_,lt,ts), vs, e1, e2) =>
            let val fp = (le, "BRANCH")
                val lt =
	          case ltFnApp fp (chkSnglInst fp (lt,ts), map typeofVal vs)
	           of [lt] => lt
                    | _ => errMsg
                            (le,
                             "BRANCK : primop must return single result ",
                             LB.ltc_void)
                val _ = ltMatch fp (lt, ltBool)
                val lts1 = typeof e1
                val lts2 = typeof e2
             in ltsMatch fp (lts1, lts2);
                lts1
            end
        | PRIMOP ((_,PO.WCAST,lt,[]), [u], lv, e) =>
            (*** a hack: checked only after reifY is done ***)
            if laterPhase phase then
              (lvarDef le lv;
               case LD.ltd_fct lt
                of ([argt], [rt]) =>
                      (ltMatch (le, "WCAST") (typeofVal u, argt);
                       typeWith (lv, rt) e)
                 | _ => bug "unexpected WCAST in typecheck")
            else bug "unexpected WCAST in typecheck"
	| PRIMOP ((dc,_,lt,ts), vs, lv, e) => let
              (* There are lvars hidden inside dicts, which we didn't check
               * before.  This is a first-order check that they at least
               * are bound to something; for now we don't care about their
               * types.  (I'm not sure what the rules should look like)
               *   --league, 10 april 1998.
               *)
              fun checkDict (SOME {default, table}) =
                    (typeofVar default;
                     app (ignore o typeofVar o #2) table)
                | checkDict (NONE : dict option) = ()
          in
              checkDict dc;
              lvarDef le lv;
              typeWithBindingToSingleRsltOfInstAndApp ("PRIMOP",lt,ts,vs,lv) e
          end
(*
	| GENOP (dict, (_,lt,ts), vs, lv, e) =>
	  (* verify dict ? *)
	  typeWithBindingToSingleRsltOfInstAndApp ("GENOP",lt,ts,vs,lv) e
	| ETAG (t,v,lv,e) =>
	  matchAndTypeWith ("ETAG", v, ltString, ltEtag (LD.ltc_tyc t), lv, e)
	| WRAP (t,v,lv,e) =>
	  matchAndTypeWith ("WRAP", v, LD.ltc_tyc t, ltWrap t, lv, e)
	| UNWRAP (t,v,lv,e) =>
	  matchAndTypeWith ("UNWRAP", v, ltWrap t, LD.ltc_tyc t, lv, e)
*)
      end
    in typeof end

in
  anyerror := false;
  ignore (typeInEnv envs lexp);
  !anyerror
end

in (* loplevel local *)

(****************************************************************************
 *  MAIN FUNCTION --- val checkTop : FLINT.fundec * typsys -> bool          *
 ****************************************************************************)
fun checkTop ((fkind, v, args, lexp) : fundec, phase: typsys) = let
  val ve = foldl (fn ((v,t), ve) => LB.ltInsert (ve,v,t,DI.top)) LB.initLtyEnv args
  val err = check phase (LT.initTkEnv, ve, DI.top) lexp
  val err = case fkind
     of {cconv=FR.CC_FCT,...} => err
      | _ => (say "**** Not a functor at top level\n"; true)
  in err end

val checkTop =
  Stats.doPhase (Stats.makePhase "Compiler 051 FLINTCheck") checkTop

(****************************************************************************
 *  MAIN FUNCTION --- val checkExp : FLINT.lexp * typsys -> bool            *
 *  (currently unused?)                                                     *
 ****************************************************************************)
fun checkExp (le,phase) = check phase (LT.initTkEnv, LB.initLtyEnv, DI.top) le

end (* toplevel local *)
end (* structure ChkFlint *)
