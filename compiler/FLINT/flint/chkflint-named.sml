(* chkflint-named.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* FLINT Type Checker for FLINT with "named" type variables *)

signature CHKFLINT_NAMED =
sig

  val checkTop : FLINT.prog * bool -> bool
  (* bool arg indicates whether last FLINT optimization phase was reify or later
   * (flintkind = FK_REIFY) *)

  val checkExp : FLINT.lexp * bool -> bool

end (* signature CHKFLINT *)

structure ChkFlintNamed : CHKFLINT_NAMED =
struct

local structure LE = LtyExtern
      structure LV = LambdaVar
      structure DA = Access
      structure DI = DebIndex
      structure PO = Primop
      structure S  = LV.Set
      structure PF = PrintFlint
      structure PP = PrettyPrint
      structure PPF = PPFlint
      open FLINT

fun bug s = ErrorMsg.impossible ("ChkFlint: "^s)
val say = Control_Print.say
val anyerror = ref false

(* pretty printing functions (linewidth?, newlines?) *)

val with_pp_nl  = PP.with_default_pp  (* always appends newline *)

val with_pp  = PP.with_default_pp_sans  (* omits final newline *)

fun ppTKind tkind =
      with_pp (fn ppstrm => PPLty.ppTKind 100 ppstrm tkind)
fun ppTyc tyc =
      with_pp (fn ppstrm => (PPLty.ppTyc 100 ppstrm tyc))
fun ppLty lty =
      with_pp (fn ppstrm => (PPLty.ppLty 100 ppstrm lty))
fun ppLexp (d: int) lexp =
      with_pp (fn ppstrm => (PPF.ppLexp d ppstrm lexp))
fun ppValue value =
      with_pp (fn ppstrm => (PP.string ppstrm (PF.valueToString value)))
fun prMsgLty (msg, lty) =
      with_pp (fn ppstrm => (PP.string ppstrm msg; PPF.ppLexp d ppstrm lexp))

fun prList (prfun: 'a -> unit) (s: string) (ts: 'a list) =
    let fun loop [] = say "<empty list>\n"
	  | loop [x] = (prfun x; say "\n")
	  | loop (x::xs) = (prfun x; say "\n* and\n"; loop xs)
     in say s; loop ts
    end

fun print2Lts (s,s',lt,lt') = (prList ppLty s lt; prList ppLty s' lt')


(****************************************************************************
 *                         BASIC UTILITY FUNCTIONS                          *
 ****************************************************************************)

(* floldl2 = foldlEq -- raises UnequalLengths is list args are of unequal lengths *)
val foldl2 = ListPair.foldlEq

(* error : lexp * (unit -> 'a) -> 'a *)
fun error (lexp, errfn) =
    (anyerror := true;
     say "\n************************************************************\
         \\n**** FLINT type checking failed: ";
     errfn () before (say "\n** term:\n"; ppLexp  3 lexp))

(* errMsg : lexp * string * 'a -> 'a *)
fun errMsg (lexp, msg, result) = error (lexp, fn () => (say msg; result))

fun catchExn f (le, g) =
    f () handle ex =>
	   error (le, fn () => g () before say ("\n** exception " ^ exnName ex ^ " raised"))

type depth = DebIndex.depth   (* rep = int *)
type kindenv = LE.tkindEnv    (* = tkind list list -- lty.sml *)
type varenv = LE.ltyEnv       (* = (lty * DI.depth) LambdaVar.Map.map -- ltybasic.sml *)
type envs = kindenv * varenv * depth
		   
fun check (postReify: bool) (envs: envs) lexp =
    let val definedLvars = ref S.empty
        (* imperative table -- keeps track of already bound variables,
         * so we can tell if a variable is re-bound (which is an error).
         * Note that lvars and tvars actually share the same namespace!
         * --league, 1998-4-11 *)

	(* defineLvar : F.lexp -> LV.lvar -> LV.lvar *)
	fun defineLvar le (lvar:lvar) =
	    if S.member (!definedLvars, lvar)
	    then errMsg (le, ("lvar " ^ (LambdaVar.prLvar lvar) ^ " redefined"), ())
	    else definedLvars := S.add(!definedLvars, lvar)

	val ltTAppChk =
	  if !FLINT_Control.checkKinds then LE.lt_inst_chk_gen()
	  else fn (lt,ts,_) => LE.lt_inst(lt,ts)

	val (ltString, ltExn, ltEtag, ltVector, ltWrap, ltBool) =
	    if postReify
	    then (LE.ltc_string, LE.ltc_void, LE.ltc_void, LE.ltc_void, LE.ltc_void, LE.ltc_void)
	    else (LE.ltc_string, LE.ltc_exn, LE.ltc_etag, LE.ltc_tyc o LE.tcc_vector,
		  LE.ltc_tyc o LE.tcc_box, LE.ltc_bool)

	(* ltMatch : (F.lexp * string) -> (lty * lty) -> unit *)
	fun ltMatch (le,s) (t,t') =
	    if LE.lt_eqv (t,t') then ()      (* lt_eqv : lty * lty -> bool  -- ltyKernel.sml *)
	    else error (le,
			fn () =>
		          (prMsgLty (s ^ ": Lty conflict\n** types:\n", t);
		           prMsgLty ("\n** and\n", t')))

	fun ltsMatch (le,s) (ts,ts') =
	    ListPair.appEq (ltMatch (le,s)) (ts, ts')
	    handle UnequalLengths =>
	      let val len_ts = Int.toString (length ts)
		  val len_ts' = Int.toString (length ts')
		  fun errFn () =
		      print2Lts
			(concat [s, ": type list mismatch (", len_ts,
				 " vs ", len_ts', ")\n** expected types:\n"],
			 "** actual types:\n",
			 ts, ts')
	       in error (le, errFn)
	      end

	local
	  fun ltFnAppGen opr (le,s,msg) (t,ts) =
	      catchExn
		(fn () => let val (xs,ys) = opr (LE.ltd_fkfun t)
			   in ltsMatch (le,s) (xs,ts); ys
			  end)
	        (le, fn () => (prMsgLty (s ^ msg ^ "\n** type:\n", t); []))
	in
	  fun ltFnApp (le,s) =
	      ltFnAppGen (fn x => x) (le,s,": Applying term of non-arrow type")
	  fun ltFnAppR (le,s) =
	      ltFnAppGen (fn (x,y) => (y,x)) (le,s,": Rev-app term of non-arrow type")
	end

	fun ltTyApp (le,s) (lt,ts,kenv) =
	    catchExn
	      (fn () => ltTAppChk (lt, ts, kenv))
	      (le,
	       fn () =>
		  (prMsgLty (s ^ ": Kind conflict\n** function Lty:\n", lt);
		   prList ppTyc "\n** argument Tycs:\n" ts;
		   []))

        (* ltArrow : (lexp * string) -> (cconv * lty list * lty list) -> lty *)
	fun ltArrow (le,s) (cconv, dom_ltys, rlts) =
	    (case cconv
	      of CC_FCT => LE.ltc_fct (dom_ltys,ran_ltys)
	       | CC_FUN fflag =>
		   (catchExn
		     (fn () => LE.ltc_arrow (fflag, dom_ltys, ran_ltys))
		     (le,
		      fn () =>
		      (print2Lts
			(s ^ ": deeply polymorphic non-functor\n** parameter types:\n",
			 "** result types:\n",
			 dom_ltys, ran_ltys);
		      LE.ltc_void))))

	(* typeInEnv : LE.tkindEnv * LE.ltyEnv * DI.depth -> lexp -> lty list *)
	fun typeInEnv (kenv: LE.tkindEnv, venv: LE.ltyEnv, d: int) =
	let fun extEnv (lv,lt,ve) = LE.ltInsert (ve,lv,lt,d)
	    fun bogusBind (lv,ve) = extEnv (lv, LE.ltc_void, ve)
	    fun typeIn venv' = typeInEnv (kenv, venv', d)
	    fun typeWith (v,t) = typeIn (LE.ltInsert (venv,v,t,d))
	    fun mismatch (le,s) (a,r,n,n') =
		errMsg
		  (le,
		   concat [s, ": binding/result list mismatch\n** expected ",
			   Int.toString n, " elements, got ", Int.toString n'],
		   foldl bogusBind a r)

	    fun typeof le =
		let fun typeofVar lv =
			(case LE.ltLookup (venv,lv,d)
			   of NONE => errMsg (le, "Unbound Lvar " ^ LV.lvarName lv, LE.ltc_void)
			    | SOME lty => lty)

		    fun typeofVal (VAR lv) = typeofVar lv
		      | typeofVal (INT{ty, ...}) = LE.ltc_num ty
		      | typeofVal (WORD{ty, ...}) = LE.ltc_num ty
		      | typeofVal (REAL _) = LE.ltc_real
		      | typeofVal (STRING _) = LE.ltc_string
		      (* REAL64: need more cases *)

		    fun typeofFn ve (_,lvar,vts,eb) =
			  let fun split ((lv,t), (ve,ts)) =
				  (defineLvar le lv;
				   (LE.ltInsert (ve,lv,t,d), t::ts))
			      val (ve',ts) = foldr split (ve,[]) vts
			   in defineLvar le lvar;
			      (ts, typeIn ve' eb)
			  end

		    (* There are lvars hidden in Access.conrep, used by dcon.
		     * These functions just make sure that they are defined in the
		     * current environemnent; we don't bother to typecheck them properly
		     * because supposedly conrep will go away...  [DBM: When? Replaced by what?]
		     *)
		    fun checkAccess (DA.LVAR v) = ignore (typeofVar v)
		      | checkAccess (DA.PATH (a,_)) = checkAccess a
		      | checkAccess _ = ()

		    fun checkConrep (DA.EXN a) = checkAccess a
		      | checkConrep (DA.SUSP (SOME (a1,a2))) = (checkAccess a1; checkAccess a2)
		      | checkConrep _ = ()

		    fun chkSnglInst (fp as (le,s)) (lt,ts) =
		      if null ts then lt
		      else case ltTyApp fp (lt,ts,kenv)
			 of [] => LE.ltc_unit
			  | [lt] => lt
			  | lts => errMsg
			      (le,
			       concat [s, ": inst yields ", Int.toString (length lts),
				       " results instead of 1"],
			       LE.ltc_void)

		    fun typeWithBindingToSingleRsltOfInstAndApp (s,lt,ts,vs,lv) e =
			  let val fp = (le,s)
			      val lt =
				  case ltFnApp fp (chkSnglInst fp (lt,ts), map typeofVal vs)
				    of [lt] => lt
				     | _ => errMsg
					      (le, concat [s, ": primop/dcon must return single result type "],
					       LE.ltc_void)
		(*
			                  | [] => LE.ltc_unit
			                  | lts => LE.ltc_tuple lts
				            (*** until PRIMOPs start returning multiple results... ***)
		*)
			   in typeWith (lv,lt) e
			  end

		    fun matchAndTypeWith (s,v,lt,lt',lv,e) =
			(ltMatch (le,s) (typeofVal v, lt); typeWith (lv, lt') e)

	         in case le
	              of RET values => map typeofVal values
		       | LET (lvs,e,e') =>
			   (app (defineLvar le) lvs;
			    typeIn
			      (foldl2 extEnv venv (lvs, typeof e)
			       handle UnequalLengths => mismatch (le,"LET"))
			      e')
		       | FIX ([],e) =>
			   (say "\n**** Warning: empty declaration list in FIX\n"; typeof e)
		       | FIX ((fd as (fk as {isrec=NONE,cconv,...},
				      lv, _, _)) :: fds', e) =>
			   let val (alts,rlts) = typeofFn venv fd
			       val lt = ltArrow (le,"non-rec FIX") (cconv,alts,rlts)
			       val ve = extEnv (lv,lt,venv)
			       val venv' =
				   if null fds' then ve
				   else errMsg
					  (le, "multiple bindings in FIX, not all recursive",
					   foldl (fn ((_,lv,_,_), ve) => bogusBind (lv,ve)) ve fds')
			    in typeIn venv' e
			   end
		       | FIX (fds,e) =>
			   let val isfct = false
			       fun extEnv (({cconv=CC_FCT, ...}, _, _, _), _) =
				     bug "unexpected case in extEnv"
				 | extEnv (({isrec,cconv,...}, lv, vts, _) : fundec, ve) =
				     case (isrec, isfct)
				       of (SOME (lts,_), false) =>
					  let val lt = ltArrow (le,"FIX") (cconv, map #2 vts, lts)
					   in LE.ltInsert (ve,lv,lt,d)
					  end
					| _ => let val msg =
						       if isfct then "recursive functor "
						       else "a non-recursive function "
					       in errMsg (le, "in FIX: " ^ msg ^ LV.lvarName lv, ve)
					       end
			       val venv' = foldl extEnv venv fds
			       fun chkDcl (({isrec = NONE, ...}, _, _, _) : fundec) = ()
				 | chkDcl (fd as ({isrec = SOME (lts,_), ...}, _, _, _)) =
				   ltsMatch (le,"FIX") (lts, #2 (typeofFn venv' fd))
			    in app chkDcl fds;
			       typeIn venv' e
			   end
		       | APP (v,vs) => ltFnApp (le,"APP") (typeofVal v, map typeofVal vs)
		       | TFN ((tfk,lv,tks,e), e') =>
			   let fun getkind (tv,tk) = (defineLvar le tv; tk)
			       val ks = map getkind tks
			       val lts = typeInEnv (LE.tkInsert (kenv,ks), venv, DI.next d) e
			    in defineLvar le lv;
			       typeWith (lv, LE.ltc_poly (ks,lts)) e'
			   end
		       | TAPP (v,ts) => ltTyApp (le,"TAPP") (typeofVal v, ts, kenv)
		       | SWITCH (_,_,[],_) => errMsg (le,"empty SWITCH",[])
		       | SWITCH (v, _, ce::ces, lo) =>
			   let val selLty = typeofVal v
			       fun g lt = (ltMatch (le,"SWITCH branch 1") (lt,selLty); venv)
			       fun brLts (c,e) =
				   let val venv' =
					   case c
					    of DATAcon ((_,conrep,lt), ts, v) =>
						let val _ = checkConrep conrep
						    val fp = (le,"SWITCH DECON")
						    val ct = chkSnglInst fp (lt,ts)
						    val nts = ltFnAppR fp (ct, [selLty])
						 in defineLvar le v;
						    foldl2 extEnv venv ([v], nts)
						      handle UnequalLengths => mismatch fp
						end
					     | INTcon{ty, ...} => g (LE.ltc_num ty)
					     | WORDcon{ty, ...} => g (LE.ltc_num ty)
					     | STRINGcon _ => g ltString
					     | VLENcon _ => g LE.ltc_int (* ? *)
				    in typeIn venv' e
				   end
			       val ts = brLts ce
			       fun chkBranch (ce,n) =
				 (ltsMatch (le, "SWITCH branch " ^ Int.toString n) (ts, brLts ce);
				  n+1)
			    in foldl chkBranch 2 ces;
			       case lo
				 of SOME e => ltsMatch (le,"SWITCH else") (ts, typeof e)
				  | NONE => ();
			       ts
			   end
		       | CON ((_,conrep,lt), ts, u, lv, e) =>
			   (checkConrep conrep;
			    defineLvar le lv;
			    typeWithBindingToSingleRsltOfInstAndApp ("CON",lt,ts,[u],lv) e)
		       | RECORD (rk,vs,lv,e) =>
			   let val lt =
			       case rk
				 of RK_VECTOR t =>
				      let val lt = LE.ltc_tyc t
					  val match = ltMatch (le,"VECTOR")
				       in app (fn v => match (lt, typeofVal v)) vs;
					  ltVector t
				      end
				  | RK_TUPLE =>
				      if null vs
				      then LE.ltc_unit
				      else let fun chkMono v =
						   let val t = typeofVal v
						       fun ef () =
							   prMsgLty ("RECORD: poly type in mono record:\n",t))
						    in if LE.ltp_fct t orelse LE.ltp_poly t
						       then error (le, ef)
						       else ();
						       t
						   end
					   in LE.ltc_tuple (map chkMono vs)
					  end
				  | RK_STRUCT => LE.ltc_str (map typeofVal vs)
			    in defineLvar le lv;
			       typeWith (lv,lt) e
			   end
		       | SELECT (v,n,lv,e) =>
			   let val lt =
				   catchExn
			             (fn () => LE.lt_select (typeofVal v, n))
			             (le, fn () => (say "SELECT from wrong type or out of range"; LE.ltc_void))
			    in defineLvar le lv;
			       typeWith (lv,lt) e
			   end
		       | RAISE (v,lts) =>
			   (ltMatch (le,"RAISE") (typeofVal v, ltExn); lts)
		       | HANDLE (e,v) =>
			   let val lts = typeof e
			    in ltFnAppR (le,"HANDLE") (typeofVal v, lts);
			       lts
			   end
		       | BRANCH ((_,_,lt,ts), vs, e1, e2) =>
			   let val fp = (le, "BRANCH")
			       val lt =
				 case ltFnApp fp (chkSnglInst fp (lt,ts), map typeofVal vs)
				  of [lt] => lt
				   | _ => errMsg
					   (le,
					    "BRANCK : primop must return single result ",
					    LE.ltc_void)
			       val _ = ltMatch fp (lt, ltBool)
			       val lts1 = typeof e1
			       val lts2 = typeof e2
			    in ltsMatch fp (lts1, lts2);
			       lts1
			   end
		       | PRIMOP ((_,PO.WCAST,lt,[]), [u], lv, e) =>
			   (*** a hack: checked only after reifY is done ***)
			   if postReify
			   then (defineLvar le lv;
				 case LE.ltd_fct lt
				  of ([argt], [rt]) =>
				       (ltMatch (le, "WCAST") (typeofVal u, argt);
					typeWith (lv, rt) e)
				   | _ => bug "unexpected WCAST in typecheck")
			   else bug "unexpected WCAST in typecheck"
		       | PRIMOP ((dc,_,lt,ts), vs, lv, e) =>
			     (* There are lvars hidden inside dicts, which we didn't check
			      * before.  This is a first-order check that they at least
			      * are bound to something; for now we don't care about their
			      * types.  (I'm not sure what the rules should look like)
			      *   --league, 10 april 1998.
			      *)
			   let fun checkDict (SOME {default, table}) =
				     (typeofVar default;
				      app (ignore o typeofVar o #2) table)
				 | checkDict (NONE : dict option) = ()
			    in checkDict dc;
			       defineLvar le lv;
			       typeWithBindingToSingleRsltOfInstAndApp ("PRIMOP",lt,ts,vs,lv) e
			   end
(*
	               | GENOP (dict, (_,lt,ts), vs, lv, e) =>
		           (* verify dict ? *)
		           typeWithBindingToSingleRsltOfInstAndApp ("GENOP",lt,ts,vs,lv) e
	               | ETAG (t,v,lv,e) =>
	                   matchAndTypeWith ("ETAG", v, ltString, ltEtag (LE.ltc_tyc t), lv, e)
	               | WRAP (t,v,lv,e) =>
	                   matchAndTypeWith ("WRAP", v, LE.ltc_tyc t, ltWrap t, lv, e)
	               | UNWRAP (t,v,lv,e) =>
	                   matchAndTypeWith ("UNWRAP", v, ltWrap t, LE.ltc_tyc t, lv, e)
*)
	        end (* fun typeof *)

         in typeof
	end (* fun typeInEvn *)

 in anyerror := false;
    ignore (typeInEnv envs lexp);
    !anyerror
end (* fun check *)

in (* toplevel local *)

(****************************************************************************
 *  MAIN FUNCTION --- val checkTop : FLINT.fundec * typsys -> bool          *
 ****************************************************************************)
fun checkTop ((fkind, lvar, args, lexp) : fundec, postReify: bool) =
    let val ve = foldl (fn ((v,lty), ve) => LE.ltInsert (ve, v, lty, DI.top)) LE.initLtyEnv args
	val err = check postReify (LE.initTkEnv, ve, DI.top) lexp
	val err = case fkind
		    of {cconv=CC_FCT,...} => err
		     | _ => (say "**** Not a functor at top level\n"; true)
     in err
    end

val checkTop =
  Stats.doPhase (Stats.makePhase "Compiler 051 FLINTCheck") checkTop

(****************************************************************************
 *  MAIN FUNCTION --- val checkExp : FLINT.lexp * typsys -> bool            *
 *  (currently unused?)                                                     *
 ****************************************************************************)
fun checkExp (le,phase) = check phase (LE.initTkEnv, LE.initLtyEnv, DI.top) le

end (* toplevel local *)
end (* structure ChkFlint *)
