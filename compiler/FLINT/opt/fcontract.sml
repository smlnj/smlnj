(* fcontract.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Author: monnier@cs.yale.edu
 * revised by DBM, 10/2021
 *)

(* [DBM, 2021.10.7] 2021 bug fix and revisions
 * Bug 294 was the first true FLINT bug reported in many years, requiring a new
 * examination of the fcontract phase. The bug was caused by code in
 * contract/fcexp/fcFix/fcFun that eliminated fundecs in a FIX if the function
 * variable's uses count was equal to its internal uses (iusenb) count, which
 * was assumed to indicate that it had no uses outside its own definition. The
 * chosen fix was to eliminate this test and the associated discarding of the fundec.
 * In the course of tracking down the sourse of the bug, DBM "clarified" and streamlined
 * the fcontract.sml code. The streamlining included:
 *   - adding typing information (in comments and definitions) and changing function
 *     and variable names to make them more useful for understanding the code;
 *   - eliminating the "int" field (internal use counts) from the Collect.info record
 *     (also making info a simple type rather than a datatype);
 *   - eliminating the "actuals" element from the arguments of the Fun constructor
 *     of the sval datatype, since it was not used;
 *   - using FLINT.fundec and FLINT.tfundec as the argument types of the Fun and TFun
 *     constructors of sval.
 * Similar clarification and streamline was applied to the Collect module.
 *
 * The bug fix is somewhat crude, but it works and the slightly weakened optimization seems
 * to have no significant effect, but reimplimenting such an optimization (removing functions
 * that are only used by themselves internally) could be considered for some optimization
 * phase in the future.
 *
 * Collect.collect is now called at the beginning of the contract function. It is no longer
 * treated as a separate "optimization" phase, since it is only used to set up the uses/calls
 * table for FCcontract. So its function is now absorbed into the fcontract phase.
 *
 * Stefan's original comments have been preserved, though the revisions may have introduced
 * some inaccuracies.
 *)

(* [SM] All kinds of beta-reductions.  In order to do as much work per pass as
 * possible, the usage counts of each variable (maintained by the Collect
 * module) is kept as much up-to-date as possible.  For instance as soon as a
 * variable becomes dead, all the variables that were referenced have their
 * usage counts decremented correspondingly.  This means that we have to
 * be careful to make sure that a dead variable will indeed not appear
 * in the output lexp since it might otherwise reference other dead variables *)

(* things that fcontract does:
 * - several things not mentioned
 * - elimination of Con(Decon x)
 * - update counts when selecting a SWITCH alternative
 * - contracting RECORD(R.1,R.2) => R  (only if the type is easily available)
 * - dropping of dead arguments
 *)

(* things that lcontract.sml does that fcontract doesn't do (yet):
 * - inline across DeBruijn depths (will be solved by named-tvar)
 * - elimination of let [dead-vs] = pure in body
 *)

(* things that cpsopt/eta.sml did that fcontract doesn't do:
 * - let f vs = select(v,i,g,g vs)
 *)

(* things that cpsopt/contract.sml did that fcontract doesn't do:
 * - IF-idiom (I still don't know what it is)
 * - unifying branches
 * - Handler operations
 * - primops expressions
 * - branch expressions
 *)

(* things that could also be added:
 * - elimination of dead vars in let
 * - elimination of constant arguments
 *)

(* things that would require some type info:
 * - dropping foo in LET vs = RAISE v IN foo
 *)

(* eta-reduction is tricky:
 * - recognition of eta-redexes and introduction of the corresponding
 *   substitution in the table has to be done at the very beginning of
 *   the processing of the FIX
 * - eta-reduction can turn a known function into an escaping function
 * - fun f (g,v2,v3) = g(g,v2,v3) looks tremendously like an eta-redex
 *)

(* order of contraction is important:
 * - the body of a FIX is contracted before the functions because the
 *   functions might end up being inlined in the body in which case they
 *   could be contracted twice.
 *)

(* When creating substitution f->g (as happens with eta redexes or with
 * code like `LET [f] = RET[g]'), we need to make sure that the usage cout
 * of f gets properly transfered to g.  One way to do that is to make the
 * transfer incremental:  each time we apply the substitution, we decrement
 * f's count and increment g's count.  But this can be tricky since the
 * elimination of the eta-redex (or the trivial binding) eliminates one of the
 * references to g and if this is the only one, we might trigger the killing
 * of g even though its count would be later incremented.  Similarly, inlining
 * of g would be dangerous as long as some references to f exist.
 * So instead we do the transfer once and for all when we see the eta-redex,
 * which frees us from those two problems but forces us to make sure that
 * every existing reference to f will be substituted with g.
 * Also, the transfer of counts from f to g is not quite straightforward
 * since some of the references to f might be from inside g and without doing
 * the transfer incrementally, we can't easily know which of the usage counts
 * of f should be transfered to the internal counts of g and which to the
 * external counts.
 *)

(* Preventing infinite inlining:
 * - inlining a function in its own body amounts to unrolling which has
 *   to be controlled (you only want to unroll some number of times).
 *   It's currently simply not allowed.
 * - inlining a recursive function outside of tis body amounts to `peeling'
 *   one iteration. Here also, since the inlined body will have yet another
 *   call, the inlining risks non-termination.  It's hence also
 *   not allowed.
 * - inlining a mutually recursive function is just a more general form
 *   of the problem above although it can be safe and desirable in some cases.
 *   To be safe, you simply need that one of the functions forming the
 *   mutual-recursion loop cannot be inlined (to break the loop).  This cannot
 *   be trivially checked.  So we (foolishly?) trust the `inline' bit in
 *   those cases.  This is mostly used to inline wrappers inside the
 *   function they wrap.
 * - even if one only allows inlining of funtions showing no sign of
 *   recursion, we can be bitten by a program creating its own Y combinator:
 *       datatype dt = F of dt -> int -> int
 *       let fun f (F g) x = g (F g) x in f (F f) end
 *   To solve this problem, `cexp' has an `ifs' parameter containing the set
 *   of funtions that we are inlining in order to detect (and break) cycles.
 * - funnily enough, if we allow inlining recursive functions the cycle
 *   detection will ensure that the unrolling (or peeling) will only be done
 *   once.  In the future, maybe.
 *)

(* Dropping useless arguments.
 * Arguments whose value is constant (i.e. the function is known and each
 * call site provides the same value for that argument (or the argument
 * itself in the case of recursive calls) can be safely removed and replaced
 * inside the body by a simple let binding.  The only problem is that the
 * constant argument might be out of scope at the function definition site.
 * It is obviously always possible to move the function to bring the argument
 * in scope, but since we don't do any code motion here, we're stuck.
 * If it wasn't for this little problem, we could do the cst-arg removal in
 * collect (we don't gain anything from doing it here).
 * The removal of dead arguments (args not used in the body) on the other
 * hand can quite well be done in collect, the only problem being that it
 * is convenient to do it after the cst-arg removal so that we can rely
 * on deadarg to do the actual removal of the cst-arg.
 *)

(* Simple inlining (inlining called-once functions, which doesn't require
 * alpha-renaming) seems inoffensive enough but is not always desirable.
 * The typical example is wrapper functions introduced by eta-expand: they
 * usually (until inlined) contain the only call to the main function,
 * but inlining the main function in the wrapper defeats the purpose of the
 * wrapper.
 * cpsopt dealt with this problem by adding a `NO_INLINE_INTO' hint to the
 * wrapper function.  In this file, the idea is the following:
 * If you have a function declaration like `let f x = body in exp', first
 * contract `exp' and only contract `body' afterwards.  This ensures that
 * the eta-wrapper gets a chance to be inlined before it is (potentially)
 * eta-reduced away.  Interesting details:
 * - all functions (even the ones that would have a `NO_INLINE_INTO') are
 *   contracted, because the "aggressive usage count maintenance" makes any
 *   alternative painful (the collect phase has already assumed that dead code
 *   will be eliminated, which means that fcontract should at the very least
 *   do the dead-code elimination, so you can only avoid fcontracting a
 *   a function if you can be sure that the body doesn't contain any dead-code,
 *   which is generally  not known).
 * - once a function is fcontracted, its inlinable status is re-examined.
 *   More specifically, if no inlining occured during its fcontraction, then
 *   we assume that the code has just become smaller and should hence
 *   still be considered inlinable.  On another hand, if inlining took place,
 *   then we have to reset the inline-bit because the new body might
 *   be completely different (i.e. much bigger) and inlining it might be
 *   undesirable.
 *   This means that in the case of
 *       let fwrap x = body1 and f y = body2 in exp
 *   if fwrap is fcontracted before f and something gets inlined into it,
 *   then fwrap cannot be inlined in f.
 *   To minimize the impact of this problem, we make sure that we fcontract
 *   inlinable functions only after fcontracting other mutually recursive
 *   functions.  One way to solve the problem more thoroughly would be
 *   to keep the uncontracted fwrap around until f has been contracted.
 *   Such a trick hasn't seemed necessary yet.
 * - at the very end of the optimization phase, cpsopt had a special pass
 *   that ignored the `NO_INLINE_INTO' hint (since at this stage, inlining
 *   into it doesn't have any undesirable side effects any more).  The present
 *   code doesn't need such a thing.  On another hand, the cpsopt approach
 *   had the advantage of keeping the `inline' bit from one contract phase to
 *   the next.  If this ends up being important, one could add a global
 *   "noinline" flag that could be set to true whenever fcontracting an
 *   inlinable function (this would ensure that fcontracting such an inlinable
 *   function can only reduce its size, which would allow keeping the `inline'
 *   bit set after fcontracting).
 *)

(* -------------------------------------------------------------------------------- *)

signature FCONTRACT =
sig
  (* Collect.collect must be called on prog before contract *)
  val contract : bool * FLINT.prog -> FLINT.prog  (* bool is etasplit option *)
  val contractCount : int ref  (* counts calls of contract *)
end

structure FContract :> FCONTRACT =
struct

    structure O  = Option
    structure DI = DebIndex
    structure A =  Access
    structure LV = LambdaVar
    structure M  = LV.Map
    structure S  = LV.Set
    structure LT = Lty
    structure FR = FunRecMeta
    structure LD = LtyDef
    structure LK = LtyKernel
    structure LE = LtyExtern
    structure PL = PLambda
    structure F  = FLINT
    structure FU = FlintUtil
    structure PU = PrintUtil
    structure PF = PrintFlint
    structure OU = OptUtils
    structure PO = Primop
    structure C  = Collect

    val debugging = FLINT_Control.fcdebugging
    val counters = FLINT_Control.fccounters

    fun say s = (Control_Print.say s; Control_Print.flush())
    fun newline () = say "\n"
    fun saynl s = (Control_Print.say s; say "\n"; Control_Print.flush())
    fun says (msgs: string list) = say (PU.interpws msgs)
    fun saysnl (msgs: string list) = saynl (PU.interpws msgs)
    fun dbsay msg = if !debugging then saynl msg else ()
    fun dbsays msgs = if !debugging then saysnl msgs else ()
    fun bug (strings: string list) = ErrorMsg.impossible (PU.interpws ("FContract:"::strings))
    fun buglexp (msg,lexp) = (say "\n"; PF.printLexp lexp; bug [msg])
    fun bugval (msg,value) = (say "\n"; PF.printValue value; bug [msg])

    val cplv = LV.dupLvar
    val mklv = LV.mkLvar

    fun tagInt n = F.INT{ival = IntInf.fromInt n, ty = Target.defaultIntSz}

    datatype sval
      = Val    of F.value  (* INVARIANT: the F.value arg should be a constant, i.e. never F.VAR *)
      | Fun    of F.fundec
      | TFun   of F.tfundec
      | Record of LV.lvar * sval list
      | Con    of LV.lvar * sval * PL.dataconstr * LT.tyc list  (* sval is Var? *)
      | Decon  of LV.lvar * sval * PL.dataconstr * LT.tyc list  (* sval is Var? *)
      | Select of LV.lvar * sval * int                          (* sval is Var? *)
      | Var    of LV.lvar * LT.lty option		        (* cop out case *)

    (* bindings: a finite map from lvars to svals,
     *  tracking the "bindings" of lvars to svals *)
    type bindings = sval M.map

    (* sval2lvarOp : sval -> LV.lvar option *)
    fun sval2lvarOp (Fun (_,lvar,_,_)) = SOME lvar
      | sval2lvarOp (TFun (_,lvar,_,_)) = SOME lvar
      | sval2lvarOp (Record(lvar,_)) = SOME lvar
      | sval2lvarOp (Con(lvar,_,_,_)) = SOME lvar
      | sval2lvarOp (Decon(lvar,_,_,_)) = SOME lvar
      | sval2lvarOp (Select(lvar,_,_)) = SOME lvar
      | sval2lvarOp (Var(lvar,_)) = SOME lvar
      | sval2lvarOp (Val _) = NONE

    (* svar2ltyOp : sval -> lty option
     *  we only try to recover the lty of an sval for Var, Decon, and Select svals *)
    fun sval2ltyOp (Var(_,ltyOp)) = ltyOp
      | sval2ltyOp (Decon(_,_,(_,_,lty),tycs)) =
	  SOME(hd(#2 (LD.ltd_arrow (hd(LE.lt_inst(lty, tycs))))))
      | sval2ltyOp (Select(_,sv,i)) =
	  (case sval2ltyOp sv
	     of SOME lty => SOME(LE.lt_select(lty, i, "fcontract#238"))
	      | _ => NONE)
      | sval2ltyOp _ = NONE

    (* tycs_eq : tyc list * tyc list -> bool
     *  equivalence of lists of tycs, based on tyc equivalence (LK.tc_eqv) *)
    fun tycs_eq ([],[]) = true
      | tycs_eq (tyc1::tycs1,tyc2::tycs2) =
	  LK.tc_eqv(tyc1,tyc2) andalso tycs_eq(tycs1,tycs2)
      | tycs_eq _ = false

    (* append: lvars:lvar list -> code:(F.value list -> F.lexp) -> lexp:F.lexp -> F.lexp
     *  calls `code' to append an lexp to each leaf of `lexp'.
     *  Typically used to transform `let lvs = lexp in code' so that
     *  `code' is now copied at the end of each branch of `lexp'.
     *  `lvars' is a list of lvars that should be used if the result of `lexp'
     *  needs to be bound before calling `code'. *)
    fun append lvars code lexp =
	let fun apd (F.RET values) = code values
	      | apd (le as (F.APP _ | F.TAPP _ | F.RAISE _ | F.HANDLE _)) =
		  let fun alpha_cvt lvar =
			  let val newlvar = cplv lvar
			   in C.new newlvar; newlvar
			  end
		      val newlvars = map alpha_cvt lvars
		  in F.LET(newlvars, le, code(map F.VAR newlvars))
		  end
	      | apd (F.LET (lvs, body, le)) = F.LET (lvs, body, apd le)
	      | apd (F.FIX (fdecs, body)) = F.FIX(fdecs, apd body)
	      | apd (F.TFN (tfdec, body)) = F.TFN(tfdec, apd body)
	      | apd (F.SWITCH (v,ac,arms,def)) =
		  let fun larm (con,arm_body) = (con, apd arm_body)
		   in F.SWITCH(v, ac, map larm arms, O.map apd def)
		  end
	      | apd (F.CON (dc,tycs,v,lv,le)) = F.CON(dc, tycs, v, lv, apd le)
	      | apd (F.RECORD (rk,vs,lv,le)) = F.RECORD(rk, vs, lv, apd le)
	      | apd (F.SELECT (v,i,lv,le)) = F.SELECT(v, i, lv, apd le)
	      | apd (F.BRANCH (po,vs,le1,le2)) = F.BRANCH(po, vs, apd le1, apd le2)
	      | apd (F.PRIMOP (po,vs,lv,le)) = F.PRIMOP(po, vs, lv, apd le)
	in apd lexp
	end

    (* extract: PL.con * F.lexp -> (PL.con * F.lexp) * F.fundec
     *  abstracts the rhs lexp of a switch arm/case into a function
     *  and replaces the rhs with a call to that function *)
    fun extract (con: PL.con, arm_body: F.lexp) =
	let val f = mklv()  (* new "name" for new arm body function *)
	    val _ = C.new f (* register it *)
	    val fk = {isrec=NONE,known=true,inline=FR.IH_SAFE,
		      cconv=FR.CC_FUN(LT.FF_FIXED)}
	in case con
	    of PL.DATAcon(dc as (_,_,lty),tycs,lvar) => (* lvar represents decon value *)
		let val newlvar = cplv lvar
		    val _ = C.usesInc (C.new newlvar)
		    val (lty,_) = LD.ltd_parrow(hd(LE.lt_inst(lty, tycs)))
		in ((PL.DATAcon(dc, tycs, newlvar),
		     F.APP(F.VAR f, [F.VAR newlvar])),
		    (fk, f, [(lvar, lty)], arm_body))
		end
	     | con =>
		 ((con, F.APP(F.VAR f, [])),  (* new arm body: "f()", unique use/call of f *)
		  (fk, f, [], arm_body))  (* fundec for abstracted arm_body *)
	end

    (* inScope : bindings -> LV.lvar -> bool
     *  lvar is in the domain of bindings m *)
    fun inScope m lvar = Option.isSome(M.find(m,lvar))

    (* click : string * int ref -> unit *)
    fun click (msg: string, counter: int ref) =
	let val n = !counter + 1
	 in if !counters then saysnl ["%%%", msg, Int.toString n] else ();
	    counter := n
	end

    (* contractCount : int ref -- records how many time contract is called *)
    val contractCount = ref 0

      (* startContract: unit -> unit
       * endContract: unit -> unit *)
      fun startContract () =
	  (contractCount := !contractCount + 1;
	   dbsay (">>> FContract.contract "^Int.toString (!contractCount)))

      fun endContract () =
	  (dbsay ("<<< FContract.contract "^Int.toString (!contractCount)))

    (* contract : : etaSplit:bool -> FLINT.prog -> FLINT.prog
     *  etaSplit is false for normal fcontract phase, true for fcontract+eta phase.
     *  [tfnInline was another option, but was always false, so was eliminated.]
     *  contract ends at line [1129] *)
    fun contract (etaSplit, fdec as (_,f,_,_)) =  (* f is fundec lvar, the "name" of function *)
      let val _ = startContract ()
	  val _ = C.collect fdec
	  val counter = ref 0
	  val c_miss = ref 0
	  val c_inline = ref 0  (* this counter is actually *used* by fcontract, not just for stats *)

	  fun click_deadval  () = click ("deadval", counter)
	  fun click_deadlexp () = click ("deadlexp", counter)
	  fun click_select   () = click ("select", counter)
	  fun click_record   () = click ("record", counter)
	  fun click_con      () = click ("con", counter)
	  fun click_switch   () = click ("switch", counter)
	  fun click_eta      () = click ("eta", counter)
	  fun click_etasplit () = click ("etasplit", counter)
	  fun click_branch   () = click ("branch", counter)
	  fun click_dropargs () = click ("dropargs", counter)

	  fun click_lacktype () = click ("lacktype", c_miss)

	  fun click_simpleinline () = click ("simpleinline", c_inline)
	  fun click_copyinline   () = click ("copyinline", c_inline)
	  fun click_unroll       () = click ("unroll", c_inline)

	  fun inline_count () = !c_inline

	  (* eqConValue : PL.con * F.value -> bool *)
	  fun eqConValue (PL.INTcon i1,    F.INT i2)    = (#ival i1 = #ival i2)
	    | eqConValue (PL.WORDcon i1,   F.WORD i2)   = (#ival i1 = #ival i2)
	    | eqConValue (PL.STRINGcon s1, F.STRING s2) = (s1 = s2)
	    | eqConValue (con, v) = bugval("unexpected comparison with val", v)

          (* lookup : bindings * LV.lvar -> sval
           *  calls M.find and turns sval for SOME or calls bug for NONE, so lookup
           *  is always expected to succeed (the lvar is in the domain of the bindings) *)
	  fun lookup (m: bindings, lv: LV.lvar) : sval =
	      (case M.find (m,lv)
		 of SOME x => x
		  | NONE => bug ["lookup:", C.lvarToString lv])

          (* sval2val : sval -> F.value *)
	  fun sval2val (sval: sval) : F.value =
	      (case sval2lvarOp sval
		 of NONE => (case sval  (* sval must be Val *)
			       of Val v => v
				| _ => bug ["sval2val"])
		  | SOME lvar => F.VAR lvar
		 (*esac*))

          (* val2sval : bindings -> F.value -> sval *)
	  fun val2sval (m: bindings) (F.VAR lvar: F.value) : sval =
	      lookup (m, lvar)
	    | val2sval m v = Val v  (* v a constant F.value *)

          (* bugsv : string * sval -> unit *)
	  fun bugsv (msg, sval) = bugval(msg, sval2val sval)

          (* subst : bindings -> LV.lvar -> F.value *)
	  fun subst m lvar = sval2val (lookup (m, lvar))

          (* substval : bindings -> F.value -> F.value *)
	  fun substval m = sval2val o (val2sval m)

          (* substvar : bindings -> LV.lvar -> LV.lvar *)
	  fun substvar m lv =
	      case substval m (F.VAR lv)
	        of F.VAR lv => lv
		 | v => bugval ("unexpected val", v)

          (* useLvar : LV.lvar -> unit *)
	  fun useLvar lvar = C.usesInc (C.getInfo lvar)

          (* unuseLvar : LV.lvar -> bool *)
          fun unuseLvar (lvar: LV.lvar) = C.usesDec (C.getInfo lvar)

          (* uncallLvar : LV.lvar -> bool *)
          fun uncallLvar (lvar: LV.lvar) = C.callsDec (C.getInfo lvar)

	  (* undertake : bindings -> LV.lvar -> unit
           *  called when a variable becomes dead; it simply adjusts the use-counts *)
	  fun undertake m lvar =
	      let val utm = undertake m
	       in case lookup (m, lvar)
		    of Var _ => ()
		     | Val _ => ()
		     | Fun (_,lv,args,body) =>
		       C.unuselexp utm
				   (F.LET(map #1 args,
					  F.RET (map (fn _ => tagInt 0) args),
					  body))
		     | TFun (_,_,_,body) => C.unuselexp utm body
		     | (Select (_,sval,_) | Con (_,sval,_,_)) => unusesval m sval
		     | Record (_,svals) => app (unusesval m) svals
		     (* decon's are implicit so we can't get rid of them *)
		     | Decon _ => ()
	      end

          (* unuseValue : bindings -> F.value -> unit *)
	  and unuseValue m (F.VAR lvar) =
	        if unuseLvar lvar
		then undertake m lvar
		else ()
	    | unuseValue _ _ = ()  (* does nothing for constants *)

          (* unusesval : bindings -> sval -> unit *)
	  and unusesval m sval = unuseValue m (sval2val sval)

          (* unusecall : bindings -> LV.lvar -> unit *)
	  fun unusecall m lvar =
	        if uncallLvar lvar
	        then undertake m lvar
	        else ()

 	  (* addbind : bindings * LV.lvar * sval -> bindings *)
	  fun addbind (m, lvar, sval) =
	      (dbsay ("### BIND " ^ C.lvarToString lvar);
	       M.insert(m, lvar, sval))

          (* substitute : bindings * LV.lvar * sval * F.value -> bindings
	   * substitute an sval sv for a variable lv and unuse value v. *)
	  fun substitute (m, lv1, sv, v) =
	      (case sval2val sv
		 of F.VAR lv2 => C.transfer(lv1,lv2)
		  | v2 => ();
	       unuseValue m v;
	       addbind(m, lv1, sv)) (* handle x =>
		   (say ("while substituting " ^ (C.lvarToString lv1) ^ " -> ");
		    PF.printValue (sval2val sv);
		    raise x) *)

          (* cpo : bindings -> F.primop -> F.primop
	   *  sustitutes lvars in primop representation (default, table entries) *)
	  fun cpo m (SOME{default,table},po,lty,tycs) =
	      (SOME{default=substvar m default,
		    table=map (fn (tycs,lv) => (tycs, substvar m lv)) table},
	       po,lty,tycs)
	    | cpo _ po = po

          (* cdcon : bindings -> PL.dataconstr -> PL.dataConstr
           *  substitutes access lvar for exn constructors, ow does nothing *)
	  fun cdcon m (s, A.EXN(A.LVAR lvar), lty) =
	        (s, A.EXN(A.LVAR(substvar m lvar)), lty)
	    | cdcon _ dc = dc

	  (* The set of functions which are apparently non-recursive but which seem
	   * to recurse nevertheless, typically with the help of a type-level
	   * recursion. *)
	  val recursive_funs = ref S.empty

          type cont = bindings * F.lexp -> F.lexp
            (* a kind of "continuation" function *)

          (* fcexp : ifs:S.set -> m:bindings * le:F.lexp * cont:cont -> F.lexp
	   *  ifs (inlined functions): set of function lvars for functions that we're currently
	   *      inlining, in order to detect loops
	   *  m: bindings -- a map from lvars to their "defining expressions" (svals)
	   *  le : expression to contract
	   *  cont: context continuation *)
	  fun fcexp (ifs : S.set) (m : bindings, le : F.lexp, cont : cont) : F.lexp =
	      let val loop = fcexp ifs (* : (bindings * F.lexp * cont) -> F.lexp  -- keeping ifs fixed *)
		  val substval = substval m
		  val cdcon = cdcon m
		  val cpo = cpo m

		  (* fcexp/fcLet : LV.lvar list * F.lexp * F.lexp -> F.lexp *)
		  fun fcLet (lvs, le, body) =
		      let (* fcexp/fcexp/fcbody : cont *)
			  fun fcbody (nm: bindings, nle: F.lexp): F.lexp =
			      let fun cbody (():unit): F.lexp =
				      let val nm = (foldl (fn (lv,m) => addbind(m, lv, Var(lv, NONE))) nm lvs)
				       in case loop (nm, body, cont)
					    of F.RET vs =>
						 if ListPair.allEq
						      (fn (v, lv) => FU.sameValue(v, F.VAR lv))
						      (vs, lvs)
						  then nle
						  else F.LET(lvs, nle, F.RET vs)
					     | nbody => F.LET(lvs, nle, nbody)
				      end
			       in case nle
				    of F.RET vs =>
					 let fun simplesubst (lv,v,m) =
						 let val sv = val2sval m v
						  in substitute(m, lv, sv, sval2val sv)
						 end
					      val nm = (ListPair.foldl simplesubst nm (lvs, vs))
					  in loop (nm, body, cont)
					 end
				     | F.TAPP _ =>
				         if List.all (C.dead o C.getInfo) lvs
					 then loop (nm, body, cont)
					 else cbody()
				     | _ => cbody()
			      end (* fcbody *)

			  (* fcexp/fcLet/cassoc: LV.lvar * F.lexp * (F.lexp -> F.lexp) -> F.lexp
			   * this is a hack originally meant to cleanup the BRANCH
			   * mess introduced in flintnm (where each branch returns
			   * just true or false which is generally only used as
			   * input to a SWITCH).
			   * The present code does more than clean up this case. *)
			  fun cassoc (lv, F.SWITCH(F.VAR v,ac,arms,NONE), wrap) =
			      if lv <> v orelse C.uses(C.getInfo lv) > 1
			      then loop (m, le, fcbody)
			      else let val (narms,fdecs) = ListPair.unzip (map extract arms)
				       fun addswitch [v] =
					   C.copylexp M.empty (F.SWITCH(v,ac,narms,NONE))
					 | addswitch _ = bug ["fcexp/fcLet/cassoc/addswitch"]
				       (* replace each leaf `ret' with a copy
					* of the switch *)
				       val nle = append [lv] addswitch le
				       (* decorate with the functions extracted
					* from the switch arms *)
				       val nle =
					   foldl (fn (f,le) => F.FIX([f],le))
						 (wrap nle) fdecs
				    in click_branch();
				       loop (m, nle, cont)
				   end
			    | cassoc _ = loop (m, le, fcbody)  (* end cassoc *)

		       in case (lvs, le, body)
			   of ([lv],(F.BRANCH _ | F.SWITCH _),F.SWITCH _) =>
			        cassoc(lv, body, fn x => x)
			    | ([lv],(F.BRANCH _ | F.SWITCH _),F.LET(lvs,body as F.SWITCH _,rest)) =>
			        cassoc(lv, body, fn le => F.LET(lvs,le,rest))
			    | _ => loop (m, le, fcbody)
		      end (* fcLet *)

		  (* fcexp/fcFix : F.fundec list * F.lexp -> F.lexp *)  (* args of F.FIX *)
		  fun fcFix (fundecs, body) =
		      let
			  (* bind_arg (LV.lvar * LT.lty) * bindings -> bindings
                           *  replaces "merge_actuals", which ingnored its sval list argument ("actuals") *)
 			  fun bind_arg ((lvar,lty), m) = addbind (m, lvar, Var(lvar, SOME lty))

			  (* fcexp/fcFix/fcFun : F.fundec * (bindings * F.fundec list) -> bindings * F.fundec list
			   *  The actual function contraction *)
			  fun fcFun ((fk as {inline,cconv,known,isrec}, fvar, args, body): F.fundec,
				     (m: bindings, fds: F.fundec list)) =
			      let val finfo = C.getInfo fvar
			      in if C.dead finfo
				 then (m, fds)
				 else
				   let val saved_ic = inline_count()
				       (* make up the bindings for args inside the body *)

				       val m1 = foldl bind_arg m args

				       (* contract the body and create the resulting fundec.
					* Temporarily remove f's definition from the
					* environment while we're rebuilding it to avoid
					* nasty problems. *)
				       val nbody = fcexp (S.add(ifs, fvar))
							 (addbind (m1, fvar, Var(fvar, NONE)), body, #2)

				       (* if inlining took place, the body might be completely
					* changed (read: bigger), so we have to reset the
					* `inline' bit *)
				       val nfk = {isrec=isrec, cconv=cconv,
						  known=known orelse not(C.escaping finfo),
						  inline=if inline_count() = saved_ic
							 then inline
							 else FR.IH_SAFE}

				       (* update the binding in the map.  This step is
					* not just a mere optimization but is necessary
					* because if we don't do it and the function
					* gets inlined afterwards, the counts will reflect the
					* new contracted code while we'll be working on the
					* the old uncontracted code *)
				       val newFundec : F.fundec = (nfk, fvar, args, nbody)
				       val newSval : sval = Fun newFundec
				       val m2 = addbind(m1, fvar, newSval)
				   in (m2, newFundec::fds)
				   end
			      end (* fcFun *)

			(* fcexp/fxFix/fcEta : F.fundec * (bindings * F.fundec list * LV.lvar list)
				   -> bindings * F.fundec list * LV.lvar list
			 *  check for eta redex *)
			  fun fcEta (fundec as (_, fvar, args, F.APP(F.VAR gvar, vals)): F.fundec,
				     (m, fundecs, fvars)) =
			      if List.length args = List.length vals andalso
				  ListPair.allEq (fn (v,(lv,_)) => FU.valueIsVar lv v) (vals, args)
			      then
				  let val g_sval = lookup (m, gvar)
				      val g_lvar = case sval2val g_sval
						     of F.VAR lvar => lvar
						      | v => bugval("not a variable", v)
				  (* NOTE: we don't want to turn a known function into an
				   * escaping one.  It's dangerous for optimisations based
				   * on known functions (elimination of dead args, f.ex)
				   * and could generate cases where call>use in collect.
				   * Of course, if g is not a locally defined function (it's
				   * bound by a LET or as an argument), then knownness is
				   * irrelevant. *)
				  in if LV.same(fvar, g_lvar) orelse
					((C.escaping(C.getInfo fvar)) andalso
					 not(C.escaping(C.getInfo g_lvar)) andalso
					 (case g_sval of Fun _ => true | _ => false))
				     (* the default case could ensure the inline *)
				     then (m, fundec::fundecs, fvars)
				     else
					 (* if an earlier function h has been eta-reduced
					  * to f, we have to be careful to update its
					  * binding to not refer to f any more since f
					  * will disappear *)
					let fun bind (lvar: LV.lvar, m: bindings): bindings =
						let val sval = lookup (m, lvar)
						 in if FU.sameValue(sval2val sval, F.VAR fvar)
						    then addbind(m, lvar, g_sval)
						    else m
						end
					    val m1 = foldl bind m fvars
					 in
					 (* I could almost reuse `substitute' but the
					  * unuse in substitute assumes the val is escaping *)
					    click_eta();
					    C.transfer(fvar, g_lvar);
					    unusecall m1 g_lvar;
					    (addbind (m1, fvar, g_sval), fundecs, fvar::fvars)
					end
				  end
			      else (m, fundec::fundecs, fvars)
			  | fcEta (fundec, (m, fundecs, fvars)) = (m, fundec::fundecs, fvars)
			  (* end fcEta *)

			(* fcexp/fcFix/wrap : F.fundec * F.fundec list -> F.fundec list
			 *  add wrapper for various purposes *)
			  fun wrap (fd as (fk as {isrec,inline,...},fd_lvar,args,body): F.fundec,
				    fds: F.fundec list) =
			      let val fd_info = C.getInfo fd_lvar
				  fun dropargs filter =
				      let val (nfk,nfk') = OU.fk_wrap(fk, O.map #1 isrec)
					  val args' = filter args
					  val new_lvar = cplv fd_lvar
					  val new_args = map (fn (lv, lty) => (cplv lv, lty)) args
					  val new_args' = map #1 (filter new_args)
					  val appargs = (map F.VAR new_args')
					  val newfd = (nfk, fd_lvar, new_args, F.APP(F.VAR new_lvar, appargs))
					  val newfd' = (nfk', new_lvar, args', body)
					  val new_info = C.new new_lvar
				       in app (ignore o C.new o #1) new_args;
					  C.callsInc new_info;
					  app useLvar new_args';
					  newfd' :: newfd :: fds
				      end
			      in
				  (* Don't introduce wrappers for escaping-only functions.
				   * This is debatable since although wrappers are useless
				   * on escaping-only functions, some of the escaping uses
				   * might turn into calls in the course of fcontract, so
				   * by not introducing wrappers here, we avoid useless work
				   * but we also postpone useful work to later invocations. *)
				  if C.dead fd_info then fds
				  else if inline=FR.IH_ALWAYS then fd::fds else
				      let val usedArgs : bool list = map (C.usedLvar o #1) args
				      in if C.called fd_info then
					   (* if some args are not used, let's drop them *)
					   if not (List.all (fn x => x) usedArgs)
					   then (click_dropargs();
						 dropargs (fn xs => OU.filter usedArgs xs))

					   (* eta-split: add a wrapper for escaping uses *)
					   else if etaSplit andalso C.escaping fd_info then
					       (* like dropargs but keeping all args *)
					       (click_etasplit();
						dropargs (fn x => x))

					   else fd::fds
					 else fd::fds
				      end
			      end (* wrap *)

			 (* add various wrappers *)
			 val wrap_fds: F.fundec list = foldl wrap [] fundecs

			 (* register the new bindings (uncontracted for now) *)
			 val (m1: bindings, fundecs0: F.fundec list) =
			     foldl (fn ((fk, lvar, args, body), (m, fundecs)) =>
					  let val f_fundec = (fk, lvar, args, body)
					   in (addbind(m, lvar, Fun f_fundec), f_fundec::fundecs)
					  end)
				   (m,[]) wrap_fds

			 (* check for eta redexes *)
			 val (m2, fundecs1: F.fundec list, _) = foldl fcEta (m1,[],[]) fundecs0

			 val (wrappers: F.fundec list, fundecs2: F.fundec list) =
			     List.partition (fn ({inline=FR.IH_ALWAYS,...},_,_,_) => true
					      | _ => false) fundecs1

			 val (maybes: F.fundec list, others: F.fundec list) =
			     List.partition (fn ({inline=FR.IH_MAYBE _,...},_,_,_) => true
					      | _ => false) fundecs2

			 (* First contract the big inlinable functions.  This might make them
			  * non-inlinable and we'd rather know that before we inline them.
			  * Then we inline the body (so that we won't go through the inline-once
			  * functions twice), then the normal functions and finally the wrappers
			  * (which need to come last to make sure that they get inlined if
			  * at all possible) *)
			 val (m3, maybes_fds) = foldl fcFun (m2, nil) maybes  (* inline = FR.IH_MAYBE *)

			 val fixbody = loop (m3, body, cont)

			 val (m4, other_fds) = foldl fcFun (m3, maybes_fds) others

			 val (m5, all_fds) = foldl fcFun (m4, other_fds) wrappers  (* inline = FR.IH_ALWAYS *)

			 (* junk newly unused funs *)
			 val used_fds = List.filter (C.usedLvar o #2) all_fds

		       in case used_fds
			   of [] => fixbody
			    | [f1 as ({isrec=NONE,...},_,_,_),f2] =>
				 (* gross hack: `wrap' might have added a second
				  * non-recursive function.  we need to split them into
				  * 2 nested FIXes.  This is _very_ ad-hoc. *)
			        F.FIX([f2], F.FIX([f1], fixbody))
			    | _ => F.FIX(used_fds, fixbody)
		      end (* fcFix *)

		(* fcexp/fcApp : F.value * F.value list -> F.lexp  -- F.APP args *)
		fun fcApp (f as F.VAR fvar, arg_vals: F.value list) =
		      let val arg_svals = map (val2sval m) arg_vals (* map arg values to their svals *)
		          val f_sval = val2sval m f  (* sval for f in m *)
		          (* F.APP inlining (if any) *)
		       in case f_sval
			    of Fun ({inline,...}, g, args, body) =>
			     (* eta contraction could cause fvar to map to a different function g,
			      * which, in turn, might have already been contracted, so we need
			      * to make sure that we get the latest version of the function.
			      *)
			       if (fvar <> g)
			       then fcApp (F.VAR g, arg_vals)  (* fvar has been "redirected" to g *)
			       else
			       let val ginfo = C.getInfo g
				 fun noinline () =
				     cont(m, F.APP(sval2val f_sval, map sval2val arg_svals))
				 fun simpleinline () =
				     (* simple inlining:  we should copy the body and then
				      * kill the function, but instead we just move the body
				      * and kill only the function name.
				      * This inlining strategy looks inoffensive enough,
				      * but still requires some care: see comments at the
				      * begining of this file and in cfun *)
				     (* FIXME: this may cause body to be optimized a second
				      * time.  Usually, this is not a big deal, but it can
				      * lead to infinite inlining in the following case:
				      * inlining a call to F generates a function G which
				      * contains a call to F (not inlined this time around
				      * thanks to ifs), later on G gets simpleinlined at which
				      * point the new call to F does get inlined, ...
				      * This particular case is handled by recursive_funs,
				      * but there might be other problematic scenarios.  *)
				     (click_simpleinline();
				      ignore(C.callsDec ginfo);
				      let val inlined = F.LET(map #1 args, F.RET arg_vals, body)
				       in loop (m, inlined, cont)
				      end)
				 fun copyinline () =
				     (* aggressive inlining.  We allow pretty much
				      * any inlinling, but we detect and reject inlining
				      * recursively which would else lead to infinite loop *)
				     if S.member(ifs, g) orelse S.member(!recursive_funs, g)
				       (* We're trying to inline a function we're already in
					* the process of inlining (or which we found earlier
					* to be recursive).  This means this function is
					* declared as non-recursive, whereas it does recurse in
					* practice.  Record it in recursive_funs to make sure
					* we won't try to inline it ever again, even after
					* we're done inlining it.  *)
				     then (recursive_funs := S.add(!recursive_funs, g);
					   noinline())
				     (* Do the actual inlining.  Random half-related note:
				      * unrolling is not as straightforward as it seems:
				      * if you inline the function you're currently
				      * fcontracting, you're asking for trouble: there is a
				      * hidden assumption in the counting that the old code
				      * will be replaced by the new code (and is hence dead).
				      * If the function to be unrolled has the only call to
				      * function f, then f might get simpleinlined before
				      * unrolling, which means that unrolling will introduce
				      * a second occurence of the `only call' but at that point
				      * f has already been killed. *)
				     else let val nle0 = (F.LET(map #1 args, F.RET arg_vals, body))
					      val nle = C.copylexp M.empty nle0
					   in click_copyinline();
					      (app (unuseValue m) arg_vals);
					      unusecall m g;
					      (* say("copyinline "^(C.lvarToString g)^"\n"); *)
					      fcexp (S.add(ifs, g)) (m, nle, cont)
					  end

			        in if C.uses ginfo = 1
				       andalso not(S.member(ifs, g)) (* Not sure why/if this is needed. *)
				    then simpleinline()
				    else case inline  (* consult inline hint for f_sval *)
					   of FR.IH_SAFE => noinline()
					    | FR.IH_UNROLL => noinline()
					    | FR.IH_ALWAYS => copyinline()
					    | FR.IH_MAYBE(min,ws) =>
						let fun value w _ (Val _ | Con _ | Record _) = w
						      | value w v (Fun _) =
							  if C.uses (C.getInfo v) = 1 then w * 2 else w
						      | value w _ _ = 0
						    val s = (OU.foldl3 (fn (sv,w,(v,t),s) => value w v sv + s)
								       0 (arg_svals,ws,args))
								handle OU.Unbalanced => 0
						in if s > min then copyinline() else noinline()
						end
			       end
			   | _ => cont (m, F.APP(sval2val f_sval, map sval2val arg_svals))

		      end
		  | fcApp (f_val, arg_vals) =  (* f not VAR, => constant => not of function type *)
		      let val arg_svals = map (val2sval m) arg_vals
			  val f_sval = val2sval m f_val
		       in cont (m, F.APP(sval2val f_sval, map sval2val arg_svals))
		      end
		  (* end fcApp *)

	        (* fcexp/fcTfn : F.tfundec * F.lexp -> F.lexp *)
		fun fcTfn ((tfk,f,args,body): F.tfundec, le) =
		      let val fi = C.getInfo f
		       in if C.dead fi
			  then (click_deadlexp(); loop (m, le, cont))
			  else let val saved_ic = inline_count()
				   val nbody = fcexp ifs (m, body, #2)
				   val ntfk =
					if inline_count() = saved_ic then tfk else {inline=FR.IH_SAFE}
				   val tfd : F.tfundec = (tfk, f, args, nbody)
				   val nm = addbind(m, f, TFun tfd)
				   val nle = loop (nm, le, cont)
			        in if C.dead fi then nle else F.TFN(tfd, nle)
			       end
		      end (* fcTfn *)

                (* fcexp/fcTapp : F.value * LT.tyc list -> F.lexp  -- takes args of F.TAPP *)
		fun fcTapp (fval,tycs) =   (* F.TAPP inlining (if any?) *)
		      cont(m, F.TAPP(sval2val(val2sval m fval), tycs))

                (* fcexp/fcSwitch : v:F.value * ac:A.consig * arms:(PL.con * F.lexp) list * def:F.lexp option
		                    -> F.lexp   -- F.SWITCH args *)
		fun fcSwitch (subject: F.value, consig, arms, defaultOp) =
		    let fun fcsCon (lvc, svc, (_, subject_rep, _): PL.dataconstr, _) =
			    (* subject (thing matched against) is a (constant) data constructor,
			       subject_rep is the conrep of that data constructor.
			       This switch contraction is performed only if subject_rep is _not_
			       an exception constructor; otherwise the equality test between datacon
			       reps is not valid because of identity declarations of exception
			       constructors. [bug 290]
			     * ASSERT: subject_rep is not EXN _ (checked before calling fcsCon) *)
			    let fun killLexp lexp = C.unuselexp (undertake m) lexp
				fun kill (lvar, lexp) =
				    C.unuselexp (undertake (addbind (m, lvar, Var(lvar,NONE)))) lexp
				fun killarm (PL.DATAcon (_, _, lvar), lexp) = kill (lvar, lexp)
				  | killarm _ = buglexp("bad arm in switch(con)", le)
				fun carm ((PL.DATAcon (dc2, _, lvar), lexp) :: rest) =
				    if subject_rep = #2 (cdcon dc2)
				    then (* subject DATAcon == arm DATAcon, so this arm is chosen *)
				      (map killarm rest; (* kill the rest of the arms *)
				       O.map killLexp defaultOp; (* and the default case *)
				       loop ((substitute(m, lvar, svc, F.VAR lvc)), lexp, cont))
				    else (* kill this arm and try the rest *)
				      (kill (lvar, lexp); carm rest)
				  | carm [] = loop (m, O.valOf defaultOp, cont)
				  | carm _ = buglexp("unexpected arm in switch(con,...)", le)
			     in click_switch(); carm arms
			    end (* fcsCon *)

		      (* fcexp/fcSwitch/fcsVal : F.value -> F.lexp *)
			fun fcsVal value =
			    let fun killLexp lexp = C.unuselexp (undertake m) lexp
				fun carm ((con,lexp) :: rest) =
				      if eqConValue (con, value)
				      then (map (killLexp o #2) rest;
					    O.map killLexp defaultOp;
					    loop (m, lexp, cont))
				      else (killLexp lexp; carm rest)
				  | carm [] = loop (m, O.valOf defaultOp, cont)
			     in click_switch(); carm arms
			    end (* fcsVal *)

		      (* fcexp/fcSwitch/fcsDefault : sval -> F.lexp
		       *  old lvar argument "lvc" has been eliminated *)
			fun fcsDefault sval =
			  (case (arms, defaultOp)
			     of ([(PL.DATAcon (dc, tycs, lvar), le)], NONE) =>
				(* single arm, no default lexp;
				 * this is a mere DECON, so we can push the let binding
				 * (hidden in cont) inside and maybe even drop the DECON *)
				let val ndc = cdcon dc
				    val slv = Decon (lvar, sval, ndc, tycs)
				    val nm = addbind (m, lvar, slv)
				    (* see below *)
				    val nle = loop (nm, le, cont)
				    val nv = sval2val sval
				in
				    if C.usedLvar lvar then
					F.SWITCH (nv, consig,[(PL.DATAcon(ndc,tycs,lvar), nle)], NONE)
				    else (unuseValue m nv; nle)
				end
			      | ([(_, lexp)], NONE) =>
				(* Single arm, no default: this should never happen,
				 * but we optimize it away anyway *)
				(unuseValue m (sval2val sval); loop (m, lexp, cont))
			      | ([], SOME defaultLexp) =>
				(* No arms, but default exists: this also should never happen,
				 * but we optimize it away anyway *)
				(unuseValue m (sval2val sval); loop (m, defaultLexp, cont))
			      | _ =>
				let fun carm (PL.DATAcon (dc, tycs, lvar), lexp) =
					  let val ndc = cdcon dc
					      val slv = Decon(lvar, sval, ndc, tycs)
					      val nm = addbind(m, lvar, slv)
					  (* deleted apparently obsolete [SM] comment *)
					  in (PL.DATAcon(ndc, tycs, lvar), loop (nm, lexp, #2))
					  end
				      | carm (con,lexp) = (con, loop (m, lexp, #2))
				    val newArms = map carm arms
				    val newDefaultOp =
					  case defaultOp
					    of NONE => NONE
					     | SOME lexp => SOME (loop (m, lexp, #2))
				in cont (m, F.SWITCH (sval2val sval, consig, newArms, newDefaultOp))
				end)

		        val subjectSval = val2sval m subject

		     in case subjectSval
			  of Con (x as (_, _, (_, conrep, _), _)) =>
			       (* check for EXN conrep, if so call fcsDefault subjectSval instead of fcsCon *)
 			       (case conrep 
				  of A.EXN _ => fcsDefault subjectSval
				   | _ => fcsCon x)
			   | Val v => fcsVal v
			   | (Var _ | Select _ | Decon _ | (* will probably never happen? *) Record _) =>
			       fcsDefault subjectSval
			   | (Fun _ | TFun _) =>
			       bugval("fcSwitch[Fun|TFun]", subject)
		    end (* fcSwitch *)

                (* fcexp/fcCon : PL.dataconstr * LT.tyc list * F.value * LV.lvar * F.lexp -> F.lexp -- F.CON args*)
		fun fcCon (dc1,tycs1,v,lv,le) =
		      let val lvinfo = C.getInfo lv
		       in if C.dead lvinfo
			  then (click_deadval(); loop (m, le, cont))
			  else let val ndc = cdcon dc1
				   fun ccon sv =
					let val nm = addbind(m, lv, Con(lv, sv, ndc, tycs1))
					    val nle = loop (nm, le, cont)
					in if C.dead lvinfo then nle
					   else F.CON(ndc, tycs1, sval2val sv, lv, nle)
					end
				 in case val2sval m v
				      of sv as (Decon (lvd,sv',dc2,tycs2)) =>
				           if FU.dcon_eq(dc1, dc2) andalso tycs_eq(tycs1,tycs2)
					   then (click_con();
						 loop (substitute (m, lv, sv', F.VAR lvd), le, cont))
					   else ccon sv
				     | sv => ccon sv
				end
		      end (* fcCon *)

                (* fcexp/fcRecord : RF.rkind * F.value list * LV.lvar * F.lexp -> F.lexp -- F.RECORD args *)
		fun fcRecord (rk,vs,lv,le) =
		    (* g: check whether the record already exists *)
		      let val lvinfo = C.getInfo lv
		       in if C.dead lvinfo then (click_deadval(); loop (m, le, cont)) else
				let fun g (Select(_,sv,0)::ss) =
					let fun g' (n,Select(_,sv',i)::ss) =
						if n = i
						andalso FU.sameValue(sval2val sv, sval2val sv')
						  then g'(n+1,ss)
						  else NONE
					      | g' (n,[]) =
						(case sval2ltyOp sv
						  of SOME lty =>
						     let val ltd =
							     case (rk, LD.ltp_tyc lty)
							      of (FR.RK_STRUCT, false) => LD.ltd_str
							       | (FR.RK_TUPLE, true) => LD.ltd_tuple
							       (* we might select out of a struct
								* into a tuple or vice-versa *)
							       | _ => (fn _ => [])
						     in if length(ltd lty) = n
							then SOME sv else NONE
						     end
						   | _ => (click_lacktype(); NONE)) (* sad *)
					      | g' _ = NONE
					in g'(1,ss)
					end
				      | g _ = NONE
				    val svs = map (val2sval m) vs
				in case g svs
				    of SOME sv => (click_record();
						   loop (substitute(m, lv, sv, tagInt 0), le, cont)
							 before app (unuseValue m) vs)
				     | _ =>
				       let val nm = addbind(m, lv, Record(lv, svs))
					   val nle = loop (nm, le, cont)
				       in if C.dead lvinfo then nle
					  else F.RECORD(rk, map sval2val svs, lv, nle)
				       end
				end
		      end (* fcRecord *)

                (* fcexp/fcSelect : F.value * int * LV.lvar * F.lexp -> F.lexp -- SELECT args *)
		fun fcSelect (v,i,lv,le) =
		      let val lvinfo = C.getInfo lv
		       in if C.dead lvinfo
			  then (click_deadval(); loop (m, le, cont))
			  else (case val2sval m v
				  of Record (lvr,svs) =>
				     let val sv = List.nth(svs, i)
				     in click_select();
					 loop (substitute(m, lv, sv, F.VAR lvr), le, cont)
				     end
				   | sv =>
				     let val nm = addbind (m, lv, Select(lv, sv, i))
					 val nle = loop (nm, le, cont)
				     in if C.dead lvinfo then nle
					else F.SELECT(sval2val sv, i, lv, nle)
				     end)
		     end (* fcSelect *)

                (* fcexp/fcBranch : F.primop * F.value list * F.lexp * F.lexp -> F.lexp -- BRANCH args *)
		fun fcBranch (po,vs,le1,le2) =
		      let val nvs = map substval vs
			  val npo = cpo po
			  val nle1 = loop (m, le1, #2)
			  val nle2 = loop (m, le2, #2)
		       in cont(m, F.BRANCH(npo, nvs, nle1, nle2))
		      end (* fcBranch *)

                (* fcexp/fcPrimop : F.primop * F.value list * LV.lvar * F.lexp -> F.lexp -- PRIMOP args *)
		fun fcPrimop (po,vs,lv,le) =
		      let val lvinfo = C.getInfo lv
			  val pure = not(PrimopUtil.effect(#2 po))
		       in if pure andalso C.dead lvinfo then (click_deadval(); loop (m, le, cont)) else
			  let val nvs = map substval vs
			      val npo = cpo po
			      val nm = addbind(m, lv, Var(lv,NONE))
			      val nle = loop (nm, le, cont)
			  in
			      if pure andalso C.dead lvinfo then nle
			      else F.PRIMOP(npo, nvs, lv, nle)
			  end
		      end (* fcPrimop *)

	        val fcexp_result =
		    (case le
		       of F.RET vs => cont(m, F.RET(map substval vs))
			| F.LET x => fcLet x
			| F.FIX x => fcFix x
			| F.APP x => fcApp x
			| F.TFN x => fcTfn x
			| F.TAPP x => fcTapp x
			| F.SWITCH x => fcSwitch x
			| F.CON x => fcCon x
			| F.RECORD x => fcRecord x
			| F.SELECT x => fcSelect x
			| F.RAISE (v,ltys) => cont(m, F.RAISE(substval v, ltys))
			| F.HANDLE (le,v) => cont(m, F.HANDLE(loop (m, le, #2), substval v))
			| F.BRANCH x => fcBranch x
			| F.PRIMOP x => fcPrimop x)

	       in fcexp_result
	      end (* fcexp *)

	  val contract_result =
	      case fcexp S.empty (M.empty, F.FIX([fdec], F.RET[F.VAR f]), #2)
		of F.FIX([fdec], F.RET[F.VAR f]) => fdec
	         | _ => bug ["contract: invalid lexp returned by fcexp"]

       in (* C.collect fdec; -- why not? -- multiple calls of contract? *)
           endContract ();
	   contract_result
      end (* contract -- starts line [350] *)

end (* structure FContract *)
