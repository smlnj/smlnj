(* Copyright 1997 Bell Laboratories *)
(* unify.sml *)

signature UNIFY =
sig

  datatype unifyFail
    = CIRC of Types.tyvar * Types.ty * SourceMap.region * SourceMap.region
        (* circularity *)
    | TYC of Types.tycon * Types.tycon * SourceMap.region * SourceMap.region
        (* tycon mismatch *)
    | TYP of Types.ty * Types.ty * SourceMap.region * SourceMap.region
        (* type mismatch *)
    | UBV of Types.tvKind * Types.ty * SourceMap.region * SourceMap.region
        (* UBOUND match *)
    | OVLD_F of string                  (* overload mismatch *)
    | OVLD_UB of string                 (* overload and user-bound ty var mismatch *)
    | EQ                                (* equality type required *)
    | REC                               (* record labels *)
    | UBVE of Types.tvKind              (* UBOUND, equality mismatch -- never used *)

  exception Unify of unifyFail

  val failMessage: unifyFail -> string

  val unifyTy : Types.ty * Types.ty * SourceMap.region  * SourceMap.region -> unit

end (* signature UNIFY *)

(*** type unification ***)
structure Unify: UNIFY =
struct


local
  structure S = Symbol
  structure T = Types
  structure TU = TypesUtil
  structure OLC = OverloadClasses
  structure PP = PrettyPrint
  structure ED = ElabDebug
  open Types

  fun bug msg = ErrorMsg.impossible("Unify: "^msg)

  (* debugging *)
  val debugging = ElabControl.unidebugging
  fun say msg = (Control_Print.say msg; Control_Print.flush ())
  fun newline () = say "\n"
  fun saynl msg = (Control_Print.say msg; Control_Print.say "\n"; Control_Print.flush ())
  fun dbsaynl (msg: string) =
      if !debugging then saynl msg else ()

  val ppType0 = PPType.ppType StaticEnv.empty
  fun debugPPType (msg,ty) =
      ED.debugPrint debugging (msg, ppType0, ty)

  fun ppType ty =
      PP.with_default_pp (fn ppstrm => ppType0 ppstrm ty)

in

(* for the time being, not region instrumenting the EQ and REC failures *)
datatype unifyFail
  = CIRC of tyvar * ty * SourceMap.region * SourceMap.region  (* circularity *)
  | EQ                               (* equality type required *)
  | TYC of tycon * tycon * SourceMap.region * SourceMap.region
                                     (* tycon mismatch *)
  | TYP of ty * ty * SourceMap.region * SourceMap.region     (* type mismatch *)
  | OVLD_F of string                 (* overload mismatch -- not a simple type *)
  | OVLD_UB of string                (* mismatch of OVLD and UBOUND tyvars *)
  | UBV of tvKind * ty * SourceMap.region * SourceMap.region  (* UBOUND match *)
  | UBVE of tvKind                   (* UBOUND, equality mismatch -- never used *)
  | REC                              (* record labels *)

(* failMessage : unifyFail -> string *)
fun failMessage (failure: unifyFail) =
    case failure
      of CIRC _ =>    "circularity"
       | EQ =>        "equality type required"
       | TYC _ =>     "tycon mismatch"
       | TYP _ =>     "type mismatch"
       | OVLD_F msg => "overload - bad instantiation"  (* or msg for more refined error messages*)
       | OVLD_UB _ => "overload - user bound tyvar" (* DBM: fixes bug 145 *)
       | UBVE _ =>    "UBOUND, equality mismatch"
       | UBV _ =>     "UBOUND match"
       | REC =>       "record labels"

exception Unify of unifyFail


(*************** misc functions *****************************************)

val eqLabel = Symbol.eq

(*
 * tyconEqprop tycon:
 *
 *    This function returns the eqprop of tycon for use in determining
 * when a CONty is an equality type.
 *
 * Note: Calling this function on ERRORtyc produces an impossible
 * because an ERRORtyc should never occur in a CONty and hence an eqprop
 * of an ERRORtyc should never be needed.
 *
 * [GK 5/7/07] The above note is not true. See bug271. Since an error
 * was already flagged, it seems harmless to return YES for the eqprop
 * to avoid possibly spurious eqprop related warnings.
 *
 * Calling this function on a DEFtyc also produces an impossible because
 * the current eqprop scheme is insufficiently expressive to describe
 * the possibilities. (Eg: first argument must be an eq type but not
 * necessarily the second.)  Because of this, it is currently necessary to
 * expand DEFtyc's before checking for equality types.
 *)
fun tyconEqprop (GENtyc { eq, ... }) =
    (case !eq of ABS => NO | ep => ep)
  | tyconEqprop (RECORDtyc _)  = YES
  | tyconEqprop (DEFtyc _) = bug "tyconEqprop: DEFtyc"
  | tyconEqprop (ERRORtyc) = YES
  | tyconEqprop _ = bug "unexpected tycon in tyconEqprop"

(*
 * fieldwise(just1,just2,combine,fields1,fields2):
 *
 *    This function merges two sorted lists of (label, type) pairs
 * (sorted by label) into a single sorted list of (label, type) pairs.
 * If (l1,t1) occurs in fields1 but l1 doesn't occur in fields2 then
 * (l1, just1 t1) occurs in the output.  Similarly with just2.
 * If (l, t1) occurs in fields1 and (l,t2) in fields2, then
 * (l, combine t1 t2) occurs in the output.
 *)
fun fieldwise(_,just2,_,[],fields2) = map (fn (n,t) => (n,just2 t)) fields2
  | fieldwise(just1,_,_,fields1,[]) = map (fn (n,t) => (n,just1 t)) fields1
  | fieldwise(just1,just2,combine,r1 as ((n1,t1)::rest1),r2 as ((n2,t2)::rest2)) =
      if eqLabel(n1,n2) then
	(n1,combine(t1,t2))::(fieldwise(just1,just2,combine,rest1,rest2))
      else if TU.gtLabel(n2,n1) then
	(n1,just1 t1)::(fieldwise(just1,just2,combine,rest1,r2))
      else
	(n2,just2 t2)::(fieldwise(just1,just2,combine,r1,rest2))


(* filter out the non-equality-types from a list of types *)
fun filterEq (nil) = nil
  | filterEq (ty::tys) =
    if EqTypes.isEqType ty then ty :: filterEq tys
    else filterEq tys

(* not used, so far --
(* templateFilter : bool list * 'a list -> 'a list
 * REQUIRE: bool list and 'a list have same length *)
fun templateFilter (nil, nil, rs) = rev rs
  | templateFilter (true::bs, x::xs, rs) = templateFilter (bs, xs, x::rs)
  | templateFilter (false::bs, x::xs, rs) = templateFilter (bs, xs, rs)
  | templateFilter _ = bug "templateFilter - lengths"

(* strictArgFilter : ty -> ty list
 *  REQUIRE: ty argument is a compound (CONty) type
 *  NOT USED *)
fun strictArgFilter (CONty (DEFtyc{strict,...}, args)) =
    templateFilter (strict, args, nil)
  | strictArgFilter (CONty (_, args)) = args
  | strictArgFilter _ = bug "strictArgFilter"
*)

(******************** occurCheck and adjustType functions ******************************)

(* occurCheck belongs in TypesUtil (it is self-contained except for Occurs exception) *)

exception Occurs

(* occurCheck : T.tyvar * T.ty -> bool
 * ASSERT: ty is not POLYty, etc.
 *  This does a "superficial" occurrence check, ignoring the possibility that a DEFtyc
 *  application could be reduced, possibly eliminating tyvar occurrences. *)
fun occurCheck (tyvar, ty) =
    (* scan : T.ty -> unit; raises Occur *)
    let fun scan WILDCARDty = ()
	  | scan (MARKty(ty,_)) = scan ty
	  | scan (VARty tyvar') =
	      if TU.eqTyvar (tyvar, tyvar') then raise Occurs else ()
	  | scan (CONty(tycon,args)) =
	      app scan args
	  | scan _ = ()
	val _ = if !debugging
		then (say "OC: "; ppType (VARty tyvar); say "; "; ppType ty; newline())
		else ()
    in (scan ty; dbsaynl "OC false"; false)
       handle Occurs => (dbsaynl "OC true"; true)
    end

fun instantiate (tyvar, ty) =
    tyvar := INSTANTIATED ty

(* adjustType : T.tyvar * int * bool * T.ty * region * region -> unit
 *  propagate depth and eq while checking for circularities in the
 *  type ty that is going to unify with tyvar var (i.e. tyvar will instantiate
 *  to ty -- unchanged, unreduced, but tyvars occurring in ty need to be
 *  adjusted to be "compatible (depth, eq) with tyvar ) *)
fun adjustType (tyvar, depth, eq, ty, region1, region2) =
    (* ASSERT: ty <> VARty tyvar *)
    (* region1 is for var, region2 is for ty and may update through iter *)
    let val _ = if !debugging
		then (say ">>> adjustType: "; ppType(VARty tyvar); say "  "; ppType ty; newline())
		else ()
        (* scan : bool * T.ty * region -> unit *)
	fun scan (_, WILDCARDty, _) = ()
	  | scan (eq, MARKty(ty, region2'), _) = scan (eq, ty, region2')
	  | scan (eq, ty' as VARty(tyvar' as ref tvkind), region2) =
	      (case tvkind
		 of INSTANTIATED ty =>  (* shouldn't happen? ty should be pruned? *)
		      (dbsaynl "adjustType INSTANTIATED";
		       scan (eq,ty,region2))
		  | OPEN{kind=k,depth=d,eq=e} =>
		      (* occurrence check for circularity;
		       * if passed, update eq and depth of tyvar' *)
		      if TU.eqTyvar (tyvar, tyvar')
		      then raise Unify (CIRC(tyvar, ty', region1, region2))
		      else (case k
			      of FLEX fields =>
				  (* recurse into FLEX field types *)
				  app (fn (l,t) => adjustType(tyvar,depth,e,t,region1,region2))
				      fields
			       | _ => ();
			    tyvar' := OPEN{depth=Int.min(depth,d), eq=eq orelse e, kind=k})
		  | UBOUND{depth=d,eq=eq2,name} =>
		      (* check if eq is compatible and propagate depth *)
		      if eq andalso not eq2 (* equality type required *)
		      then raise Unify EQ
		      else if depth < d
		      then tyvar' := UBOUND{depth=depth,eq=eq2,name=name}
		      else ()
		  | OVLDV{sources,eq=eq2} =>
		      (* circularity can't happen, because var can't be OVLD* *)
		      tyvar' := OVLDV{sources = sources, eq = eq orelse eq2}
		  | (OVLDI _ | OVLDW _) => () (* no eq propagation necessary *)
		      (* circularity can't happen, because tyvar can't be OVLD* *)
		  | LBOUND _ => bug "unify:adjustType:LBOUND")
	  | scan (eq, ty as CONty(DEFtyc{tyfun=TYFUN{body,...},...}, args), region2) =
	      ((* app (fn t => scan (false, t, region2)) args;
                * -- caused Unify(CIRC) exn to be raised (occur check) in
		* tests/typing/tests/18.sml when typing function g ('Y <=> 'Y t) *)
	       scan (eq, TU.headReduceType ty, region2))    (* critical occur check missed; 18.sml *)
	      (* A headReduceType is done here to ensure that both
	       * depth and eq are propagated into only "relevant" tyvars of ty
	       * (see tests/typing/tests/18.sml).
	       * avoiding an infinite loop that could occur if we do the
               * occurrence check against a nonrelevant tyvar within an argument
	       * of ty. To avoid the head-reduce, we would need to figure out eq
	       * propagation by analyzing the definition of the DEFtyc. [DBM 2021.10.28]
	       * [[scan should only do the occurrence check and
	       * not propagate eq to the args.
	       * MLRISC/library/dynamic-array.sml's checkArray
	       * is an example. [GK 2/24/08] ??? ]] *)
              (* Note that this may involve redundant reductions -- scan may be applied
               * multiple times to the same type, causing multiple headReduceType
	       * calls on a type. *)
 	  | scan (eq, CONty(tycon,args), region2) =
              (* tycon is not a DEFtyc in this case, hence all args are "relevant"
	       * because non-DEFtyc tycons are strict. [DBM, 2020.04.12] *)
	      (case tyconEqprop tycon
		 of OBJ => app (fn t => scan (false, t, region2)) args
		  | YES => app (fn t => scan (eq, t, region2)) args
		  | _ =>
		      if eq
		      then raise Unify EQ
		      else app (fn t => scan (false, t, region2)) args)
 (* BUG? why don't these cases blow up (in tyconEqprop) when scan is applied
    to arguments that are unreduced applications of DEFtycs? *)
          | scan (_, POLYty _, _) = bug "adjustType[POLYty]"
          | scan (_, IBOUND _, _) = bug "adjustType[IBOUND]"
	  | scan _ = bug "adjustType[ty arg]"
     in scan (eq, ty, region2); dbsaynl "<<< adjustType"
    end (* adjustType *)

(*************** unify functions *****************************************)

(* OVLD can be instantiated to a type that will have to (after overload
   resolution) turn out to be a primitive type in the appropriate overload class.
 * OVLD (when unified with another OVLD)
 * UBOUND cannot be instantiated, but it's depth property can be reduced
 * OPEN/FLEX can merge with another FLEX or instantiate a META
 * OPEN/META can be instantiated to anything
 *)

(* reorder two tyvars in descending order according to the ordering
 * OVLDI > OVLDW > OVLDV > UBOUND > OPEN/FLEX > OPEN/META *)
fun sortVars(v1 as ref i1, v2 as ref i2) =
    case (i1,i2)
      of (OVLDI _, _) => (v1,v2)
       | (_, OVLDI _) => (v2,v1)
       | (OVLDW _, _) => (v1,v2)
       | (_, OVLDW _) => (v2,v1)
       | (OVLDV _, _) => (v1,v2)
       | (_, OVLDV _) => (v2,v1)
       | (UBOUND _, _) => (v1,v2)
       | (_, UBOUND _) => (v2,v1)
       | (OPEN{kind=FLEX _,...}, _) => (v1,v2)
       | (_, OPEN{kind=FLEX _,...}) => (v2,v1)
       | _ => (v1,v2) (* both OPEN/META *)

(* unifyTy expects that there are no POLYtys with 0-arity;
 *  CONty(DEFtyc, _) are reduced only if absolutely necessary.
 *   [DBM, 20201.10] But consider always head reducing both types, which
 *   would simplify the strictness analysis and avoid having to reduce one
 *   or both types after an initial unification failure. This might not be
 *   too expensive  after all. Note that we have already done this for the
 *   cases where  one type is a type variable (see the 2 calls of instTyvar). *)
fun unifyTy(type1, type2, reg1, reg2) =
    let val _ = if !debugging
		then (say ">>> unifyTy: "; ppType type1; say "; "; ppType type2; newline())
		else ()
	fun unifyRaw (type1, type2, reg1, reg2) =
	    (if !debugging
	     then (say ">>> unifyRaw: "; ppType type1; say "; "; ppType type2; newline())
	     else ();
	    (case (TU.prune type1, TU.prune type2)
	      of (MARKty (ty1, reg1'), type2) => unifyRaw(TU.prune ty1, type2, reg1', reg2)
	       | (type1, MARKty (ty2, reg2')) => unifyRaw(type1, TU.prune ty2, reg1, reg2')
		  (* missing region args to unify, so MARKs are discarded *)
	       | (VARty tyvar1, VARty tyvar2) =>
		   unifyTyvars(tyvar1, tyvar2, reg1, reg2)
		     (* this used to take type1 and type2 as args *)
	       | (VARty tyvar1, type2) =>       (* type2 may be WILDCARDty *)
		   instTyvar(tyvar1, type2, reg1, reg2)
	       | (type1, VARty var2) =>         (* type1 may be WILDCARDty *)
		   instTyvar(var2, type1, reg2, reg1)
	       | (CONty(ERRORtyc, _), _) => ()
	       | (_, CONty(ERRORtyc, _)) => ()
	       | (ty1 as CONty(tycon1,args1), ty2 as CONty(tycon2,args2)) =>
		   if TU.eqTycon(tycon1,tycon2) then
		       (* Because tycons are equal (and not ERRORtyc), they must have the
			  same arity and strictness signatures [CLAIM]. Thus lengths of
			  args1 and args2 are the same. We unify only "relevant"
			  args, as determined by the strictness signature in the
			  case where the tycons are DEFtycs. For non-DEFtyc tycons,
			  all arguments are "relevant" and are unified.
			  [GK 4/28/07; DBM 2021.10.28] *)
		       (case tycon1
			 of DEFtyc{strict, ...} =>  (* unify common relevant args *)
			    let fun unifyRelArgs ([],[],[]) = ()
				  | unifyRelArgs (true::ss, ty1::tys1, ty2::tys2) =
				      (unifyTy(ty1,ty2,reg1,reg2); unifyRelArgs(ss,tys1,tys2))
				  | unifyRelArgs (false::ss, _::tys1, _::tys2) =
				      unifyRelArgs (ss,tys1,tys2)
				  | unifyRelArgs _ = bug "unifyTy: arg ty lists wrong length"
			    in unifyRelArgs (strict, args1, args2)
			    end
			  | _ => ListPair.appEq (fn (t1,t2) => unifyTy(t1,t2,reg1,reg2))
					      (args1,args2))
		   else (* tycon1, tycon2 not "equal", so try reducing one or the other *)
		     let val (tycon1', tycon2') =
			      (case (tycon1, tycon2)
				 of (DEFtyc _, DEFtyc _) =>
				      (TU.headReduceType ty1, TU.headReduceType ty2)
				  | (DEFtyc _, _) =>
				      (TU.headReduceType ty1, ty2)
				  | (_, DEFtyc _) =>
				      (ty1, TU.headReduceType ty2)
				  | _ => (* neither can be reduced, so give up! *)
				      raise Unify (TYC(tycon1,tycon2,reg1,reg2)))
		      in (* neither tycon1' nor tycon2' is head-reducible *)
			  unifyRaw (tycon1', tycon2', reg1, reg2)
		     end
	      (* if one of the types is WILDCARDty, propagate it down into the
	       * other type to eliminate tyvars that might otherwise cause
	       * generalizeTy to complain. *)
	       | (WILDCARDty, CONty(_, args2)) =>
		    app (fn x => unifyTy(x, WILDCARDty, reg1, reg2)) args2
	       | (CONty(_, args1), WILDCARDty) =>
		    app (fn x => unifyTy(x, WILDCARDty, reg1, reg2)) args1
	       | (WILDCARDty,_) => ()
	       | (_,WILDCARDty) => ()
	       | (ty1,ty2) => raise Unify (TYP(ty1,ty2,reg1,reg2)));
	    dbsaynl "<<< unifyRaw")

     in unifyRaw(type1, type2, reg1, reg2)
    end (* unifyTy *)

and unifyTyvars (var1: tyvar, var2: tyvar, reg1, reg2) =
    let val _ = dbsaynl ">>> unifyTyvars"
	fun unify(var1 as ref i1, var2 as ref i2) =
	    (* ASSERT: var1 <> var2  -- cf equality test in let-body of unifyTyvars, and
	     *         var1 =< var2 by sortVars, same reason
	     *         var1 and var2 are pruned, i.e. are _not_ INSTANTIATED *)
	    (dbsaynl ">>> unifyTyvars..unify";
	     case i1
	      of OVLDI sources1 =>
		 ((case i2
		    of OVLDI sources2 =>
		       (dbsaynl "@unifyTyvars[OVLDI/OVLDV]";
			var1 := OVLDI (sources1 @ sources2))
		     | OVLDW _ =>
		       (dbsaynl "@unifyTyvars[OVLDI/OVLDW]";
			raise Unify (OVLD_F "OVLDI/OVLDW"))
		     | OVLDV{sources=sources2,eq=eq2} =>
		       (dbsaynl "@unifyTyvars[OVLDI/OVLDV]";
			var1 := i1)
		     | OPEN{kind=FLEX _,...} =>
		       (dbsaynl "@unifyTyvars[OVLDI/OPEN:FLEX]";
			raise Unify (OVLD_F "OVLDI/OPEN:FLEX"))
		     | OPEN{eq = eq2, ...} =>
		       (dbsaynl ("@unifyTyvars[OVLDI/OPEN] eq: "^Bool.toString eq2);
			var1 := i1)  (* all Int types are equality types *)
		     | UBOUND _ => raise Unify (OVLD_UB "OVLDI")
		     | _ => bug "unifyTyvars OVLDI");
	          instantiate (var2, MARKty(VARty var1, reg1)))

              | OVLDW sources1 =>
		 ((case i2
		    of OVLDW sources2 =>
		       (dbsaynl ("@unifyTyvars[OVLDW/OVLDW]");
			var1 := OVLDW (sources1 @ sources2))
		     | OVLDV{sources=sources2,eq=eq2} =>
		       (dbsaynl ("@unifyTyvars[OVLDI/OVLDV]");
			var1 := i1)
		     | OPEN{kind=FLEX _,...} => raise Unify (OVLD_F "OVLDW/OPEN:FLEX")
		     | OPEN{eq = eq2, ...} =>
		       (dbsaynl ("@unifyTyvars[OVLDW/OPEN] eq: "^Bool.toString eq2);
			var1 := i1)  (* all Word types are equality types *)
		     | UBOUND _ => raise Unify (OVLD_UB "OVLDW")
		     | _ => bug "unifyTyvars OVLDW");
	          instantiate (var2, MARKty(VARty var1, reg1)))

	      | OVLDV{sources,eq=eq1} =>
		 ((case i2
		    of OVLDV{sources=sources2,eq=eq2} =>
		       (dbsaynl ("@unifyTyvars[OVLDV/OVLDV]");
			var1 := OVLDV{sources=sources@sources2, eq = eq1 orelse eq2})
		     | OPEN{kind=FLEX _,...} => raise Unify (OVLD_F "OVLDV/OPEN:FLEX")
		     | OPEN{eq = eq2, ...} =>
		       (dbsaynl ("@unifyTyvars[OVLDV/OPEN] eq: "^Bool.toString eq2);
			if eq2 andalso not eq1
			then var1 := OVLDV{sources=sources, eq = true}
			else ())
		     | UBOUND _ => raise Unify (OVLD_UB "OVLDV")
		     | _ => bug "unifyTyvars OVLDI");
	          instantiate (var2, MARKty(VARty var1, reg1)))

	       | UBOUND {depth=d1,eq=e1,name} =>
		 (* Note: UBOUND tyvars unify only if equal *)
		  (case i2
		     of OPEN{kind=META,eq=e2,depth=d2} =>
			 if e1 orelse (not e2)
			 then (if d2 < d1
				   then var1 := UBOUND{depth=d2,eq=e1,name=name}
				   else ();
			       instantiate (var2, MARKty(VARty var1, reg1)))
			 else raise Unify (UBV(i1,VARty var2,reg1,reg2))
		      | _ => raise Unify (UBV(i1,VARty var2,reg1,reg2)))

	       | OPEN{kind=k1 as FLEX f1,depth=d1,eq=e1} =>
		  (case i2
		     of OPEN{kind=k2,eq=e2,depth=d2} =>
			 let val d = Int.min(d1,d2)
			     val e = e1 orelse e2
			  in case k2
			       of FLEX f2 =>
				   (app (fn (l,t) => adjustType(var1,d,e,t,reg1,reg2)) f2;
				    app (fn (l,t) => adjustType(var2,d,e,t,reg1,reg2)) f1;
				    var1 :=
				      OPEN{depth=d, eq=e,
					   kind=FLEX(merge_fields(true,true,f1,f2,reg1,reg2))};
				    instantiate (var2, MARKty(VARty var1, reg1)))
			        | META =>
				   (app (fn (l,t) => adjustType(var2,d,e,t,reg1,reg2)) f1;
				    var1 := OPEN{kind=k1,depth=d,eq=e};
				    instantiate (var2, MARKty(VARty var1, reg1)))
			 end
		      | _ => bug "unifyTyvars OPEN/FLEX")

	       | OPEN{kind=META,depth=d1,eq=e1} =>
		  (case i2
		     of OPEN{kind=META,depth=d2,eq=e2} =>
			 let val d = Int.min(d1,d2)
			     val e = e1 orelse e2
			  in var1 := OPEN{kind=META,depth=d,eq=e};
			     instantiate (var2, MARKty(VARty var1, reg1))
			 end
		      | _ => bug "unifyTyvars OPEN/META")

	       | _ => bug "unifyTyvars tyvar1")
               (* end unify *)

     in if TU.eqTyvar(var1,var2)
        then (dbsaynl "<<< unifyTyvars[equal]")  (* immediate success! *)
        else (dbsaynl "### unifyTyvars[not equal]"; unify(sortVars(var1,var2)))
    end (* unifyTyvars *)

(* instTyvar: T.tyvar * T.ty * region * region -> unit
 * instTyvar(tv,ty,reg1,reg2) -- instantiate tyvar tv to type ty with (eq,depth) propagation.
 * ASSERT: ty is pruned and is not a VARty
 *   (otherwise unifyTyvars would have been used instead). *)
and instTyvar (tyvar as ref(OPEN{kind=META,depth,eq}), ty, reg1, reg2) =
      (case ty
         of WILDCARDty => ()
	  | MARKty(ty1, reg2') => instTyvar (tyvar, ty1, reg1, reg2')
	  | _ => (* BUG?: may need further internal reductions to avoid spurious tyvar occurrences? *)
	    let val ty' = if occurCheck (tyvar, ty) then TU.headReduceType ty else ty
                (* see if occurCheck is removed if we head-reduce ty;
                 * adjustType will do another occurrence check in case the reduction
		 * did not "solve" the occurrence check. *)
	     in if !debugging
		then (say "@instTyvar[OPEN]:"; ppType (VARty tyvar); say " -> "; ppType ty'; newline())
		else ();
		(case ty'
		  of VARty tyvar' =>
		     if TU.eqTyvar (tyvar, tyvar')  (* bugs/tests/bug0573.sml *)
		     then ()  (* do nothing if ty head-reduced to tyvar *)
		     else (adjustType(tyvar, depth, eq, ty', reg1, reg2);
			   instantiate (tyvar, ty'))
		   | _ => (adjustType(tyvar, depth, eq, ty', reg1, reg2);
			   instantiate (tyvar, ty')))
	    end)

  | instTyvar (tyvar as ref(OPEN{kind=FLEX fields,depth,eq}), ty, reg1, reg2) =
      (* BUG? : could ty be a VARty(ref(OPEN{kind=FLEX _,...})) also? If so, merge them? *)
      (case TU.headReduceType ty
	 of CONty(RECORDtyc field_names, field_types) =>
              let val record_fields = ListPair.zip (field_names,field_types)
               in app (fn t => adjustType(tyvar,depth,eq,t,reg1,reg2)) field_types;
                  merge_fields(false, true, fields, record_fields, reg1, reg2);
                  instantiate (tyvar, ty)
              end
	  | MARKty(ty1, reg2') => instTyvar (tyvar, ty1, reg1, reg2')
          | WILDCARDty => (* propagate WILDCARDty to the fields *)
	      (app (fn (lab,ty) => unifyTy(WILDCARDty,ty,reg1,reg2)) fields)
          | _ => raise Unify (TYP (VARty tyvar, ty, reg1, reg2)))

  | instTyvar (tyvar as ref(OVLDV{eq,...}), ty, reg1, reg2) = (* operator overloading tyvar *)
      (debugPPType(">>instTyvar[OVLDV]",ty);
       case ty
	 of WILDCARDty => ()  (* error survival *)
	  | MARKty(ty1, reg2') => instTyvar(tyvar, ty1, reg1, reg2')
	  | _ =>
	     (case TU.headReduceType ty
	        of (ty' as CONty(tycon,nil)) =>
		    (* checkiing that it is  a type constant, but not checking if
		     * the instantiation is compatible with overload class until
		     * overloading resolution *)
		     (debugPPType("instTyvar[OVLD] OK: ",ty');
		      if eq then adjustType(tyvar, T.infinity, eq, ty', reg1, reg2) else ();
		      instantiate (tyvar, ty'))
		 | ty' as VARty tyvar' =>
		     (case !tyvar'
		       of OPEN _ =>
			    (dbsaynl "### instTyvar[OVLDV/OPEN]";
			     instantiate (tyvar', VARty tyvar))  (* was: tyvar' := !tyvar *)
			| (OVLDI _ | OVLDW _) => (* eq = true for these *)
			    (dbsaynl "### instTyvar[OVLDV/OPEN]";
			     instantiate (tyvar, ty'))  (* was: tyvar := !tyvar' *)
		        | OVLDV _ => dbsaynl "### instTyvar[OVLDV/OVLDV]"
			    (* do nothing?  Leave original OVLDV in alone?
			     * likely same tyvar, so instantiating may create a cycle *)
		        | UBOUND {name,...} =>
			    bug ("instTyvar[OVLDV/UBOUND]: " ^ S.name name)
			    (* can't unify a UBOUND with an overload tyvar *)
		        | LBOUND _ => bug "instTyvar[OVLDV/LBOUND]" (* not relevant *)
		        | INSTANTIATED _ => bug "instTyvar[OVLDV/INSTANTIATED]")
			    (* headReduceType cannot return an instantiated tyvar, it prunes *)
		     (* POLICY: literal overloads take priority over function overloads (?) *)
		 | ty' => (* incompatible ty *)
		     (debugPPType ("instTyvar[OVLDV] Fail: ", ty');
		      raise Unify (OVLD_F "OVLDV tyvar unified with incompatible type"))))

  | instTyvar (tyvar as ref(OVLDI _), ty, reg1, reg2) =  (* int literal overloading tyvar *)
      (debugPPType(">>instTyvar[OVLDI]",ty);
       case ty
	 of WILDCARDty => ()  (* error survival *)
	  | MARKty(ty1, reg2') => instTyvar(tyvar, ty1, reg1, reg2')
	  | _ =>
	     (case TU.headReduceType ty
	        of (ty' as CONty(tycon,nil)) =>
		     (* checkiing that it is  a type constant in Int overloading class *)
		     (debugPPType("instTyvar[OVLDI]: ",ty');
		      if OLC.inClass(ty', OLC.intClass)
		      then instantiate (tyvar, ty')
		      else raise Unify (OVLD_F "INT tyvar instantiated to non-int type"))
		 | VARty (tyvar' as ref (OPEN _ | OVLDI _ | OVLDV _)) => tyvar' := !tyvar  (* ok *)
		 | ty' =>  (* incompatible reduced ty *)
		     (debugPPType ("instTyvar[OVLDI] Fail: ", ty');
		      raise Unify (OVLD_F "INT tyvar unified with incompatible type"))))

  | instTyvar (tyvar as ref(OVLDW _), ty, reg1, reg2) =  (* word literal overloading tyvar *)
      (debugPPType(">>instTyvar[OVLDW]",ty);
       case ty
	 of WILDCARDty => ()  (* error survival *)
	  | MARKty(ty1, reg2') => instTyvar(tyvar, ty1, reg1, reg2')
	  | _ =>
	     (case TU.headReduceType ty
	        of (ty' as CONty(tycon,nil)) =>  (* possible word tycon *)
		     (* checkiing that it is  a type constant in Word overloading class *)
		     (debugPPType("instTyvar[OVLDW]: ",ty');
		      if OLC.inClass(ty', OLC.wordClass)
		      then instantiate (tyvar, ty')
		      else raise Unify (OVLD_F "WORD tyvar unified with non-word type"))
		 | VARty (tyvar' as ref (OPEN _ | OVLDW _ | OVLDV _)) => tyvar' := !tyvar  (* ok *)
		 | ty' => (* incompatible reduced ty *)
		     (debugPPType ("instTyvar[OVLDW] Fail: ", ty');
		      raise Unify (OVLD_F "WORD tyvar unified with incompatible type"))))

  | instTyvar (tyvar as ref(i as UBOUND _), ty, reg1, reg2) =
      (case ty
         of WILDCARDty => ()
	  | MARKty(ty1, reg2') => instTyvar(tyvar, ty1, reg1, reg2')
          | _ => (* check if ty reduces to same tyvar *)
	     (case TU.headReduceType ty
	       of VARty tyvar' =>
 		    if TU.eqTyvar (tyvar, tyvar') then ()  (* tyvar cannot be instantiated *)
		    else (dbsaynl "instTyvar[UBOUND]: raising Unify";
			  raise Unify (UBV (i, ty, reg1, reg2)))
	        | ty' => raise Unify (UBV (i, ty', reg1, reg2))))
              (* could return the ty for error msg*)

  | instTyvar (ref(INSTANTIATED _),_,_,_) = bug "instTyvar: INSTANTIATED"
  | instTyvar (ref(LBOUND _),_,_,_) = bug "instTyvar: LBOUND"

(* merge_fields(extra1,extra2,fields1,fields2):
 *  This function merges the 2 sorted field lists.  Fields occuring
 *  in both lists have their types unified.  If a field occurs in only
 *  one list, say fields{i} then if extra{i} is true, an Unify error
 *  is raised. *)
and merge_fields(extra1, extra2, fields1, fields2, reg1, reg2) =
    let fun extra allowed t =
	  if not allowed
	  then raise Unify REC
	  else t
     in fieldwise(extra extra1, extra extra2,
                  (fn (t1,t2) => (unifyTy(t1, t2, reg1, reg2); t1)),
		  fields1, fields2)
    end

end (* local *)
end (* structure Unify *)
