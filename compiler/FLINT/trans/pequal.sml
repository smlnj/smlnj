(* pequal.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature PEQUAL =
  sig
    type toTcLt = (Types.ty -> Lty.tyc) * (Types.ty -> Lty.lty)
    (*
     * Constructing generic equality functions; the current version will
     * use runtime polyequal function to deal with abstract types. (ZHONG)
     *)
    val equal : {getStrEq : unit -> PLambda.lexp,
		 getIntInfEq : unit -> PLambda.lexp,
		 getPolyEq : unit -> PLambda.lexp}
		 * StaticEnv.staticEnv
		 -> (Types.ty * Types.ty * toTcLt) -> PLambda.lexp

    val debugging : bool ref

  end (* signature PEQUAL *)


structure PEqual : PEQUAL =
  struct

    structure DA = Access
    structure EM = ErrorMsg
    structure T  = Types
    structure BT = BasicTypes
    structure LT = Lty
    structure LK = LtyKernel
    structure LD = LtyDef
    structure LB = LtyBasic
    structure LE = LtyExtern
    structure TU = TypesUtil
    structure SE = StaticEnv
    structure PO = Primop
    structure PU = PrimopUtil
    structure PP = PrettyPrint
    open Types PLambda
    (* mentions Target *)

    val debugging = ref false
    fun bug msg = ErrorMsg.impossible("PEqual: "^msg)
    val say = Control.Print.say

    type toTcLt = (ty -> LT.tyc) * (ty -> LT.lty)

    val --> = BT.-->
    infix -->

  (*
   * MAJOR CLEANUP REQUIRED ! The function mkv is currently directly taken
   * from the LambdaVar module; I think it should be taken from the
   * "compInfo". Similarly, should we replace all mkLvar in the backend
   * with the mkv in "compInfo" ? (ZHONG)
   * DBM: is it worth cleaning up, what harm might come from using a single
   * global generator -- maybe resetting between compilations?
   *)
    val mkv = LambdaVar.mkLvar

  (** translating the typ field in DATACON into lty; constant datacons
      will take ltc_unit as the argument *)
    fun toDconLty (toTyc, toLty) ty = (case ty
	   of POLYty{sign, tyfun=TYFUN{arity, body}} =>
		if BT.isArrowType body then toLty ty
		else toLty (POLYty{sign=sign,
				   tyfun=TYFUN{arity=arity,
					       body=BT.-->(BT.unitTy, body)}})
	    | _ => if BT.isArrowType ty then toLty ty
		   else toLty (BT.-->(BT.unitTy, ty))
	  (* end case *))

  (*
   * Is TU.dconType necessary, or could a variant of transTyLty that
   * just takes tyc and domain be used in transDcon???
   *)
    fun transDcon(tyc, {name,rep,domain}, toTcLt) =
	  (name, rep, toDconLty toTcLt (TU.dconType(tyc,domain)))

    val (trueDcon', falseDcon') = let
	  val lt = LD.ltc_parrow(LB.ltc_unit, LB.ltc_bool)
	  fun h (DATACON{name, rep, ...}) = (name, rep, lt)
	  in
	    (h BT.trueDcon, h BT.falseDcon)
	  end

  (* synthesize a if-then-else from a switch *)
    fun COND (a, b, c) =
          SWITCH(a, BT.boolsign, [
	      (DATAcon(trueDcon', [], mkv()),b),
	      (DATAcon(falseDcon', [], mkv()),c)
	    ], NONE)

    val (trueLexp, falseLexp) = let
          val unitLexp = RECORD []
          in
	    (CON (trueDcon', [], unitLexp), CON (falseDcon', [], unitLexp))
          end

    fun argType(domain, []) = domain
      | argType(domain, args) = TU.applyTyfun(TYFUN{arity=length args,body=domain},args)

    fun reduceTy ty = (case TU.headReduceType ty
	   of POLYty{tyfun=TYFUN{body,...},...} => reduceTy body
	    | ty => ty
	  (* end case *))

  (* Given a list of data constructors; return its signature and a list
   * of value-carrying data constructors
   *)
    fun getCsig dcons = let
          fun isConst (DA.CONSTANT _) = true
	    | isConst (DA.LISTNIL) = true
	    | isConst _ = false
	  fun h ([], c, v, rds) = (DA.CSIG(v,c), rev rds)
	    | h ((dc as {rep=a,domain,name})::r, c, v, rds) = if isConst a
		then h(r, c+1, v, rds)
		else h(r, c, v+1, dc::rds)
          in
	    h(dcons, 0, 0, [])
          end

    fun expandREC (family as {members: T.dtmember vector, ...}, stamps, freetycs) = let
          fun g (RECtyc i) = let
	        val {tycname,dcons,arity,eq,lazyp,sign} =
		       Vector.sub(members,i)
		val s = Vector.sub(stamps, i)
		in
		  GENtyc{
		      stamp=s,arity=arity, eq=ref(YES),
		      kind=DATATYPE{
			  index=i, family=family,root=NONE,
			  stamps=stamps, freetycs=freetycs, stripped=false
			},
		      path=InvPath.IPATH[tycname],
		      stub = NONE
		    }
	        end
	    | g (FREEtyc i) = List.nth(freetycs, i)
	    | g x = x
	  fun f (CONty(tyc,tyl)) = CONty(g tyc, map f tyl)
	    | f (x as IBOUND _) = x
	    | f (MARKty (t, _)) = f t
	    | f _ = bug "unexpected type in expandREC"
          in
	    f
          end

    exception Poly

  (* are two types equal?  Raises Poly if POLYty or type variables encountered *)
    fun equivType (ty, ty') = let
	  fun eq (ty as CONty(tycon, args), ty' as CONty(tycon', args')) =
		  (if TU.eqTycon(tycon, tycon')
		   then ListPair.all equivType (args,args')
		   else (equivType(TU.reduceType ty, ty')
			 handle ReduceType =>
			     (equivType(ty,TU.reduceType ty')
			handle ReduceType => false)))
	    | eq (MARKty (t, _), t') = eq(t, t')
	    | eq (t, MARKty (t', _)) = eq(t, t')
	    | eq (VARty _, _) = raise Poly
	    | eq (_, VARty _) = raise Poly
	    | eq (POLYty _, _) = raise Poly
	    | eq (_, POLYty _) = raise Poly
	    | eq _ = false
	in
	  eq(TU.prune ty, TU.prune ty')
	end

  (****************************************************************************
   *                   Lambda Types for equality                              *
   ****************************************************************************)

    val boolty = LB.ltc_bool
    fun eqLty lt = LD.ltc_parrow(LD.ltc_tuple [lt, lt], boolty)
    fun intEqTy sz = eqLty (LB.ltc_num sz)
    val uintEqTy = intEqTy  (* unsigned numbers same as signed in LT *)
    val booleqty = eqLty (LB.ltc_bool)
(* FIXME: since real is **not** an equality type, this definition is not needed!!! *)
    val realeqty = eqLty (LB.ltc_real)

    exception Notfound

(****************************************************************************
 *              equal --- the equality function generator                   *
 ****************************************************************************)
    fun equal
        ({getStrEq, getIntInfEq, getPolyEq}, env)
	(polyEqTy : ty, concreteType : ty, toTcLc as (toTyc, toLty)) = let
	  val cache : (ty * lexp * lexp ref) list ref = ref nil
	  fun enter ty = let
		val v = VAR(mkv())
		val r = ref v
		in
		  if !debugging
		    then PP.with_pp (EM.defaultConsumer())
			  (fn ppstrm => (PP.string ppstrm "enter: ";
			     PPType.resetPPType(); PPType.ppType env ppstrm ty))
		    else ();
		  cache := (ty, v, r) :: !cache; (v,r)
		end
	  fun find ty = let
		fun f ((t,v,e)::r) = if equivType(ty,t) then v else f r
		  | f [] = (if !debugging
			    then say "equal.sml-find-notfound\n" else ();
			    raise Notfound)
		in
		  if !debugging
		    then PP.with_pp (EM.defaultConsumer())
			 (fn ppstrm => (PP.string ppstrm "find: ";
					PPType.resetPPType();
					PPType.ppType env ppstrm ty))
		    else ();
		  f (!cache)
		end

	  fun eqTy ty = eqLty(toLty ty)
	  fun ptrEq(p, ty) = PRIM(p, eqTy ty, [])
	  fun prim(p, lt) = PRIM(p, lt, [])

	  val dSz = Target.defaultIntSz  (* 31 or 63 *)
	  fun numKind tyc =
		if      TU.equalTycon(tyc, BT.intTycon)     then SOME(PO.INT dSz)
		else if TU.equalTycon(tyc, BT.wordTycon)    then SOME(PO.UINT dSz)
		else if TU.equalTycon(tyc, BT.word8Tycon)   then SOME(PO.UINT dSz)  (* could be 8? *)
		else if TU.equalTycon(tyc, BT.charTycon)    then SOME(PO.INT dSz)
		else if TU.equalTycon(tyc, BT.int32Tycon)   then SOME(PO.INT 32)
		else if TU.equalTycon(tyc, BT.word32Tycon)  then SOME(PO.UINT 32)
		else if TU.equalTycon(tyc, BT.int64Tycon)   then SOME(PO.INT 64)
		else if TU.equalTycon(tyc, BT.word64Tycon)  then SOME(PO.UINT 64)
		else NONE

	  fun atomeq (tyc, ty) = (case numKind tyc
		 of SOME(PO.INT sz) => prim(PU.mkIEQL sz, intEqTy sz)
		  | SOME(PO.UINT sz) => prim(PU.mkUIEQL sz, uintEqTy sz)
		  | NONE =>
		      if TU.equalTycon(tyc, BT.boolTycon)   then prim(PU.IEQL,booleqty)
(* FIXME: since real is **not** an equality type, this case is not needed!!! *)
		      else if TU.equalTycon(tyc, BT.realTycon)   then prim(PU.FEQLd,realeqty)
		      else if TU.equalTycon(tyc, BT.stringTycon) then getStrEq()
		      else if TU.equalTycon(tyc, BT.intinfTycon) then getIntInfEq()
		      else if TU.equalTycon(tyc, BT.refTycon)    then ptrEq(PO.PTREQL, ty)
		      else if TU.equalTycon(tyc, BT.pointerTycon) then ptrEq(PO.PTREQL, ty)
		  (**********************
		   * For arrays under the new array representation, we need to compare
		   * the data pointers for equality.  polyequal does this comparison
		   * correctly, so use it as the fallback. (JHR)
		   *
		      else if TU.equalTycon(tyc,BT.arrayTycon) then ptrEq(PO.PTREQL, ty)
		      else if TU.equalTycon(tyc,BT.word8arrayTycon) then ptrEq(PO.PTREQL, ty)
		      else if TU.equalTycon(tyc,BT.real64arrayTycon) then ptrEq(PO.PTREQL, ty)
		    ## also still falling back on polyequal for int64 and word64 -- 64BIT fixme ##
		  **********************)
		      else raise Poly
		(* end case *))

	  fun test (ty, 0) = raise Poly
	    | test (ty, depth) = (
		if !debugging
		  then PP.with_pp (EM.defaultConsumer())
		       (fn ppstrm => (PP.string ppstrm "test: ";
				      PPType.resetPPType();
				      PPType.ppType env ppstrm ty))
		  else ();
		case ty
		 of VARty(ref(INSTANTIATED t)) => test(t,depth)
		  | MARKty(ty, region) => test(ty, depth)
		  | CONty(DEFtyc _, _) => test(TU.reduceType ty,depth)
		  | CONty(RECORDtyc _, tyl) => (
		      find ty
			handle Notfound => let
			  val v = mkv() and x=mkv() and y=mkv()
			  val (eqv, patch) = enter ty
			  fun loop(n, [ty]) =
				APP(test(ty,depth), RECORD[SELECT(n, VAR x),
							   SELECT(n, VAR y)])
			    | loop(n, ty::r) =
				COND(loop(n,[ty]), loop(n+1,r), falseLexp)
			    | loop(_,nil) = trueLexp
			  val lt = toLty ty
			  in
			    patch := FN(v, LD.ltc_tuple [lt,lt],
				       LET(x, SELECT(0, VAR v),
					 LET(y, SELECT(1, VAR v),
					   loop(0, tyl))));
			    eqv
			  end)
		  | CONty (tyc as GENtyc { kind, eq, stamp, arity, path, ... }, tyl) => (
		      case (!eq, kind)
		       of (YES, PRIMITIVE) => atomeq (tyc, ty)
			| (YES, ABSTRACT tyc') => test (CONty (tyc', tyl), depth)
			| (ABS,_) =>
			    test(T.CONty(GENtyc{eq=ref YES,stamp=stamp,arity=arity,
						 kind=kind,path=path,stub=NONE}, tyl),
				 depth)
		      (* assume that an equality datatype has been converted
		       * to an abstract type in an abstype declaration *)
			| (_,DATATYPE{index,family as {members,...}, freetycs,stamps,...}) => let
			    val {dcons=dcons0,...} = Vector.sub(members,index)
			    fun expandRECdcon{domain=SOME x, rep, name} = {
				    domain=SOME(expandREC (family, stamps, freetycs) x),
				    rep=rep,name=name
				  }
			      | expandRECdcon z = z
			    in
			      case map expandRECdcon dcons0
			       of [{rep=REF,...}] => atomeq(tyc, ty)
				| dcons => (
				    find ty
				      handle Notfound => let
					val v = mkv()
					val x=mkv()
					val y=mkv()
					val (eqv, patch) = enter ty
					fun inside ({name,rep,domain}, ww, uu) =
					    case domain of
						NONE => trueLexp
					      | SOME dom =>
						(case reduceTy dom
						  of (CONty(RECORDtyc [], _)) =>
						     trueLexp
						   | _ => let
							 val argt = argType(dom, tyl)
						     in
							 APP(test(argt, depth-1),
							     RECORD[VAR ww, VAR uu])
						     end)
					val lt = toLty ty
					val argty = LD.ltc_tuple [lt,lt]
					val pty = LD.ltc_parrow(argty, boolty)
					val body = (case dcons
					       of [] => bug "empty data types"
(*						| [dcon] => inside dcon       *)
						| _ => let
						    val (sign, ndcons) = getCsig dcons
						    fun concase dcon = let
							  val tcs = map toTyc tyl
							  val ww = mkv()
							  val uu = mkv()
							  val dc =
							      transDcon(tyc,dcon,toTcLc)
							  val dconx = DATAcon(dc, tcs, ww)
							  val dcony = DATAcon(dc, tcs, uu)
							  in
							    (dconx,
							     SWITCH(VAR y, sign,
								    [(dcony,
								      inside(dcon,ww,uu))],
								    SOME(falseLexp)))
							  end
						    in
						      case sign
						       of DA.CSIG(0, _) => falseLexp
							| DA.CSIG(_, 0) =>
							    SWITCH(VAR x, sign,
							      map concase ndcons, NONE)
							| _ =>
							    SWITCH(VAR x, sign,
							      map concase ndcons,
								       SOME falseLexp)
						      (* end case *)
						    end
					      (* end case *))
					 val root = APP(PRIM(PO.PTREQL, pty, []),
							RECORD[VAR x, VAR y])
					 val nbody = COND(root, trueLexp, body)
					 in
					   patch := FN(v, argty,
						       LET(x, SELECT(0, VAR v),
							   LET(y, SELECT(1, VAR v), nbody)));
					   eqv
					 end)
			      (* end case *)
			    end
			| _ => raise Poly
		      (* end case *))
		  | _ => raise Poly
		(* end case *))

	  val body = test(concreteType, 10)
	  val fl = !cache
	  in
	    case fl
	     of [] => body
	      | _ => let fun g ((ty, VAR v, e), (vs, ts, es)) =
				   (v::vs, (eqTy ty)::ts, (!e)::es)
			   | g _ = bug "unexpected equality cache value"

			 val (vs, ts, es) = foldr g ([], [], []) fl
		      in FIX(vs, ts, es, body)
		     end
	    (* end case *)
	  end handle Poly =>
	    (GENOP({default=getPolyEq(),
		    (* might want to include intinf into this table (but we
		     * need a tcc_intinf for that)... *)
		    table=[([LB.tcc_string], getStrEq())]},
		   PO.POLYEQL, toLty polyEqTy,
		   [toTyc concreteType]))

  end (* structure PEqual *)
