(* typesutil.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure TypesUtil : TYPESUTIL =
  struct

    structure EM = ErrorMsg
    structure SS = Substring
    structure EP = EntPath
    structure T = Types
    structure BT = BasicTypes
    structure SP = SymPath
    structure IP = InvPath
    structure S = Symbol
    structure ST = Stamps
    structure A = Access
    structure V = Variable

    open Types

    val array = Array.array
    val sub = Array.sub
    val update = Array.update
    infix 9 sub

    val --> = BT.-->
    infix -->

    val say = Control_Print.say
    val debugging = ElabDataControl.tudebugging
    fun debugmsg msg = if !debugging then say ("TypesUtil: " ^ msg ^ "\n") else ()
    fun bug msg = EM.impossible("TypesUtil: "^msg)

    fun eqpropToString p = (case p
	   of NO => "NO"
	    | YES => "YES"
	    | IND => "IND"
	    | OBJ => "OBJ"
	    | DATA => "DATA"
	    | UNDEF => "UNDEF"
	    | ABS => "ABS"
	  (* end case *))

  (*************** operations to build tyvars, VARtys ***************)

    (* mkMETA : int -> tvKind *)
    fun mkMETA depth = OPEN{kind=META, depth=depth, eq=false}

    (* mkFLEX: (label * ty) list * int -> tvKind *)
    fun mkFLEX (fields, depth) = OPEN{kind=FLEX fields, depth=depth, eq=false}

    (* extract_tyvar_name_eq : string -> string * bool *)
    (* name: string arg is assumed to start with one or two apostrophies
     * ''abc ==> (abc, true);  'abc ==> (abc, false) *)
    fun extract_tyvar_name_eq name =
	let val name = SS.triml 1 (SS.full name)  (* remove leading "'" *)
	    val (name, eq) =
		if SS.sub(name, 0) = #"'"
		then (SS.triml 1 name, true) (* initial "'" signifies equality; removed *)
		else (name, false)
	 in (SS.string name, eq)
	end

    (* mkUBOUND : symbol -> tvkind *)
    (* sym argument is assumed to start with one or two apostrophies *)
    fun mkUBOUND (sym : Symbol.symbol) : tvKind =
	let val (name, eq) = extract_tyvar_name_eq (Symbol.name sym)
	 in UBOUND{name=Symbol.tyvSymbol name, depth=infinity, eq=eq}
	end

(* mkLITERALty moved to ElabCore because of use of OverloadLit *)

  (*
   * mkMETAtyBounded : int -> ty
   * mkMETAty: unit -> ty
   *   mkMETAty returns a type that represents a "fresh" type meta variable,
   * which will NOT appear in the "context" anywhere (e.g. in instantiating
   * the polytype of an applied occurrence of a variable. To do the same
   * thing for a meta variable that will occur in the type of a (lambda-)
   * bound variable in the context, use mkMETAtyBounded with the appropriate
   * lambda-binding depth. *)

    fun mkMETAtyBounded (depth: int) : ty = VARty(mkTyvar (mkMETA depth))

    fun mkMETAty() = mkMETAtyBounded infinity

    (* "marking" tyvars as generalized and identifying generalized tyvars.
     * This is provisional and "abuses" the LBOUND tyvar kind as a shortcut.
     * A "marker" is a LBOUND kind where depth = 0 (irrelevant) and index >= 1000.
     * During plambda translation of an absyn exp containing occurences of
     * generalized tyvars, their kind will be reset to proper LBOUND values
     * containing DeBruijn indexes. *)
    local
      val generalizedTyvarCount = ref 0
    in
      (*  markGeneralizedTyvar: tyvar -> unit *)
      fun markGeneralizedTyvar tv =
	  let val count = !generalizedTyvarCount
	   in generalizedTyvarCount := count + 1;
	      tv := LBOUND{depth = 0, index = 1000 + count, eq = false}
	  end

      (* isGeneralizedTyvar : tyvar -> bool *)
      fun isGeneralizedTyvar tv =
	  case !tv
	    of LBOUND{index, ...} => index >= 1000
	     | _ => false

      (* generalizedTyvarId : tyvar -> int option *)
      fun generalizedTyvarId tv =
	  case !tv
	   of LBOUND{index, ...} =>
	      if index >= 1000 then SOME (index - 1000)
	      else NONE
	    | _ => NONE

      (* resetGeneralizedTyvarCount : unit -> unit *)
      fun resetGeneralizedTyvarCount () =
	  generalizedTyvarCount := 0

    end (* local *)

  (*************** primitive operations on tycons ***************)
    fun bugTyc (s: string, tyc) = (case tyc
	   of GENtyc { path, ... } => bug (s ^ " GENtyc " ^ S.name (IP.last path))
	    | DEFtyc {path,...} => bug (s ^ " DEFtyc " ^ S.name(IP.last path))
	    | RECORDtyc _ => bug (s ^ " RECORDtyc")
	    | PATHtyc{path,...} => bug (s ^ " PATHtyc " ^ S.name(IP.last path))
	    | RECtyc _ => bug (s ^ " RECtyc")
	    | FREEtyc _ => bug (s ^ " FREEtyc")
	    | ERRORtyc => bug (s ^ " ERRORtyc")
	  (* end case *))

  (* short (single symbol) name of tycon *)
    fun tycName (GENtyc { path, ... } | DEFtyc{path,...} | PATHtyc{path,...}) =
	  IP.last path
      | tycName (RECORDtyc _) = S.tycSymbol "<RECORDtyc>"
      | tycName (RECtyc _) = S.tycSymbol "<RECtyc>"
      | tycName (FREEtyc _) = S.tycSymbol "<FREEtyc>"
      | tycName ERRORtyc = S.tycSymbol "<ERRORtyc>"

  (* get the stamp of a tycon *)
    fun tycStamp (GENtyc { stamp, ... } | DEFtyc { stamp, ... }) = stamp
      | tycStamp tycon = bugTyc("tycStamp",tycon)

    (* tycPath : T.tycon -> InvPath.path option
     *  full (inv-)path name of tycon *)
    fun tycPath (GENtyc{path,...} | DEFtyc{path,...} | PATHtyc{path,...}) = SOME path
      | tycPath ERRORtyc = SOME (IP.IPATH[S.tycSymbol "error"])
      | tycPath tycon = NONE

    fun tycEntPath(PATHtyc{entPath,...}) = entPath
      | tycEntPath tycon = bugTyc("tycEntPath",tycon)

    fun tyconArity(GENtyc { arity, ... } | PATHtyc{arity,...}) = arity
      | tyconArity(DEFtyc{tyfun=TYFUN{arity,...},...}) = arity
      | tyconArity(RECORDtyc l) = length l
      | tyconArity(ERRORtyc) = 0
      | tyconArity tycon = bugTyc("tyconArity",tycon)

    fun setTycPath(tycon,path) = (case tycon
	   of GENtyc { stamp, arity, eq, kind, path = _, stub = _ } =>
	      GENtyc { stamp = stamp, arity = arity, eq = eq, kind = kind,
		       path = path, stub = NONE }
	    | DEFtyc{tyfun,strict,stamp,path=_} =>
		DEFtyc{tyfun=tyfun,path=path,strict=strict,stamp=stamp}
	    | _ => bugTyc("setTycName",tycon)
	  (* end case *))

    fun eqRecordLabels(nil,nil) = true
      | eqRecordLabels(x::xs,y::ys) = Symbol.eq(x,y) andalso eqRecordLabels(xs,ys)
      | eqRecordLabels _ = false

    fun eqTycon (GENtyc g, GENtyc g') = Stamps.eq (#stamp g, #stamp g')
      | eqTycon (ERRORtyc,_) = true
      | eqTycon (_,ERRORtyc) = true
      (* this rule for PATHtycs is conservatively correct, but is only an
	 approximation *)
      | eqTycon(PATHtyc{entPath=ep,...},PATHtyc{entPath=ep',...}) =
	  EP.eqEntPath(ep,ep')
      | eqTycon(RECORDtyc l1, RECORDtyc l2) = eqRecordLabels(l1,l2)
      (*
       * This next case used for comparing DEFtyc's, where we can be
       * sure they are equal if they share the same creation stamp,
       * but otherwise we'll assume they may be different.
       * Also used in PPBasics to check data constructors of
       * a datatype.  Used elsewhere?
       *)
      | eqTycon(DEFtyc{stamp=s1,...},DEFtyc{stamp=s2,...}) =
	  Stamps.eq(s1,s2)
      | eqTycon _ = false

    (* eqDatacon : datacon * datacon -> bool
     * equality of datacons belonging to the same datatype, based on
     * the fact that no two datacons of a datatype have the same rep value,
     * and conrep is an equality type. *)
    fun eqDatacon(DATACON{rep=a1,...}: datacon, DATACON{rep=a2,...}: datacon) =
        (a1 = a2)

  (* prune: ty -> ty; eliminates outermost INSTANTIATED indirections *)
    fun prune (MARKty(ty, _)) = prune ty
      | prune (VARty(tv as ref(INSTANTIATED ty))) = let
	  val pruned = prune ty
	  in
	    tv := INSTANTIATED pruned; pruned
	  end
      | prune ty = ty

    fun pruneTyvar (tv as ref(INSTANTIATED ty)) : ty = let
	  val pruned = prune ty
	  in
	    tv := INSTANTIATED pruned; pruned
	  end
      | pruneTyvar _ = bug "pruneTyvar: not an instantiated tyvar"

    fun eqTyvar (tv1: tyvar, tv2: tyvar) = (tv1 = tv2)

    fun bindTyvars (tyvars: tyvar list) : unit =
	let fun loop([],_) = ()
	      | loop(tv::rest,n) =
		  (tv := INSTANTIATED (IBOUND n);
		   loop(rest,n+1))
	 in loop(tyvars,0)
	end

    fun bindTyvars1 (tyvars: tyvar list) : Types.polysign =
	let fun loop([],_) = []
	      | loop((tv as ref(UBOUND{eq,...}))::rest,n) =
		   (tv := INSTANTIATED (IBOUND n);
		    eq :: loop(rest,n+1))
	      | loop _ = bug "bindTyvars1:UBOUND"
	 in loop(tyvars,0)
	end

    fun tyvarIsEq tyvar =
	case !tyvar
	 of (OPEN{eq,...} | UBOUND{eq,...} | OVLDV{eq,...}
	     | LBOUND{eq,...}) => eq
	  | (OVLDI _ | OVLDW _) => true
	  | _ => false

    exception SHARE

  (* assume that f fails on identity, i.e. f x raises SHARE instead of
     returning x *)
    fun shareMap f nil = raise SHARE
      | shareMap f (x::l) =
	  (f x) :: ((shareMap f l) handle SHARE => l)
	  handle SHARE => x :: (shareMap f l)

  (* applyTyfun is more general than instantiatePoly and has
     many uses beyond applyPoly *)
    fun applyTyfun(TYFUN{arity,body}, args: ty list) =
      let fun subst(IBOUND n) = List.nth(args,n)
	    | subst(CONty(tyc,args)) = CONty(tyc, shareMap subst args)
	    | subst(VARty(ref(INSTANTIATED ty))) = subst ty
	    | subst(MARKty(ty,_)) = subst ty
	    | subst _ = raise SHARE
       in if arity <> length args
	    then (say ("$$$ applyTyfun: arity = "^(Int.toString arity)^
		       ", |args| = "^(Int.toString(length args))^"\n");
		  bug "applyTyfun: arity mismatch")
	  else if arity > 0
	    then subst body
		 handle SHARE => body
		      | Subscript => bug "applyTyfun - not enough arguments"
	  else body
      end

    fun applyPoly(POLYty{tyfun,...}, args) =
	  applyTyfun(tyfun, args)  (* applyTyfun checks args, arity agreement *)
      | applyPoly (ty, nil) = ty
      | applyPoly _ = bug "applyPoly -- non-polytype with args"

    fun mapTypeFull f =
	let fun mapTy ty =
		case ty
		  of CONty (tc, tl) =>
		      CONty(f tc, map mapTy tl)
		   | POLYty {sign, tyfun=TYFUN{arity, body}} =>
		      POLYty{sign=sign, tyfun=TYFUN{arity=arity,body=mapTy body}}
		   | VARty(ref(INSTANTIATED ty)) => mapTy ty
		   | MARKty(ty, region) => mapTy ty
		   | _ => ty
	 in mapTy
	end

    fun appTypeFull f =
	let fun appTy ty =
		case ty
		  of CONty (tc, tl) => (f tc;  app appTy tl)
		   | POLYty {sign, tyfun=TYFUN{arity, body}} => appTy body
		   | VARty(ref(INSTANTIATED ty)) => appTy ty
		   | MARKty(ty, region) => appTy ty
		   | _ => ()
	 in appTy
	end

    exception ReduceType

    fun reduceType(CONty(DEFtyc{tyfun,...}, args)) = applyTyfun(tyfun,args)
      | reduceType(POLYty{sign=[],tyfun=TYFUN{arity=0,body}}) = body
      | reduceType(VARty(ref(INSTANTIATED ty))) = ty
      | reduceType(MARKty(ty, region)) = ty
      | reduceType _ = raise ReduceType

    (* headReduceType : ty -> ty *)
    fun headReduceType ty = headReduceType(reduceType ty) handle ReduceType => ty

    fun equalType(ty: ty,ty': ty) : bool =
	let fun eq(IBOUND i1, IBOUND i2) = i1 = i2
	      | eq(ty1 as VARty(tv1), ty2 as VARty(tv2)) =
		eqTyvar(tv1,tv2) orelse
		(case (tv1,tv2)
		  of (ref(INSTANTIATED ty1'), ref(INSTANTIATED ty2')) =>
		      equalType(ty1', ty2')
		   | (ref(INSTANTIATED ty1'), _) =>
		      equalType(ty1',ty2)
		   | (_, ref(INSTANTIATED ty2')) =>
		      equalType(ty1,ty2')
		   | _ => false)
	      | eq(ty as CONty(tycon, args), ty' as CONty(tycon', args')) =
		  if eqTycon(tycon, tycon') then
		     (case tycon
			of DEFtyc{strict,...} =>
			   (* since tycons are equal, both are DEFtycs with
			    * the same arity and strict field values *)
			   let fun eqargs([],[],[]) = true
				 | eqargs(true::ss,ty1::rest1,ty2::rest2) =
				     equalType(ty1,ty2) andalso eqargs(ss,rest1,rest2)
				 | eqargs(false::ss,ty1::rest1,ty2::rest2) =
				     eqargs(ss,rest1,rest2)
				 | eqargs _ = bug "eqargs in equalType [TypesUtil]"
			    in eqargs(strict,args,args')
			   end
			 | _ => ListPair.all equalType(args,args'))
		  else (eq(reduceType ty, ty')
			handle ReduceType =>
			  (eq(ty,reduceType ty')
			    handle ReduceType => false))
	      | eq(ty1 as (VARty _ | IBOUND _), ty2 as CONty _) =
		  (eq(prune ty1,reduceType ty2)
		   handle ReduceType => false)
	      | eq(ty1 as CONty _, ty2 as (VARty _ | IBOUND _)) =
		  (eq(reduceType ty1, prune ty2)
		   handle ReduceType => false)
	      | eq(WILDCARDty,_) = true
	      | eq(_,WILDCARDty) = true
	      | eq(ty1, MARKty(ty, region)) = eq(ty1, ty)
	      | eq(MARKty(ty, region), ty2) = eq(ty, ty2)
	      | eq _ = false
	 in eq(prune ty, prune ty')
	end

    fun equalTypeP(POLYty{sign=s1,tyfun=TYFUN{body=b1,...}},
		   POLYty{sign=s2,tyfun=TYFUN{body=b2,...}}) =
	if s1 = s2 then equalType(b1,b2) else false
      | equalTypeP(POLYty _, t2) = false
      | equalTypeP(t1, POLYty _) = false
      | equalTypeP(t1,t2) = equalType(t1,t2)

    local
      (* making dummy argument lists to be used in equalTycon *)
	val generator = Stamps.newGenerator()
	fun makeDummyType() =
	    CONty(GENtyc{stamp = Stamps.fresh generator,
			 path = IP.IPATH[Symbol.tycSymbol "dummy"],
			 arity = 0, eq = ref YES, stub = NONE,
			 kind = PRIMITIVE},[])
	     (*
	      * Making dummy type is a temporary hack ! pt_void is not used
	      * anywhere in the source language ... Requires major clean up
	      * in the future. (ZHONG)
	      * DBM: shouldn't cause any problem here.  Only thing relevant
	      * property of the dummy types is that they have different stamps
	      * and their stamps should not agree with those of any "real" tycons.
	      *)
	(* precomputing dummy argument lists
	 * -- perhaps a bit of over-optimization here. [dbm] *)
	fun makeargs (0,args) = args
	  | makeargs (i,args) = makeargs(i-1, makeDummyType()::args)
	val args10 = makeargs(10,[])  (* 10 dummys *)
	val args1 = [hd args10]
	val args2 = List.take (args10,2)
	val args3 = List.take (args10,3)  (* rarely need more than 3 args *)
    in
      fun dummyargs 0 = []
	| dummyargs 1 = args1
	| dummyargs 2 = args2
	| dummyargs 3 = args3
	| dummyargs n =
	    if n <= 10 then List.take (args10,n) (* should be plenty *)
	    else makeargs(n-10,args10)  (* but make new dummys if needed *)
    end (* local *)

  (* equalTycon.  This definition deals only partially with types that
     contain PATHtycs.  There is no interpretation of the PATHtycs, but
     PATHtycs with the same entPath will be seen as equal because of the
     definition on eqTycon. *)
    fun equalTycon(ERRORtyc,_) = true
      | equalTycon(_,ERRORtyc) = true
      | equalTycon(t1,t2) =
	 let val a1 = tyconArity t1 and a2 = tyconArity t2
	  in a1=a2 andalso
	     (let val args = dummyargs a1
	      in equalType(CONty(t1,args),CONty(t2,args))
	      end)
	 end

    (* calcStrictness : int * Types.ty -> bool list *)
    (* Returns a list of bools of length arity, where the ith element indicates
     * whether DB index (IBOUND i) occurs in the type "body". *)
    fun calcStrictness (arity, body) =
	let val argument_found = Array.array(arity,false)
	    fun search (VARty(ref(tvkind))) =
		  (case tvkind
		     of INSTANTIATED ty => search ty
		      | _ => ())
	      | search (IBOUND n) = Array.update(argument_found,n,true)
	      | search (ty as CONty(tycon, args)) =
		  (case tycon
		     of DEFtyc _ => search(headReduceType ty)
		      | _ => app search args)
	      | search (MARKty(ty,_)) = search ty
	      | search (POLYty _) = bug "calcStrictness: POLYty"
	      | search WILDCARDty = bug "calcStrictness: WILDCARDty"
	      | search UNDEFty = bug "calcStrictness: UNDEFty"
	 in search body;
	    Array.foldr (op ::) nil argument_found
	end

  (* instantiating polytypes *)

    fun typeArgs n =
	if n>0
	then mkMETAty() :: typeArgs(n-1)
	else []

    val default_tvprop = false

    fun mkPolySign 0 = []
      | mkPolySign n = default_tvprop :: mkPolySign(n-1)

    fun dataconTyc(DATACON{typ,const,name,...}) =
	let fun f (POLYty{tyfun=TYFUN{body,...},...},b) = f (body,b)
	      | f (CONty(tyc,_),true) = tyc
	      | f (CONty(_,[_,CONty(tyc,_)]),false) = tyc
	      | f (MARKty(ty, region), b) = f(ty, b)
	      | f _ = bug "dataconTyc"
	 in f (typ,const)
	end

    fun boundargs n =
	let fun loop(i) =
	    if i>=n then nil
	    else IBOUND i :: loop(i+1)
	 in loop 0
	end

    fun dconType (tyc,domain) =
	(case tyconArity tyc
	  of 0 => (case domain
		     of NONE => CONty(tyc,[])
		      | SOME dom => dom --> CONty(tyc,[]))
	   | arity =>
	     POLYty{sign=mkPolySign arity,
		    tyfun=TYFUN{arity=arity,
				body = case domain
					 of NONE => CONty(tyc,boundargs(arity))
					  | SOME dom =>
					    dom --> CONty(tyc,boundargs(arity))}})

    (* compressTy : ty -> unit
     * uses of compressTy can probably be replaced by calls of mapTy (fn x => x)
     *)
    fun compressTy (t as VARty(x as ref(INSTANTIATED(VARty(ref v))))) =
	    (x := v; compressTy t)
      | compressTy (VARty(ref(OPEN{kind=FLEX fields,...}))) =
	    app (compressTy o #2) fields
      | compressTy (CONty(tyc,tyl)) = app compressTy tyl
      | compressTy (POLYty{tyfun=TYFUN{body,...},...}) = compressTy body
      | compressTy _ = ()

  (* instantiatePoly: ty -> ty * tyvar list
     if argument is a POLYty, instantiates body of POLYty with new META typa
     variables, returning the instantiatied body and the list of META tyvars.
     if argument is not a POLYty, does nothing, returning argument type *)
    fun instantiatePoly(POLYty{sign,tyfun}) : ty * tyvar list =
	  let val args =  (* fresh OPEN metavariables *)
		  map (fn eq =>
			  ref(OPEN{kind = META, depth = infinity, eq = eq}))
		      sign
	   in (applyTyfun(tyfun, map VARty args), args)
	  end
      | instantiatePoly ty = (ty,[])

    local
      exception CHECKEQ
    in
    fun checkEqTySig(ty, sign: polysign) =
	let fun eqty(VARty(ref(INSTANTIATED ty))) = eqty ty
	      | eqty(CONty(DEFtyc{tyfun,...}, args)) =
		  eqty(applyTyfun(tyfun,args))
	      | eqty(CONty(GENtyc { eq, ... }, args)) =
		 (case !eq
		    of OBJ => ()
		     | YES => app eqty args
		     | (NO | ABS | IND) => raise CHECKEQ
		     | p => bug ("checkEqTySig: "^eqpropToString p))
	      | eqty(CONty(RECORDtyc _, args)) = app eqty args
	      | eqty(IBOUND n) = if List.nth(sign,n) then () else raise CHECKEQ
	      | eqty _ = ()
	 in eqty ty;
	    true
	end
	handle CHECKEQ => false

    fun checkEqTyInst(ty) =
	let fun eqty(VARty(ref(INSTANTIATED ty))) = eqty ty
	      | eqty(VARty(ref(OPEN{eq,...}))) = if eq then () else raise CHECKEQ
	      | eqty(VARty(ref(LBOUND{eq,...}))) = if eq then () else raise CHECKEQ
	      | eqty(CONty(DEFtyc{tyfun,...}, args)) =
		  eqty(applyTyfun(tyfun,args))
	      | eqty(CONty(GENtyc { eq, ... }, args)) =
		 (case !eq
		    of OBJ => ()
		     | YES => app eqty args
		     | (NO | ABS | IND) => raise CHECKEQ
		     | p => bug ("checkEqTyInst: "^eqpropToString p))
	      | eqty(CONty(RECORDtyc _, args)) = app eqty args
	      | eqty(IBOUND n) = bug "checkEqTyInst: IBOUND in instantiated polytype"
	      | eqty _ = () (* what other cases? dbm *)
	 in eqty ty;
	    true
	end
	handle CHECKEQ => false
    end

  (* compType, compareTypes used to compare specification type with type of
   * corresponding actual element.  Check that spec type is an instance of
   * the actual type *)
    exception CompareTypes
    fun compType(specty, specsign:polysign, actty,
		 actsign:polysign, actarity): unit =
	let val env = array(actarity,UNDEFty) (* instantiations of IBOUNDs in actual body *)
	    fun comp'(WILDCARDty, _) = ()
	      | comp'(_, WILDCARDty) = ()
	      | comp'(ty1, IBOUND i) =
		 (case env sub i
		    of UNDEFty =>
			(let val eq = List.nth(actsign,i)
			  in if eq andalso not(checkEqTySig(ty1,specsign))
			     then raise CompareTypes
			     else update(env,i,ty1)
			 end handle Subscript => ())
		     | ty => if equalType(ty1,ty)
			     then ()
			     else raise CompareTypes)
	      | comp'(CONty(tycon1, args1), CONty(tycon2, args2)) =
		  if eqTycon(tycon1,tycon2)
		  then ListPair.app comp (args1,args2)
		  else raise CompareTypes
	      | comp' _ = raise CompareTypes
	    and comp(ty1,ty2) = comp'(headReduceType ty1, headReduceType ty2)
	 in comp(specty,actty)
	end

  (* returns true if actual type > spec type, i.e. if spec is an instance of actual *)
    fun compareTypes (spec : ty, actual: ty): bool =
	let val actual = prune actual
	 in case spec
	      of POLYty{sign,tyfun=TYFUN{body,...}} =>
		  (case actual
		     of POLYty{sign=sign',tyfun=TYFUN{arity,body=body'}} =>
			  (compType(body,sign,body',sign',arity); true)
		      | WILDCARDty => true
		      | _ => false) (* if spec is poly, then actual must be poly *)
	       | WILDCARDty => true
	       | _ => (* spec is a monotype *)
		  (case actual
		     of POLYty{sign,tyfun=TYFUN{arity,body}} =>
			  (compType(spec,[],body,sign,arity); true)
		      | WILDCARDty => true
		      | _ => equalType(spec,actual))
	end handle CompareTypes => false

    exception WILDCARDmatch

    fun indexBoundTyvars (tdepth : int, []: tyvar list) : unit = ()
      | indexBoundTyvars (tdepth, lboundtvs) =
	let fun setbtvs (i, []) = ()
	      | setbtvs (i, (tv as ref (OPEN{eq,...}))::rest) =
		 (tv := LBOUND{depth=tdepth,eq=eq,index=i};
		  setbtvs (i+1, rest))
	      | setbtvs (i, (tv as ref (LBOUND _))::res) =
		 bug ("unexpected tyvar LBOUND in indexBoundTyvars")
	      | setbtvs _ = bug "unexpected tyvar INSTANTIATED in mkPE"
	 in setbtvs(0, lboundtvs)
	end

  (* matchInstTypes: bool * ty * ty -> (tyvar list * tyvar list) option
   * The first argument tells matchInstTypes to ignore the abstract property
   * of abstract types, i.e., this call is being used in FLINT where
   * we can look into abstract types.
   * The third argument is a spec type (e.g. from a signature spec),
   * while the fourth is a potentially more general actual type. The
   * two types are instantiated (if they are polymorphic), and a one-way
   * match is performed on their generic instantiations.
   * [Note that the match cannot succeed if spec is polymorphic while
   * actualTy is monomorphic.]
   * This function is also used more generally to obtain instantiation
   * parameters for a polytype (actualTy) to match one of its instantiations
   * (specTy). This usage occurs in translate.sml where we match an occurrence
   * type of a primop variable with the intrinsic type of the primop to obtain
   * the instantiation parameters for the primop relative to its intrinsic type.
   *)
    fun matchInstTypes(doExpandAbstract,tdepth,specTy,actualTy) =
	let fun debugmsg' msg = debugmsg ("matchInstTypes: " ^ msg)
	    fun expandAbstract(GENtyc {kind=ABSTRACT tyc', ...}) =
		expandAbstract tyc'
	      | expandAbstract(tyc) = tyc
	    fun match'(WILDCARDty, _) = () (* possible? how?
					     [GK 4/20/07] See bug1179
					     We do matches against WILDCARDs
					     when signature matching fails to
					     match a type spec and yet we have
					     to match valspec and vals mentioning
					     that missing type. *)
	      | match'(_, WILDCARDty) = ()
	      | match'(MARKty (t, _), t') = match'(t, t')
	      | match'(t, MARKty (t', _)) = match'(t, t')
	      | match'(ty1, ty2 as VARty(tv as ref(OPEN{kind=META,eq,...}))) =
		  (* If we're told to ignore abstract, then we can't
		     check for equality types because the original GENtyc
		     was lost by setting the type to abstract imperatively.
		     Thus, if doExpandAbstract, we skip the equality type
		     check. At this point, the elaborator already checked
		     for equality types (before they were side-effected),
		     hence it is guaranteed that if one is an equality type
		     so is the other. The regression test suite coresml
		     d005a-ac.sml tests this. [GK 4/11/07]
		    *)
		  if not(doExpandAbstract) andalso
		     (eq andalso not(checkEqTyInst(ty1)))
		  then (debugmsg' "VARty META\n"; raise CompareTypes)
		  else if equalType(ty1, ty2)
		  then ()
		  else tv := INSTANTIATED ty1
	      | match'(ty1, VARty(tv as ref(INSTANTIATED ty2))) =
		  if equalType(ty1,ty2) then ()
		  else (debugmsg' "INSTANTIATED"; raise CompareTypes)
	      (* GK: Does this make sense? matchInstTypes should not apply
		     as is if all the metavariables have been translated
		     into LBOUNDs *)
	      | match'(ty1, ty2 as VARty(tv' as (ref(LBOUND _)))) =
		  if equalType(ty1,ty2) then ()
		  else (debugmsg' "matchInstTypes: matching and LBOUND tyvar";
			raise CompareTypes)
	      | match'(CONty(tycon1, args1), CONty(tycon2, args2)) =
		  if eqTycon(tycon1,tycon2)
		  then ListPair.app match (args1,args2)
		  else
		      (* Example:
		       *)
		      if doExpandAbstract (* Expand GENtyc ABSTRACT for translate *)
		      then
			  let val tyc1 = expandAbstract tycon1
			      val tyc2 = expandAbstract tycon2
			  in if not (eqTycon(tyc1,tycon1)
				     andalso eqTycon(tyc2,tycon2))
			     then match(CONty(tyc1, args1),
					CONty(tyc2, args2))
			     else raise CompareTypes
			  end
		  else (debugmsg' "CONty"; raise CompareTypes)
	      | match'(_, UNDEFty) = (debugmsg' "UNDEFty"; raise CompareTypes)
	      | match'(_, IBOUND _) = (debugmsg' "IBOUND"; raise CompareTypes)
	      | match'(_, POLYty _) = (debugmsg' "POLYty"; raise CompareTypes)
	      | match'(_, CONty _) = (debugmsg' "unmatched CONty"; raise CompareTypes)
	      | match'(t1, VARty vk) = (debugmsg' "VARty other";
					raise CompareTypes)
	    and match(ty1,ty2) = match'(headReduceType ty1, headReduceType ty2)
	    val (actinst, actParamTvs) = instantiatePoly actualTy
	    val (specinst, specGenericTvs) = instantiatePoly specTy
	    val _ = indexBoundTyvars(tdepth,specGenericTvs)
	    val _ = debugmsg' "Instantiated both\n"
	in match(specinst, actinst);
	   debugmsg' "matched\n";
	   SOME(specGenericTvs, actParamTvs)
	end handle CompareTypes => NONE

  (* given a single-type-variable type, extract out the tyvar *)
    fun tyvarType (VARty (tv as ref(OPEN _))) = tv
      | tyvarType (VARty (tv as ref(INSTANTIATED t))) = tyvarType t
      | tyvarType WILDCARDty = ref(mkMETA infinity)  (* fake a tyvar *)
      | tyvarType (IBOUND i) = bug "tyvarType: IBOUND"
      | tyvarType (CONty(_,_)) = bug "tyvarType: CONty"
      | tyvarType (POLYty _) = bug "tyvarType: POLYty"
      | tyvarType UNDEFty = bug "tyvarType: UNDEFty"
      | tyvarType _ = bug "tyvarType - unexpected argument"

  (*
   * getRecTyvarMap : int * ty -> (int -> bool)
   * see if a bound tyvar has occurred in some datatypes, e.g. 'a list.
   * this is useful for representation analysis. This function probably
   * will soon be obsolete (dbm: Why?).
   *)
    fun getRecTyvarMap (n,ty) =
	let val s = Array.array(n,false)
	    fun notArrow tyc = not (eqTycon (tyc, BT.arrowTycon))
			      (* orelse eqTycon(tyc,contTycon) *)
	    fun special (tyc as GENtyc { arity, ... }) =
		arity <> 0 andalso notArrow tyc
	      | special(RECORDtyc _) = false
	      | special tyc = notArrow tyc

	    fun scan(b,(IBOUND n)) = if b then (update(s,n,true)) else ()
	      | scan(b,CONty(tyc,args)) =
		 let val nb = (special tyc) orelse b
		  in app (fn t => scan(nb,t)) args
		 end
	      | scan(b,VARty(ref(INSTANTIATED ty))) = scan(b,ty)
	      | scan _ = ()

	    val _ = scan(false,ty)

	 in fn i => (Array.sub(s,i) handle General.Subscript =>
		       bug "Strange things in TypesUtil.getRecTyvarMap")
	end

    fun gtLabel(a,b) =
	let val a' = Symbol.name a and b' = Symbol.name b
	    val a0 = String.sub(a',0) and b0 = String.sub(b',0)
	 in if Char.isDigit a0
	      then Char.isDigit b0
		andalso (size a' > size b' orelse size a' = size b' andalso a' > b')
	      else Char.isDigit b0 orelse (a' > b')
	end

  (* Tests used to implement the value restriction *)
  (* Based on Ken Cline's version; allows refutable patterns *)
  (* Modified to support CAST, and special binding CASEexp. (ZHONG) *)
  (* Modified to allow applications of lazy val rec Y combinators to
     be nonexpansive. (Taha, DBM) *)

    local open Absyn in

    (* orAlternatives : A.pat -> A.pat list *)
    (* DBM: assumes "|" operator in patterns is right associative
     * this function should only be applied to ??? *)
    fun orAlternatives (ORpat(p1,p2)) =
	p1 :: orAlternatives p2
      | orAlternatives p = [p]

    (* dconRefutable : dcon -> bool
     * a dcon is irrefutable if its datatype has only one data constructor *)
    fun dconRefutable(DATACON{sign,...}) =
	case sign
	 of A.CSIG(n,m) => (n+m) > 1 (* ref, etc. dcons considered irrefutable *)
	  | A.CNIL => true (* exn constructors have sign = CNIL and are refutable *)

    (* refutable: A.pat -> bool
     * a pattern is refutable if there exists a value of its type that does
     * not match it, i.e. the "coverage" of the pattern is incomplete *)
    fun refutable pat =
	case pat
	 of VARpat _ => false
	  | WILDpat => false
	  | CONpat (dcon, tyvars) => dconRefutable dcon
	  | APPpat (dcon, _, arg) =>
	      dconRefutable dcon orelse refutable arg
	  | RECORDpat {fields, ...} =>
	      List.exists (fn (_,p) => refutable p) fields
	  | VECTORpat (pats, _) =>
	      List.exists refutable pats
	  | LAYEREDpat (p1, p2) => refutable p2
	  | CONSTRAINTpat (p, _ ) => refutable p
	  | MARKpat (p, _) => refutable p
	  | ORpat (p1, p2) => true (* punt, conservatively *)
	  | _ => true  (* NOPAT, numbers, strings, characters are refutable *)

    (* We don't (yet?) cope with OR patterns. One expects that
     * p1 at least is refutable, else the ORpat is degenerate.
     * together, p1 and/or p2 may be refutable, but ORpat(p1,p2) may
     * not be; e.g. ORpat(true,false). This test is not straitforward! *)

    (* refutablePats : A.pat list -> bool
     * test whether a list of alternative pats is refutable as a whole for their
     * common type, i.e. are there values that don't match any of the pats? *)
    (* BUG: punting! -- temporarily assuming all OR patterns refutable. This is
     * not true -- consider "(true | false)". Could invoke matchComp on a list
     * of dummy rules to see if it produces a SWITCHexp with SOME default. *)
    and refutablePats pats = true

    fun isValue (VARexp _) = true
      | isValue (CONexp _) = true
      | isValue (NUMexp _) = true
      | isValue (REALexp _) = true
      | isValue (STRINGexp _) = true
      | isValue (CHARexp _) = true
      | isValue (FNexp _) = true
      | isValue (RECORDexp fields) =
	foldr (fn ((_,exp),x) => x andalso (isValue exp)) true fields
      | isValue (RSELECTexp(var,index)) = true (* should not occur at this point *)
      | isValue (VSELECTexp(var,_,index)) = true (* should not occur at this point *)
      | isValue (VECTORexp (exps, _)) =
	foldr (fn (exp,x) => x andalso (isValue exp)) true exps
      | isValue (SEQexp nil) = true
      | isValue (SEQexp [e]) = isValue e
      | isValue (SEQexp _) = false
      | isValue (APPexp(rator, rand)) =
	let fun isrefdcon(DATACON{rep=A.REF,...}) = true
	      | isrefdcon _ = false
	    fun iscast (V.VALvar {prim, ...}) = PrimopId.isPrimCast prim
	      | iscast _ = false

	    (* LAZY: The following function allows applications of the
	     * fixed-point combinators generated for lazy val recs to
	     * be non-expansive. *)
	    fun issafe(V.VALvar{path=(SymPath.SPATH [s]),...}) =
		(case String.explode (Symbol.name s)
		  of (#"Y" :: #"$" :: _) => true
		   | _ => false)
	      | issafe _ = false

	    fun iscon (CONexp(dcon,_)) = not (isrefdcon dcon)
	      | iscon (MARKexp(e,_)) = iscon e
	      | iscon (VARexp(ref v, _)) = (iscast v) orelse (issafe v)
	      | iscon _ = false
	in if iscon rator then isValue rand
	   else false
	end
      | isValue (CONSTRAINTexp(e,_)) = isValue e
      | isValue (CASEexp(e, (RULE(p,e')::_, _, _))) =  (* case for binding. OBS. FIX! DBM *)
	isValue e andalso not(refutable p) andalso isValue e'
        (* DBM: at the point where isValue is used in typecheck.sml,
         * have "val pat = e" declarations been rewritten as
	 * "val bvars = case e of pat => bvars | _ => raise Bind"?
	 * I don't think so. This happens in FLINT/trans/translate
	 * (and/or the match compiler?). In the general case, this is wrong, because
	 * the rhs rule expression e' associated with p may not be a value.
	 * a more general treatment of case expressions would allow the case
         * where the set of all rule patterns is irrefutable and all the rhs
         * expressions are values. DBM [7/17/20] reconsider in light of new MC. *)
      | isValue (LETexp(VALRECdec _, e)) = (isValue e) (* special RVB hacks *)
      | isValue (MARKexp(e,_)) = isValue e
      | isValue _ = false

    end (* local *)

    fun isVarTy(VARty(ref(INSTANTIATED ty))) = isVarTy ty
      | isVarTy(VARty _) = true
      | isVarTy(_) = false

  (* sortFields, mapUnZip: two utility functions used in type checking
     (typecheck.sml, mtderiv.sml, reconstruct.sml) *)

    fun sortFields fields =
	ListMergeSort.sort
	    (fn ((Absyn.LABEL{number=n1,...},_),
		 (Absyn.LABEL{number=n2,...},_)) => n1>n2)
	    fields

  (* projectField : symbol * ty -> ty option *)
    fun projectField (label: S.symbol, CONty(RECORDtyc fieldNames, fieldTypes)) =
	let fun search (nil, _) = NONE
	      | search (n::ns, t::ts) =
		if Symbol.eq (label,n) then SOME t
		else search(ns,ts)
	      | search _ = bug "projectField - bad record type"
	 in search (fieldNames, fieldTypes)
	end
      | projectField _ = bug "projectField - not record type"

  (* mapUnZip : ('a -> 'b * 'c) -> 'a list -> 'b list * 'c list *)
  (* mapUnZip f xs = ListPair.unzip(map f xs), with loop fusion, not tail recursive *)
    fun mapUnZip f =
        let fun muz nil = (nil,nil)
	      | muz (x::xs) =
		let val (u,v) = f x
		    val (us,vs) = muz xs
		 in (u::us, v::vs)
		end
	in muz
	end

    fun foldTypeEntire f =
	let fun foldTc (tyc, b0) =
	      case tyc
	       of GENtyc { kind, ... } =>
		  (case kind
		     of DATATYPE{family={members=ms,...},...} => b0
		      | ABSTRACT tc => foldTc (tc, b0)
		      | _ => b0)
		| DEFtyc{tyfun=TYFUN{arity,body}, ...} => foldTy(body, b0)
		| _ => b0

	    and foldDcons({name, rep, domain=NONE}, b0) = b0
	      | foldDcons({domain=SOME ty, ...}, b0) = foldTy(ty, b0)

	    and foldTy (ty, b0) =
	      case ty
	       of CONty (tc, tl) =>
		    let val b1 = f(tc, b0)
			val b2 = foldTc(tc, b1)
		     in foldl foldTy b2 tl
		    end
		| POLYty {sign, tyfun=TYFUN{arity, body}} => foldTy(body, b0)
		| VARty(ref(INSTANTIATED ty)) => foldTy(ty, b0)
		| _ => b0
	 in foldTy
	end

    fun mapTypeEntire f =
	let fun mapTy ty =
	      case ty
	       of CONty (tc, tl) =>
		    CONty(f(mapTc, tc), map mapTy tl)
		| POLYty {sign, tyfun=TYFUN{arity, body}} =>
		    POLYty{sign=sign, tyfun=TYFUN{arity=arity,body=mapTy body}}
		| VARty(ref(INSTANTIATED ty)) => mapTy ty
		| _ => ty

	    and mapTc tyc =
	      case tyc
	       of GENtyc { stamp, arity, eq, path, kind, stub = _ } =>
		  (case kind of
		       DATATYPE{index,family={members,...},...} => tyc
(*
 *  The following code needs to be rewritten !!! (ZHONG)

		   GENtyc{stamp=stamp, arity=arity, eq=eq, path=path,
			   kind=DATATYPE {index=index, members=map mapMb members,
					  lambdatyc = ref NONE}}
*)
		     | ABSTRACT tc =>
		       GENtyc {stamp=stamp, arity=arity, eq=eq, path=path,
			       kind= ABSTRACT (mapTc tc),
			       stub = NONE}
		     | _ => tyc)
		| DEFtyc{stamp, strict, tyfun, path} =>
		  DEFtyc{stamp=stamp, strict=strict, tyfun=mapTf tyfun,
			 path=path}
		| _ => tyc

	     and mapMb {tycname, stamp, arity, dcons, lambdatyc} =
		  {tycname=tycname, stamp=stamp, arity=arity,
		   dcons=(map mapDcons dcons), lambdatyc=ref NONE}

	     and mapDcons (x as {name, rep, domain=NONE}) = x
	       | mapDcons (x as {name, rep, domain=SOME ty}) =
		  {name=name, rep=rep, domain=SOME(mapTy ty)}

	     and mapTf (TYFUN{arity, body}) =
		  TYFUN{arity=arity, body=mapTy body}

	 in mapTy
	end


  (*
   * Here, using a set implementation should suffice, however,
   * I am using a binary dictionary instead. (ZHONG)
   *)
    local
      structure TycSet = StampMap
    in
      type tycset = tycon TycSet.map

      val mkTycSet = fn () => TycSet.empty

      fun addTycSet (tyc as GENtyc { stamp, ... }, tycset) =
	    TycSet.insert (tycset, stamp, tyc)
	| addTycSet _ = bug "unexpected tycons in addTycSet"

      fun inTycSet (tyc as GENtyc { stamp, ... }, tycset) =
	    isSome (TycSet.find(tycset, stamp))
	| inTycSet _ = false

      fun filterSet (ty, tycs) =
	let fun inList (a::r, tc) = eqTycon(a, tc) orelse inList(r, tc)
	      | inList ([], tc) = false

	    fun pass1 (tc, tset) =
	      if inTycSet(tc, tycs) then
		  (if inList(tset, tc) then tset else tc::tset)
	      else tset
	 in foldTypeEntire pass1 (ty, [])
	end
(*
      val filterSet = fn x =>
	Stats.doPhase(Stats.makePhase "Compiler 034 filterSet") filterSet x
*)
    end (* local TycSet *)

    fun dtSibling (n,tyc as GENtyc { kind = DATATYPE dt, ... }) =
	let val {index,stamps,freetycs,root, family as {members,...},stripped} = dt
	in
	    if n = index then tyc
	    else let val {tycname,arity,dcons,eq,lazyp,sign} =
			 Vector.sub(members,n)
		     val stamp= Vector.sub(stamps,n)
		 in
		     GENtyc {stamp=stamp,
			     arity=arity,eq=eq,path=IP.IPATH[tycname],
			     kind=DATATYPE{index=n,stamps=stamps,
					   freetycs=freetycs,
					   root=NONE (*!*),
					   stripped=false,
					   family=family},
			     stub = NONE}
		 end
	end
      | dtSibling _ = bug "dtSibling"

  (* NOTE: this only works (perhaps) for datatype declarations, but not
     specifications. The reason: the root field is used to connect mutually
     recursive datatype specifications together, its information cannot be
     fully recovered in dtSibling. (ZHONG)
   *)
    fun extractDcons (tyc as GENtyc { kind = DATATYPE dt, ... }) =
	let val {index,freetycs,family as {members,...},...} = dt
	    val {dcons,sign,lazyp,...} = Vector.sub(members,index)
	    fun expandTyc(PATHtyc _) =
		bug "expandTyc:PATHtyc" (* use expandTycon? *)
	      | expandTyc(RECtyc n) = dtSibling(n,tyc)
	      | expandTyc(FREEtyc n) =
		((List.nth(freetycs,n))
		 handle _ => bug "unexpected freetycs in extractDcons")
	      | expandTyc tyc = tyc

	    fun expand ty = mapTypeFull expandTyc ty

	    fun mkDcon({name,rep,domain}: dconDesc) =
		DATACON{name = name, rep = rep, sign = sign, lazyp = lazyp,
			typ = dconType (tyc, Option.map expand domain),
			const = case domain of NONE => true | _ => false}

	in map mkDcon dcons
	end
      | extractDcons ERRORtyc = bug "extractDcons ERRORtyc"
      | extractDcons (DEFtyc _) = bug "extractDcons DEFtyc"
      | extractDcons _ = bug "extractDcons"

    fun mkStrict 0 = []
      | mkStrict n = true :: mkStrict(n-1)

  (* used in ElabSig for datatype replication specs, where the tyc arg
   * is expected to be either a GENtyc/DATATYPE or a PATHtyc. *)
    fun wrapDef (tyc as DEFtyc _,_) = tyc
      | wrapDef (tyc,s) =
	let val arity = tyconArity tyc
	    val name = tycName tyc
	    val args = boundargs arity
	 in DEFtyc{stamp=s,strict=mkStrict arity,path=IP.IPATH[name],
		   tyfun=TYFUN{arity=arity,body=CONty(tyc,args)}}
	end

  (* eta-reduce a type function: \args.tc args => tc *)
    fun unWrapDef1 (tyc as DEFtyc{tyfun=TYFUN{body=CONty(tyc',args),arity},...}) =
	 let fun formals((IBOUND i)::rest,j) = if i=j then formals(rest,j+1) else false
	       | formals(nil,_) = true
	       | formals _ = false
	  in if formals(args,0) then SOME tyc' else NONE
	 end
      | unWrapDef1 tyc = NONE

  (* closure under iterated eta-reduction *)
    fun unWrapDefStar tyc = (case unWrapDef1 tyc
	    of SOME tyc' => unWrapDefStar tyc'
	     | NONE => tyc)

  (* dummyTyGen produces a generator of dummy types with names X0, X1, etc.
   * These are used to to instantiate type metavariables in top-level val
   * decls that are not generalized because of the value restriction. *)
    fun dummyTyGen () : unit -> Types.ty =
	let val count = ref 0
	    fun next () = (count := !count + 1; !count)
	    fun nextTy () =
		let val name = "X"^Int.toString(next())
		 in CONty(GENtyc{stamp = ST.special name,
				 path = IP.IPATH[S.tycSymbol name],
				 arity = 0, eq = ref NO,
				 kind = ABSTRACT BT.boolTycon,
				 stub = NONE},
			  [])
		end
	 in nextTy
	end

  (* a crude translation of types to strings *)
    fun tyToString ty = let
	  fun showargs tys = (case tys
		 of nil => ""
		  | [ty] => tyToString ty
		  | ty::tys => concat[tyToString ty, ",", showargs tys]
		(* end case *))
	  in
	    case ty
	     of VARty(ref kind) =>
		(case kind
		   of INSTANTIATED _ => "<tv:INSTANTIATED>"
		    | OPEN _ => "<tv:OPEN>"
		    | UBOUND _ => "<tv:UBOUND>"
		    | OVLDV _ => "<tv:OVLDV>"
		    | OVLDI _ => "<tv:OVLDI>"
		    | OVLDW _ => "<tv:OVLDW>"
		    | LBOUND _ => "<tv:LBOUND>"
		  (* end case *))
	      | IBOUND n => concat["<", Int.toString n, ">"]
	      | CONty(tyc,args) =>
		if BT.isArrowType ty
		then concat["(", tyToString (BT.domain ty), " -> ",
			    tyToString (BT.range ty), ")"]
		else if null args
		   then Symbol.name(tycName tyc)
		   else concat["(", showargs args, ") ", Symbol.name(tycName tyc)]
	      | POLYty{tyfun=TYFUN{body,arity},...} =>
		concat["<P", Int.toString arity, ">[", tyToString body, "]"]
	      | WILDCARDty => "<wc>"
	      | UNDEFty => "<ud>"
	      | MARKty (ty,_) => tyToString ty
	  end

  (* return size and signedness information about integer and word types.  We use a width
   * of zero for IntInf.int.
   *)
    fun numInfo ty = let
	  fun int w = {wid = w, signed = true}
	  fun word w = {wid = w, signed = false}
	  val ty = prune ty
	  in
	    if equalType(ty, BT.intTy) then int Target.defaultIntSz
	    else if equalType(ty, BT.wordTy) then word Target.defaultIntSz
	    else if equalType(ty, BT.intinfTy) then int 0
	    else if equalType(ty, BT.int32Ty) then int 32
	    else if equalType(ty, BT.int64Ty) then int 64
	    else if equalType(ty, BT.word8Ty) then word 8
	    else if equalType(ty, BT.word32Ty) then word 32
	    else if equalType(ty, BT.word64Ty) then word 64
	    else ErrorMsg.impossible(concat[
		"TypeUtil.numInfo(", tyToString ty, ")"
	      ])
	  end

    fun numInRange (n, ty) =
	let fun pow2 w = IntInf.<<(1, Word.fromInt w)
	 in case numInfo ty
	      of {wid=0, ...} => true
	         (* IntInf.int literals are always in range! *)
	       | {wid, signed=true} =>
		   let val limit = pow2(wid-1)
		    in (~limit <= n) andalso (n < limit)
		   end
	       | {wid, ...} => (n < pow2 wid)
		 (* we assume that n > 0, since it is unsigned *)
	      (* end case *)
	end


    (* dataconToTycon : datacon -> tycon *)
    fun dataconToTycon (DATACON{typ, ...}) =
	let val typ' =
		case typ
		  of POLYty{tyfun = TYFUN{body,...},...} => body
		   | _ => typ
	    val typ'' = if BT.isArrowType typ'
			then BT.range typ'
			else typ'
	in case headReduceType typ''
	     of CONty(tycon, _) => tycon
	      | _ => bug "dataconToTycon"
	end

    (* datatypeWidth : tycon -> int *)
    (* BUG: exception of an exn datacon, where tycon is PRIMITIVE exnTycon *)
    fun datatypeWidth (GENtyc {kind, ...}) =
        (case kind
	   of DATATYPE {index, family={members, ...}, ...} =>
                let val {dcons,...} = Vector.sub(members, index)
	         in length dcons
	        end
	    | PRIMITIVE => infinity (* exn pseudo datatype, width "infinite" *)
	    | ABSTRACT tycon => datatypeWidth tycon  (* probably impossible *)
	    | _ => bug "datatypeWidth: bad tycon" )
      | datatypeWidth _ = bug "datatypeWidth: not GENtyc"

    (* typeVariants : ty -> int *)
    fun typeVariants ty =
	(case headReduceType ty
	  of CONty (tycon, _) => datatypeWidth tycon
	   | _ => infinity)

    (* dataconName: datacon -> symbol *)
    fun dataconName (DATACON{name,...}) = name

    (* dataconType : datacon -> ty *)
    fun dataconType (DATACON{typ,...}) = typ

    (* dataconSign : datacon -> Access.consig *)
    fun dataconSign (DATACON{sign,...}) = sign

    (* dataconIsConst : datacon -> bool *)
    fun dataconIsConst (DATACON{const,...}) = const

    (* dataconWidth: datacon -> int *)
    val dataconWidth = datatypeWidth o dataconToTycon

    (* vectorElemTy : ty -> ty *)
    fun vectorElemTy ty =
	(case (headReduceType ty)
           of CONty(tycon,[argTy]) =>
	      if equalTycon(tycon, BT.vectorTycon) then argTy
	      else bug "vectorElemTy: CONty, not vector"
	    | VARty _ => bug "vectorElemTy: VARty"
	    | IBOUND _ => bug "vectorElemTy: IBOUND"
	    | POLYty _ => bug "vectorElemTy: POLYty"
	    | WILDCARDty => bug "vectorElemTy: WILDCARDty"
	    | UNDEFty => bug "vectorElemTy: UNDEFty"
	    | MARKty _ => bug "vectorElemTy: MARKty"
	    | CONty(tycon,args) =>
	      (if equalTycon(tycon, BT.vectorTycon)
	       then print "TypesUtil.vectorTycon: vectorTycon -- ok\n"
	       else print ("TypesUtil.vectoTycon: bad tycon: "
			   ^ Symbol.name (tycName tycon) ^ "\n");
	       (* bug ("vectorElemTy: CONty: |args| = " ^ Int.toString(length args)) *)
	       raise Fail "vectorElemTy"))

    (* replicateTy : ty * int -> ty list *)
    fun replicateTy (ty,len) =
	let fun build (0,tys) = tys
	      | build (n,tys) = build (n-1, ty::tys)
	in build (len,nil)
	end

    (* matchPoly : ty * int * ty -> ty vector *)
    (* ty is expected to be an instance of the polytype <arity, body>;
     * the vector returned consists of the instantiation parameters for
     * this instance. *)
    fun matchPoly (ty, arity, body) =
	let val instArray = Array.array(arity,UNDEFty)
	    fun match (target, scheme) =
		let val targetR = headReduceType target
		    val schemeR = headReduceType scheme  (* should do nothing *)
		in case (targetR, schemeR)
		    of (ty, IBOUND i) =>
		          (case Array.sub(instArray, i)
			     of UNDEFty =>
			        ((* print ("matchPoly:update:"^Int.toString i^"\n"); *)
				 Array.update(instArray, i, ty))
			      | instTy => if equalType(ty, instTy)
					  then ()
					  else bug "matchPoly 0")
		    | (CONty(tyc1,args1), (CONty(tyc2,args2))) =>
		        if equalTycon (tyc1,tyc2)
			then ListPair.appEq match (args1, args2)
			else bug "matchPoly 1"
		    | (ty1, ty2) => bug "matchPoly 2"
		end
	in match(ty, body);
	   Array.vector(instArray)
	end

    (* instTy : ty * ty vector -> ty *)
    fun instTy (ty, vec) =
        (case (headReduceType ty)
	  of CONty(tyc, args) =>
	     ((* print ("instTy:CONty: " ^ (S.name(tycName tyc)) ^ "\n"); *)
	      CONty(tyc, map (fn ty => instTy(ty,vec)) args))
	   | IBOUND i => ((*print ("instTy:IBOUND:"^Int.toString i^"\n");*) Vector.sub(vec,i))
	   | POLYty _ => bug "instTy:POLYty"
	   | WILDCARDty => bug "instTy:WILDCARDty"
	   | UNDEFty => bug "instTy:UNDEFty"
	   | MARKty _ => bug "instTy:MARKty"
	   | VARty _ => bug "instTy:VARty")

    (* destructDataconTy : T.ty * datacon -> T.ty
     * given the type instTy of an application of the (nonconstant) dcon,
     * returns the corresponding instance of the domain of the dcon *)
    fun destructDataconTy (instRangeTy, dcon) =
	(* ASSERT: dcon is not a constant dcon, and ty is the range of an instance of
	 * the polymorphic type of dcon *)
	let val dconTy = headReduceType (dataconType dcon) (* should do nothing? *)
	 in case dconTy
	     of POLYty{sign, tyfun = TYFUN{arity,body}} =>
		  if BT.isArrowType body
		  then let val (domainScheme, rangeScheme) = BT.domainNrange body
			   val instVector = matchPoly (instRangeTy, arity, rangeScheme)
		       in instTy (domainScheme, instVector)
		       end
		  else bug "destructDataconTy"
	      | _ => (* dcon not polymorphic: instTy = BT.range dconty *)
		BT.domain dconTy
	end

    (* destructRecordTy : ty -> ty list *)
    fun destructRecordTy recTy =
	(case headReduceType recTy
	   of (CONty(_, elemTys)) => elemTys
	    | POLYty{sign, tyfun = TYFUN{arity,body}} =>
	        bug "destructRecordTy: POLYty"
	    | ty => bug "destructRecordTy: ?")

    (* dePoly : T.ty -> T.ty  -- depolymorphise a type *)
    fun dePoly (T.POLYty {tyfun as T.TYFUN{arity, ...}, ...}) =
	  applyTyfun (tyfun, List.tabulate (arity, (fn i => T.UNDEFty)))
      | dePoly ty = ty

    (* dePolyVar : V.var -> T.ty  -- depolymorphise type of a variable *)
    fun dePolyVar var = dePoly (V.varType var)

  end (* structure TypesUtil *)
