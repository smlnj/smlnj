(* Copyright (c) 1998 by Lucent Technologies *)

structure TypeUtil : TYPE_UTIL =
struct

  structure S = Symbol
  structure Pid = Pid
  structure Tid = Tid
  structure B = Bindings
  structure TypeCheckControl = Config.TypeCheckControl

  exception TypeError of Ast.ctype

  (* some parameters used here, but passed in that should be lifted out of here *)
  fun warning s = (print "warning "; print s; print "\n")

  fun internalError s = (print "internal error "; print s; print "\n")

  val don't_convert_SHORT_to_INT = TypeCheckControl.don't_convert_SHORT_to_INT
                                            (* In ANSI C, usual unary converstion converts
					       SHORT to INT; for DSP code, we want to
					       keep SHORT as SHORT.
					       Default: true for ANSI C behavior *)

  val don't_convert_DOUBLE_in_usual_unary_cnv = TypeCheckControl.don't_convert_DOUBLE_in_usual_unary_cnv
                                            (* In ANSI, FLOAT is not converted to DOUBLE during
					       usual unary converstion; in old style compilers
					       FLOAT *is* converted to DOUBLE.
					       Default: true for ANSI behavior *)

  val enumeration_incompatibility = TypeCheckControl.enumeration_incompatibility
                                            (* ANSI says that different enumerations are incomptible
					       (although all are compatible with int);
					       older style compilers say that different enumerations
					       are compatible.
					       Default: true for ANSI behavior *)
					       
  val pointer_compatibility_quals = TypeCheckControl.pointer_compatibility_quals
                                            (* ANSI says that pointers to differently qualified types
					       are different; some compilers vary.
					       Default: true for ANSI behavior *)

  val stdInt = Ast.Numeric(Ast.NONSATURATE, Ast.WHOLENUM, Ast.SIGNED, Ast.INT, Ast.SIGNASSUMED)
   
  fun ctToString tidtab =
    PPLib.ppToString (PPAst.ppCtype () tidtab)
    (* pid table actually not needed to print out a ct, but it is 
       a parameter passed to ppCtype, so just fudge one to make types work.
       This is ugly dpo?
     *)

  fun reduceTypedef (tidtab: Tables.tidtab) ty = 
    case ty
      of Ast.TypeRef tid => 
	   (case Tidtab.find (tidtab,tid)
	      of SOME{ntype=SOME(B.Typedef (_,ty)),...} => reduceTypedef tidtab ty
	       | _ => ( internalError "poorly formed type table (unresolved type id),assuming Void"
		      ; Ast.Void
		      )

)
       | ty => ty

  fun getCoreType tidtab ty =
      (* derefs typedefs and and removes qualifiers *)
    case ty
      of Ast.TypeRef tid => getCoreType tidtab (reduceTypedef tidtab ty)
       | Ast.Qual (_,ty) => getCoreType tidtab ty
       | ty => ty

  fun checkQuals tidtab ty =
    let fun check ty = 
      (case ty
	 of Ast.TypeRef tid => check (reduceTypedef tidtab ty)
       | Ast.Qual (q,ty) => 
	   let val {volatile, const, cerr, verr} = check ty
	   in
	     case q of
	       Ast.CONST => {volatile=volatile, const=true, verr=verr, cerr=const}
	     | Ast.VOLATILE => {volatile=true, const=const, cerr=cerr, verr=volatile}
	   end
       | ty => {volatile=false, const=false, verr=false, cerr=false})
	val res = check ty
    in
      {redundantConst = #cerr res,
       redundantVolatile = #verr res}
    end

  fun getQuals tidtab ty =
      (* collects qualifiers *)
    case ty
      of Ast.TypeRef tid => getQuals tidtab (reduceTypedef tidtab ty)
       | Ast.Qual (q,ty) => 
	let val {volatile, const, ty} = getQuals tidtab ty
	in
	  case q of
	    Ast.CONST => {volatile=volatile, const=true, ty=ty}
	  | Ast.VOLATILE => {volatile=true, const=const, ty=ty}
	end
       | ty => {volatile=false, const=false, ty=ty}

(*
   fun hasKnownStorageSize tidtab {ty, withInitializer} =
           (* withInitializer=true: does ty have known storage size when an initializer is present (see array case)
	      withInitializer=false: does ty have known storage size, period. *)
    case ty of
      Ast.Void => false
    | Ast.Qual(_, ty) => hasKnownStorageSize tidtab ty
    | Ast.Numeric _ => true
    | Ast.Array(SOME _, ty) => hasKnownStorageSize tidtab ty
    | Ast.Array(NONE, _) => withInitializer
    | Ast.Pointer _ => true
    | Ast.Function _ => true
    | Ast.EnumRef tid => true
    | Ast.AggrRef tid =>
	(case Tidtab.find (tidtab,tid)
	   of SOME(_,SOME(Ast.Aggr (_,_,fields)),_) => 
	       List.foldl
	         (fn ((ty, _, _), b) => b andalso (hasKnownStorageSize tidtab ty))
	         true fields
	    | _ => false)
    | Ast.TypeRef tid => hasKnownStorageSize tidtab (reduceTypedef tidtab ty)
    | Ast.Ellipses => false
*)


(* nch fix: 
    hasKnownStorageSize should reuse some code from
    sizeof -- same kinds of checks and memoization 
*)


   fun hasKnownStorageSize (tidtab: Tables.tidtab) ty =
    case ty
      of Ast.Void => false
       | Ast.Qual(_, ty) => hasKnownStorageSize tidtab ty
       | Ast.Numeric _ => true
       | Ast.Array(SOME _, ty) => hasKnownStorageSize tidtab ty
       | Ast.Array(NONE, _) => false
       | Ast.Pointer _ => true
       | Ast.Function _ => true
       | Ast.EnumRef tid => 
	(case Tidtab.find (tidtab,tid)
	   of SOME{ntype=SOME _, ...} => true
	 | _ => 
	     if TypeCheckControl.partial_enums_have_unknown_size then false
	     else true)
       | Ast.StructRef tid =>
	(case Tidtab.find (tidtab,tid)
	   of SOME{ntype=SOME(B.Struct (_,fields)),...} => 
	       List.all
	         (fn (ty, _, _) => (hasKnownStorageSize tidtab ty))
		 fields
	    | _ => false)
       | Ast.UnionRef tid =>
	(case Tidtab.find (tidtab,tid)
	   of SOME{ntype=SOME(B.Union (_,fields)),...} => 
	       List.all
	         (fn (ty, _) => (hasKnownStorageSize tidtab ty))
		 fields
	    | _ => false)
       | Ast.TypeRef tid => hasKnownStorageSize tidtab (reduceTypedef tidtab ty)
       | Ast.Ellipses => false
       | Ast.Error => false

(*
  fun fixArrayType tidtab {ty, n} =
    case ty of
      Ast.Void => {err=(n<=1), ty}
    | Ast.Qual(_, ty) => fixArrayType tidtab {ty=ty, n=n}
    | Ast.Numeric _ => {err=(n<=1), ty}
    | Ast.Array(SOME n', ty) => {err=(n<=n'), ty}
    | Ast.Array(NONE, ty) => {err=true, Ast.Array(SOME n, ty})
    | Ast.Pointer _ => {err=(n<=1), ty}
    | Ast.Function _ => {err=(n<=1), ty}
    | Ast.EnumRef tid => {err=(n<=1), ty}
    | Ast.AggrRef tid => {err=(n<=1), ty}
    | Ast.TypeRef tid => fixArrayType tidtab {ty=reduceTypedef tidtab ty, n=n}
    | Ast.Ellipses => {err=false, ty}
*)

  fun isConst tidtab ty = #const(getQuals tidtab ty)

  fun isPointer tidtab ty =
      case ty
	of Ast.Qual (_,ty) => isPointer tidtab ty
	 | Ast.Array _ => true
	 | Ast.Pointer _ => true
	 | Ast.Function _ => true
	 | Ast.TypeRef _ => isPointer tidtab (reduceTypedef tidtab ty)
	 | _ => false

  fun isIntegral tidtab ty =
      case ty
	of Ast.Qual (_,ty) => isIntegral tidtab ty
	 | Ast.Array _ => false
	 | Ast.Pointer _ => false
	 | Ast.Function _ => false
	 | Ast.Numeric(sat, frac, sign, Ast.CHAR, _) => true
	 | Ast.Numeric(sat, frac, sign, Ast.SHORT, _) => true
	 | Ast.Numeric(sat, frac, sign, Ast.INT, _) => true
	 | Ast.Numeric(sat, frac, sign, Ast.LONG, _) => true
	 | Ast.Numeric(sat, frac, sign, Ast.LONGLONG, _) => true
	 | Ast.Numeric(sat, frac, sign, Ast.FLOAT, _) => false
	 | Ast.Numeric(sat, frac, sign, Ast.DOUBLE, _) => false
	 | Ast.Numeric(sat, frac, sign, Ast.LONGDOUBLE, _) => false
	 | Ast.EnumRef _ => true
	 | Ast.TypeRef _ => isIntegral tidtab (reduceTypedef tidtab ty)
	 | _ => false

  fun isArray tidtab ty =
      case ty
        of Ast.Qual (_,ty) => isArray tidtab ty
	 | Ast.Array _ => true
	 | Ast.TypeRef _ => isArray tidtab (reduceTypedef tidtab ty)
	 | _ => false

  fun isNumberOrPointer tidtab ty =
      case ty
	of Ast.Qual (_,ty) => isNumberOrPointer tidtab ty
	 | Ast.Array _ => true
	 | Ast.Pointer _ => true
	 | Ast.Function _ => true
	 | Ast.Numeric _ => true
	 | Ast.EnumRef _ => true
	 | Ast.TypeRef _ => isNumberOrPointer tidtab (reduceTypedef tidtab ty)
	 | _ => false

  fun isNumber tidtab ty =
      case ty
	of Ast.Qual (_,ty) => isNumber tidtab ty
	 | Ast.Array _ => false
	 | Ast.Pointer _ => false
	 | Ast.Function _ => false
	 | Ast.Numeric _ => true
	 | Ast.EnumRef _ => true
	 | Ast.TypeRef _ => isNumber tidtab (reduceTypedef tidtab ty)
	 | _ => false

  fun deref tidtab ty =
      case ty
	of Ast.Qual (_,ty) => deref tidtab ty
	 | Ast.Array (_,ty) => SOME ty
	 | Ast.Pointer ty => SOME ty
	 | Ast.Function _ => SOME ty
	 | Ast.TypeRef _ => deref tidtab (reduceTypedef tidtab ty)
	 | _ => NONE

  fun getFunction tidtab ty =
    let fun getF ty {deref} =
      case ty
	of Ast.Qual (_,ty) => getF ty {deref=deref}
      | Ast.Pointer ty => if deref then NONE else getF ty {deref=true}
	  (* allow one level of dereferencing of function pointers
             see H & S p 147: "an expression of type `pointer to function' can be used in a 
		              function call without an explicit dereferencing" *)
      | Ast.Function (retTy,argTys) => SOME(retTy,argTys)
      | Ast.TypeRef _ => getF (reduceTypedef tidtab ty) {deref=deref}
      | _ => NONE
    in
      getF ty {deref=false}
    end

  fun isFunction tidtab ty =  (* returns true of ty is a function; excludes fn pointer case *)
      case reduceTypedef tidtab ty of (* might have prototype fn def using typedef?? *)
	Ast.Function _ => true
      | _ => false

  fun isFunctionPrototype tidtab ty =
      case getFunction tidtab ty of
	   NONE => false
	 | SOME(_, nil) => false
	 | SOME(_, _ :: _) => true

  fun isNonPointerFunction tidtab ty =
      case ty
	of Ast.Qual (_,ty) => isNonPointerFunction tidtab ty
	 | Ast.TypeRef _ => isNonPointerFunction tidtab (reduceTypedef tidtab ty)
	 | Ast.Function _ => true
	 | _ => false

  fun isStructOrUnion tidtab ty =
    case reduceTypedef tidtab ty
      of Ast.Qual (_,ty) => isStructOrUnion tidtab ty
       | (Ast.StructRef tid | Ast.UnionRef tid) => SOME tid
       | _ => NONE

  fun isEnum tidtab (ty,member as {uid,kind=Ast.ENUMmem _,...}: Ast.member) =
    (case reduceTypedef tidtab ty
      of Ast.Qual (_,ty) => isEnum tidtab (ty,member)
       | Ast.EnumRef tid =>
	   (case Tidtab.find (tidtab,tid)
	      of SOME {ntype=SOME (B.Enum (_,memberIntList)),...} => 
		   let fun pred ({uid=uid',...}: Ast.member,_) =
		           Pid.equal (uid',uid)
		   in List.exists pred memberIntList end
	       | SOME {ntype=NONE,...} =>
		   (warning
		     "Enum type used but not declared, assuming member is not an EnumId";
		    false)
	       | SOME {ntype=SOME _,...} =>
		   (internalError
		     ("poorly formed type table: expected enumerated type for "
		      ^ (Tid.toString tid));
		    false)
	       | NONE =>
		   (internalError
		     ("poorly formed type table: expected enumerated type for "
		      ^ (Tid.toString tid));
		    false))
       | _ => false)
    | isEnum tidtab (ty,member) = 
       (internalError "isEnum applied to struct or union member";
	false)

  fun lookupEnum tidtab (ty,member as {uid,...}: Ast.member) =
    case reduceTypedef tidtab ty
      of Ast.Qual (_,ty) => lookupEnum tidtab (ty,member)
       | Ast.EnumRef tid =>
	   (case Tidtab.find (tidtab,tid)
	      of SOME{ntype=SOME(B.Enum(_,memberIntList)),...} => 
		   let fun pred ({uid=uid',...}: Ast.member,_) =
		           Pid.equal(uid', uid)
		   in case List.find pred memberIntList
		        of SOME (_,i) => SOME i
		         | NONE => NONE
		   end
	       | _ => NONE)
       | _ => NONE

  (* Haberson/Steele "C Reference Manual", 4th Ed, section 5.11.1 p152 *)
  fun equalType tidtab (ty1,ty2) =
    let open Ast
	fun eq (ty1,ty2) = 
	  case (ty1,ty2)
	    of (Void, Void) => true
	     | (Qual(q1, ct1), Qual(q2, ct2)) => 
		   (q1 = q2) andalso eq (ct1, ct2)
	     | (Numeric(sat1, frac1, sign1, intKnd1, signednessTag1),
		Numeric(sat2, frac2, sign2, intKnd2, signednessTag2)) =>
		   sat1 = sat2 andalso frac1 = frac2 andalso
		   sign1 = sign2 andalso intKnd1 = intKnd2
		   (* note: don't require signednessTags to be the same *)
	     | (Array(SOME(i1, _), ct1), Array(SOME(i2,_), ct2)) => (i1=i2) andalso eq (ct1, ct2)
	     | (Array(NONE, ct1), Array(NONE, ct2)) => eq (ct1, ct2)
	     | (Array _, Array _) => false
	     | (Pointer ct1, Pointer ct2) => eq (ct1, ct2)
	     | (Function(ct1, ctl1), Function(ct2, ctl2)) =>
		   eq (ct1, ct2) andalso eql (ctl1, ctl2)
	     | (EnumRef tid1, EnumRef tid2) => Tid.equal (tid1, tid2)
	     | (UnionRef tid1, UnionRef tid2) => Tid.equal (tid1, tid2)
	     | (StructRef tid1, StructRef tid2) => Tid.equal (tid1, tid2)
	     | (TypeRef _, _) => eq (reduceTypedef tidtab ty1, ty2)
	     | (_, TypeRef _) => eq (ty1, reduceTypedef tidtab ty2)
	     | _ => false
	and eql ([],[]) = true
	  | eql ((ty1,_)::tyl1,(ty2,_)::tyl2) =
	    eq (ty1,ty2) andalso eql (tyl1,tyl2)
	  | eql _ = false
    in eq (ty1,ty2) end

(* implements "ISO C conversion" column of table 6-4 in Haberson/Steele, p175
 "C Reference Manual", 4th Ed *)

  fun usualUnaryCnv tidtab tp =
    let val tp = getCoreType tidtab tp
    in case tp
      of Ast.Numeric (sat, frac, _, Ast.CHAR, _) =>
	  Ast.Numeric (sat, frac, Ast.SIGNED, if don't_convert_SHORT_to_INT then Ast.SHORT else Ast.INT, Ast.SIGNASSUMED)
       | Ast.Numeric (sat, frac, _, Ast.SHORT,_) =>
	  Ast.Numeric (sat, frac, Ast.SIGNED, if don't_convert_SHORT_to_INT then Ast.SHORT else Ast.INT, Ast.SIGNASSUMED)
      (* for dsp work, want to keep short as short *)
       | ty as (Ast.Numeric (sat, frac, sign, Ast.FLOAT, d)) =>
	  if don't_convert_DOUBLE_in_usual_unary_cnv then ty else Ast.Numeric (sat, frac, sign, Ast.DOUBLE, d)
       | Ast.Array (_, arrayTp) => if (Config.DFLAG) then tp else Ast.Pointer arrayTp
       | Ast.Function x => Ast.Pointer tp  (* this code is now not used: it is overridden by the stronger condition that
					    all expressions of Function type are converted to Pointer(Function),
					    (except for & and sizeof) *)
       | Ast.EnumRef _ => stdInt
	  (* Not explicit in table 6-4, but seems to be implicitly assumed -- e.g. see compatiblity *)
       | _ => tp
    end

  (* implements section 6.3.5 of H&S, p177. *)
  fun functionArgConv tidtab tp = 
    case getCoreType tidtab tp
      of 
	(Ast.Numeric (sat, frac, sign, Ast.FLOAT, d)) => 
         Ast.Numeric (sat, frac, sign, Ast.DOUBLE, d)
      | _ => usualUnaryCnv tidtab tp

  fun combineSat (Ast.SATURATE, Ast.SATURATE) = Ast.SATURATE
    | combineSat _ = Ast.NONSATURATE

  fun combineFrac (Ast.FRACTIONAL, _) = Ast.FRACTIONAL
    | combineFrac (_, Ast.FRACTIONAL) = Ast.FRACTIONAL
    | combineFrac _ = Ast.WHOLENUM

(* follows "ISO C conversion" column of table 6-5 in Haberson/Steele, p176
 "C Reference Manual", 4th Ed *)
  fun usualBinaryCnv tidtab (tp1,tp2) =
    case ( usualUnaryCnv tidtab (getCoreType tidtab tp1)
         , usualUnaryCnv tidtab (getCoreType tidtab tp2)
	 )
      of ( Ast.Numeric(sat1, frac1, sign1, int1, d1)
	 , Ast.Numeric(sat2, frac2, sign2, int2, d2)
	 ) =>
	 (* removes CHAR, and (maybe) SHORT *)
  	 let val (sign', int') =
	       case ((sign1, int1), (sign2, int2))
		 of ((_, Ast.LONGDOUBLE), _) => (Ast.SIGNED, Ast.LONGDOUBLE)
	          | (_, (_, Ast.LONGDOUBLE)) => (Ast.SIGNED, Ast.LONGDOUBLE)
		  | ((_, Ast.DOUBLE), _) => (Ast.SIGNED, Ast.DOUBLE)
	          | (_, (_, Ast.DOUBLE)) => (Ast.SIGNED, Ast.DOUBLE)
		  | ((_, Ast.FLOAT), _) => (Ast.SIGNED, Ast.FLOAT)
		  | (_, (_, Ast.FLOAT)) => (Ast.SIGNED, Ast.FLOAT)
       
		  (* we've removed: LONGDOUBLE, DOUBLE, FLOAT, CHAR and (maybe) SHORT *)
		  (* this leaves: INT, LONG, LONGLONG and (possibly) SHORT *)
		  | (x1, x2) =>
		     let
		       val int' =
			 case (int1, int2)
			   of (Ast.LONGLONG, _) => Ast.LONGLONG
			 | (_, Ast.LONGLONG) => Ast.LONGLONG
			 | (Ast.LONG, _) => Ast.LONG
			 | (_, Ast.LONG) => Ast.LONG
			 | (Ast.INT, _) => Ast.INT
			 | (_, Ast.INT) => Ast.INT
			 | (Ast.SHORT, _) => Ast.SHORT
			 | (_, Ast.SHORT) => Ast.SHORT
			 | _ => int1  (* should be nothing left *)
		       val sign' = 
			 case (sign1, sign2)
			   of (Ast.UNSIGNED, _) => Ast.UNSIGNED
			    | (_, Ast.UNSIGNED) => Ast.UNSIGNED
			    | _ => Ast.SIGNED
		     in (sign', int') end
	 in
	     SOME ( Ast.Numeric(combineSat(sat1, sat2)
	          , combineFrac(frac1, frac2), sign', int', Ast.SIGNASSUMED)
		  )
	 end
     | (tp1', tp2') => 
	 (print "Warning: unexpected call of usualBinaryCnv on non-Numeric types\n";
	  if equalType tidtab (tp1',tp2')
	    then SOME tp1'
	  else NONE)

  (* Many compilers consider function args to be compatible when they
     can be converted to pointers of the same type *)
  fun preArgConv tidtab ty =
    (case reduceTypedef tidtab ty of
         Ast.Array (_, arrayTp) => Ast.Pointer arrayTp
       | Ast.Function x => Ast.Pointer ty
       | Ast.Qual(q, ty) => Ast.Qual(q, preArgConv tidtab ty)
       | _ => ty)

  (* Used to convert function args of type Function(...) to Pointer(Function(...)) *)
  fun cnvFunctionToPointer2Function tidtab ty =
    (case getCoreType tidtab ty of
      (coreType as (Ast.Function _)) => Ast.Pointer(coreType)
    | _ => ty)

  (* section 5.11, pp151-155, in Haberson/Steele "C Reference Manual", 4th Ed *)
  fun composite tidtab (ty1,ty2) =
    let
      open Ast
      fun enumCompose (tid, ty) =
	(case ty of
	   EnumRef tid2 => 
	     if enumeration_incompatibility then
	       if Tid.equal(tid, tid2) then SOME ty else NONE
	     else
	       SOME ty  (* old style: all enums are compatible *)

	 | Numeric(NONSATURATE, WHOLENUM, SIGNED, INT, d) => SOME(Numeric(NONSATURATE, WHOLENUM, SIGNED, INT, d))
	 (* enumeration types are always compatible with the underlying implementation type, 
	  assume in this frontend to the int *)
	 | _ => NONE)

      fun composeid (NONE, x2) = x2
	| composeid (x1, NONE) = x1
	| composeid (x1 as SOME (i1: Ast.id), SOME (i2: Ast.id)) =
	  if Symbol.equal (#name i1, #name i2) then x1 else NONE
	     
      fun compose (ty1,ty2) = 
	let val ty1 = if pointer_compatibility_quals then ty1 else getCoreType tidtab ty1
	  val ty2 = if pointer_compatibility_quals then ty2 else getCoreType tidtab ty2
	  fun em1() = ("Prototype " ^ (ctToString tidtab ty1) ^
	              " and non-prototype " ^ (ctToString tidtab ty2) ^
		      " are not compatible because parameter is not compatible with the" ^
		      " type after applying default argument promotion.")
	  fun em2() = ("Prototype " ^ (ctToString tidtab ty2) ^
	              " and non-prototype " ^ (ctToString tidtab ty1) ^
		      " are not compatible because parameter is not compatible with the" ^
		      " type after applying default argument promotion.")
	  in
	  case (ty1,ty2)
	    of
	      (Void, Void) => (SOME(Void), nil)
	    | (TypeRef _, _) => compose (reduceTypedef tidtab ty1, ty2)
	    | (_, TypeRef _) => compose (ty1, reduceTypedef tidtab ty2)
	    | (EnumRef tid1, _) => (enumCompose(tid1, ty2), nil)
	    | (_, EnumRef tid2) => (enumCompose(tid2, ty1), nil)
	    | (Array(io1, ct1), Array(io2, ct2)) => 
		(case (compose(ct1, ct2), io1, io2) of
		   ((SOME ct, eml), NONE, NONE) => (SOME(Array(NONE, ct)), eml)
		 | ((SOME ct, eml), SOME opt1, NONE) => (SOME(Array(SOME opt1, ct)), eml)
		 | ((SOME ct, eml), NONE, SOME opt2) => (SOME(Array(SOME opt2, ct)), eml)
		 | ((SOME ct, eml), SOME(i1, expr1), SOME(i2, _)) =>
                     (* potential source-to-source problem: what if i1=i2, but expr1 and expr2 are diff? *)
		     if (i1 = i2) then (SOME(Array(SOME(i1, expr1), ct)),
					eml)
		     else (NONE, "Arrays have different lengths." :: eml)
		 | ((NONE,eml),_, _) => (NONE,eml))
	    | (Function(ct1, nil), Function(ct2, nil)) => (* both non-prototypes *)
		   (case compose (ct1, ct2) of
		      (NONE, eml) => (NONE, eml)
		    | (SOME ct, eml) => (SOME(Function(ct, nil)), eml))
	    | (Function(ct1, [(Void, _)]), Function(ct2, nil)) => (* first is Void-arg-prototype *)
		   (case compose (ct1, ct2) of
		      (NONE, eml) => (NONE, eml)
		    | (SOME ct, eml) => (SOME(Function(ct, [(Void,NONE)])), eml))
	    | (Function(ct1, nil), Function(ct2, [(Void,_)])) => (* second is Void-arg-prototype *)
		   (case compose (ct1, ct2) of
		      (NONE, eml) => (NONE, eml)
		    | (SOME ct, eml) => (SOME(Function(ct, [(Void,NONE)])), eml))
	    | (Function(ct1, ctl1), Function(ct2, nil)) => (* first is prototype *)
		   (case (compose(ct1, ct2), checkArgs ctl1) of
		      ((SOME ct,eml), fl) => (SOME(Function(ct, ctl1)), if fl then eml else (em1()) :: eml)
		    | ((NONE, eml), fl) => (NONE, if fl then eml else (em1()) :: eml))
	    | (Function(ct1, nil), Function(ct2, ctl2)) => (* second is prototype *)
		   (case (compose(ct1, ct2), checkArgs ctl2) of
		      ((SOME ct, eml), fl) => (SOME(Function(ct, ctl2)), if fl then eml else (em2()) :: eml)
		    | ((NONE, eml), fl) => (NONE, if fl then eml else (em2()) :: eml))
	    | (Function(ct1, ctl1), Function(ct2, ctl2)) => (* both are prototypes *)
	           (case (compose (ct1, ct2), composel (ctl1, ctl2)) of (* composel: deals with ellipses *)
		      ((SOME ct, eml1), (SOME ctl, eml2)) => (SOME(Function(ct, ctl)), eml1 @ eml2)
		    | ((_, eml1), (_, eml2)) => (NONE, eml1 @ eml2))
	    | (ct1 as Qual _, ct2 as Qual _) => 
		      let val {volatile, const, ty=ct} = getQuals tidtab ct1
			val {volatile=volatile', const=const', ty=ct'} = getQuals tidtab ct2
		      in case compose (ct, ct') of
			(NONE, eml) => (NONE, eml)
		      | (SOME ct, eml) => let val ct = if volatile then Qual(VOLATILE, ct) else ct
					      val ct = if const then Qual(CONST, ct) else ct
					  in
					    (SOME ct, eml)
					  end
		      end
	    | (Numeric x, Numeric y) => if x = y then (SOME ty1, nil) else (NONE, nil)
	    | (Pointer ct1, Pointer ct2) => (case compose (ct1, ct2) of
					       (SOME ct, eml) => (SOME(Pointer ct), eml)
					     | (NONE, eml) => (NONE, eml))
	    | ((StructRef tid1, StructRef tid2) | (UnionRef tid1, UnionRef tid2)) =>
		if Tid.equal (tid1, tid2) then (SOME ty1, nil) else (NONE, nil)
	    | _ => (NONE, nil)
	end
      and checkArgs ((Ellipses, _) :: _) = true
	| checkArgs ((ct, _) :: ctl) = (case compose(ct, functionArgConv tidtab ct) of
				    (NONE, _) => false
				  | (SOME _, _) => checkArgs ctl
				      (* H & S, p 154, midpage:
				       each parameter type T must be compatible with the type
				       resulting from applying the usual unary conversions to T.
				       Correction: usual unary cnv except that float always
				          converted to unary (c.f. ISO conversion)
					  *)
				      )
	| checkArgs nil = true
	and composel ([],[]) = (SOME nil, nil)
	  | composel ([(Ast.Ellipses, _)], [(Ast.Ellipses, _)]) = (SOME([(Ast.Ellipses,NONE)]), nil)
	  | composel ([(Ast.Ellipses, _)], _) = (NONE,  ["Use of ellipses does not match."])
	  | composel (_, [(Ast.Ellipses, _)]) = (NONE,  ["Use of ellipses does not match."])
	  | composel ((ty1, id1)::tyl1,(ty2, id2)::tyl2) = 
	  (case (compose (ty1,ty2), composel (tyl1,tyl2)) of
	     ((SOME ty, eml1), (SOME tyl, eml2)) =>
	     (SOME((ty, composeid (id1, id2)) :: tyl), eml1@eml2)
	   | ((_, eml1), (_, eml2)) => (NONE, eml1@eml2))
	  | composel _ = (NONE, ["Function types have different numbers of arguments."])
    in compose (ty1,ty2) end

  fun compatible tidtab (ty1,ty2) = 
    (case composite tidtab (ty1,ty2) of
       (SOME _, _) => true
     | (NONE, _) => false)

  fun isAssignable tidtab {lhs, rhs, rhsExpr0} =
     (* From H&S p 174, table 6-3 (but also see Table 7-7, p221)
        Note 1: This function just checks that the implicit assignment conversion is allowable.
	        - it does not check that lhs is assignable.
        Note 2: The usualUnaryCnv conversion on rhs is not explicit in H & S,
	        but seems implied?
                (otherwise can't typecheck: int i[4], *j = i)
        Note 3: The definition below structure to correspond to table 6-3, but because of the
	        redundancy in this definition, we have reorganized order of some lines
        Note 4: The EnumRef case is not explicit in Table 6-3,
	        but seems implied by compatibility (and is needed).
      *)
    (case (getCoreType tidtab lhs, usualUnaryCnv tidtab rhs, rhsExpr0) of 
         (* Note: usualUnary eliminates: Array, Function and Enum *)

(*1*)    (Ast.Numeric _, Ast.Numeric _, _) => true

(*2a*)  | (ty1 as Ast.StructRef _, ty2 as Ast.StructRef _, _) => compatible tidtab (ty1, ty2)
(*2b*)  | (ty1 as Ast.UnionRef _, ty2 as Ast.UnionRef _, _) => compatible tidtab (ty1, ty2)

(*3a*) | (Ast.Pointer Ast.Void, _, true) => true
(*3c*) | (Ast.Pointer Ast.Void, Ast.Pointer Ast.Void, _) => true
(*3b*) | (Ast.Pointer Ast.Void, Ast.Pointer _, _) => true


(*5a*) | (Ast.Pointer (Ast.Function _), _, true) => true
(*5b*) | (Ast.Pointer (ty1 as Ast.Function _), Ast.Pointer (ty2 as Ast.Function _), _)
             => compatible tidtab (ty1,ty2)

(*4a*) | (Ast.Pointer ty1, _, true) => true
(*4c*) | (Ast.Pointer _, Ast.Pointer Ast.Void, _) => true
(*4b*) | (Ast.Pointer ty1, Ast.Pointer ty2, _) => 
       let 
	 val ty1' = getCoreType tidtab ty1
	 val ty2' = getCoreType tidtab ty2
	 val {volatile=vol1, const=const1, ...} = getQuals tidtab ty1
	 val {volatile=vol2, const=const2, ...} = getQuals tidtab ty2
	 val qual1 = vol1 orelse not vol2
	 val qual2 = const1 orelse not const2
       in
	 qual1 andalso qual2 andalso compatible tidtab (ty1',ty2')
       end
       | (Ast.EnumRef _, _, _) => isIntegral tidtab rhs

       | (ty1, ty2, fl) =>  (* this case is important when type checking function calls if
			       convert_function_args_to_pointers is set to false *)
          (equalType tidtab (ty1,ty2)) orelse
	  (equalType tidtab (ty1,getCoreType tidtab rhs)))

  fun isEquable tidtab {ty1, exp1Zero, ty2, exp2Zero} = (* for Eq and Neq *)
    (case (usualUnaryCnv tidtab ty1, exp1Zero, usualUnaryCnv tidtab ty2, exp2Zero) of  
       (Ast.Numeric _, _, Ast.Numeric _, _) => usualBinaryCnv tidtab (ty1, ty2) (* get common type *)
     | (Ast.Pointer Ast.Void, _, Ast.Pointer _, _) => SOME ty1
     | (Ast.Pointer _, _, Ast.Pointer Ast.Void, _) => SOME ty2
     | (Ast.Pointer _, _, _, true) => SOME ty1
     | (_, true, Ast.Pointer _, _) => SOME ty2
     | (ty1' as Ast.Pointer _, _, ty2' as Ast.Pointer _, _) =>
	 let val (x, _) = composite tidtab (ty1', ty2')  (* composite *AFTER* usualUnaryCnv! *)
	 in x
	 end
     | _ => NONE)

  fun conditionalExp tidtab {ty1, exp1Zero, ty2, exp2Zero} = (* for Eq and Neq *)
    (case (usualUnaryCnv tidtab ty1, exp1Zero, usualUnaryCnv tidtab ty2, exp2Zero) of  
       (Ast.Numeric _, _, Ast.Numeric _, _) => usualBinaryCnv tidtab (ty1, ty2) (* get common type *)
     | ((Ast.StructRef tid1, _, Ast.StructRef tid2, _) |
	(Ast.UnionRef tid1, _, Ast.UnionRef tid2, _)) =>
	  if Tid.equal (tid1, tid2) then SOME ty1
	  else NONE
     | (Ast.Void, _, Ast.Void, _) => SOME ty1

     | (Ast.Pointer _, _, Ast.Pointer Ast.Void, _) => SOME ty2
     | (Ast.Pointer Ast.Void, _, Ast.Pointer _, _) => SOME ty1

     | (ty1' as Ast.Pointer _, _, ty2' as Ast.Pointer _, _) =>
	    let val (x, _) = composite tidtab (ty1', ty2') (* composite *AFTER* usualUnaryCnv! *)
	    in 
	      x
	    end

     | (Ast.Pointer _, _, _, true) => SOME ty1
     | (_, true, Ast.Pointer _, _) => SOME ty2

     | (ty1, _, ty2, _) => NONE)

  fun isAddable tidtab {ty1, ty2} = (* for Plus *)
    (case (usualUnaryCnv tidtab ty1, usualUnaryCnv tidtab ty2) of  
       (Ast.Numeric _, Ast.Numeric _) => 
	 (case usualBinaryCnv tidtab (ty1, ty2) (* get common type *)
	    of SOME ty => SOME{ty1=ty, ty2=ty, resTy=ty}
	      | NONE => NONE)
     | (Ast.Pointer _, Ast.Numeric _) => 
	    if isIntegral tidtab ty2
	      then SOME{ty1=ty1, ty2=stdInt, resTy=ty1}
	    else NONE
     | (Ast.Numeric _, Ast.Pointer _) => 
	    if isIntegral tidtab ty1
	      then SOME{ty1=stdInt, ty2=ty2, resTy=ty2}
	    else NONE
     | _ => NONE)

  fun isSubtractable tidtab {ty1, ty2} = (* for Plus *)
    (case (usualUnaryCnv tidtab ty1, usualUnaryCnv tidtab ty2) of  
       (Ast.Numeric _, Ast.Numeric _) => 
	 (case usualBinaryCnv tidtab (ty1, ty2) (* get common type *)
	    of SOME ty => SOME{ty1=ty, ty2=ty, resTy=ty}
	      | NONE => NONE)
     | (ty1' as Ast.Pointer _, ty2' as Ast.Pointer _) => 
	    (case composite tidtab (ty1', ty2') of (* composite *AFTER* usualUnaryCnv *)
	       (SOME ty, _) => SOME{ty1=ty, ty2=ty, resTy=stdInt}
	     | (NONE, _) => NONE)
     | (Ast.Pointer _, Ast.Numeric _) => 
	    if isIntegral tidtab ty2 then SOME{ty1=ty1, ty2=stdInt, resTy=ty1}
	    else NONE
     | _ => NONE)

  fun isComparable tidtab {ty1, ty2} = (* for Eq and Neq *)
    (case (usualUnaryCnv tidtab ty1, usualUnaryCnv tidtab ty2) of  
       (Ast.Numeric _, Ast.Numeric _) => usualBinaryCnv tidtab (ty1, ty2) (* get common type *)
     | (ty1' as Ast.Pointer _, ty2' as Ast.Pointer _) =>
	 let val (x, _) = composite tidtab (ty1', ty2') (* composite *AFTER* usualUnaryCnv *)
	 in x
	 end
     | _ => NONE)

  fun checkFn tidtab (funTy, argTys, isZeroExprs) =
    (case getFunction tidtab funTy of
       NONE => (Ast.Void, ["Called object is not a function."], argTys)
     | SOME(retTy, paramTysIdOpts) =>
      let
	val paramTys = map #1 paramTysIdOpts
	val paramTys = case paramTys
	  of [Ast.Void] => nil (* a function with a single void argument is a function of no args *)
	| _ => paramTys
	fun isAssignableL n x = 
	  case x
	    of (Ast.Ellipses :: _, argl, _) => (nil, List.map (functionArgConv tidtab) argl)
	       (* Ellipses = variable arg length function *)
	     | (param :: paraml, arg :: argl, isZeroExpr :: isZeroExprs) =>
	      let val (strL, paraml) = isAssignableL (n+1) (paraml, argl, isZeroExprs)
		val strL' = if isAssignable tidtab {lhs=param, rhs=arg, rhsExpr0=isZeroExpr}
			      then strL
			    else
				let val msg = "Bad function call: arg " ^ Int.toString n ^
				  " has type " ^ (ctToString tidtab arg)
				  ^ " but fn parameter has type " ^ (ctToString tidtab param)
				in
				  msg :: strL
				end
	      in
		(strL', param :: paraml)
	      end
	     | (nil, nil, _) => (nil, nil)
     (* bugfix 12/Jan/00: the previous bugfix of 15/jun/99 overdid it a little (recursion!).
                          the case of a function with a single void arg is
			  now handled above in val paramTys = ...
	     | ([Ast.Void], nil) => (nil, nil) (* bugfix 15/jun/99: a function with a single void argument
                                               * is a function of no args  *)
     *)
	     | ((_, nil, _) | (_, _, nil)) => ( ["Type Warning: function call has too few args"]
					       , nil
					       )
	     | (nil, argl, _) => (["Type Warning: function call has too many args"]
				  , List.map (functionArgConv tidtab) argl
				  )
	val (msgL, argTys') = isAssignableL 1 (paramTys,argTys, isZeroExprs)
      in 
	(retTy, msgL, argTys')
      end)

  (* The notion of "scalar" types is not defined in e.g. K&R or H&S although
     it is refered to in H&S p218. 
     It is used to restrict the type of controlling expressions (e.g. while, do, for, ?:, etc.).
     According to the ISO standard (p24), scalars consist of
       a) arithmetic types (integral and floating types)
       b) pointer types
     This seems to exclude array and function types.

     However most compilers consider an array type to be scalar (i.e. just consider it a pointer).

     We shall assume that everthing is a scalar except: functions, unions and structs.
     Lint agrees with this; gcc and SGI cc disagree with this on functions. 
     *)
   
  fun isScalar tidtab ty =
    case ty
      of Ast.Qual (_,ty) => isScalar tidtab ty
       | Ast.Numeric _ => true
       | Ast.Pointer _ => true
       | Ast.Array _ => true
       | Ast.EnumRef _ => true 
       | Ast.TypeRef _ => isScalar tidtab (reduceTypedef tidtab ty)
       | Ast.Function _ => false (* although a function can be viewed as a pointer *)
       | Ast.StructRef _ => false
       | Ast.UnionRef _ => false
       | Ast.Ellipses => false  (* can't occur *)
       | Ast.Void => false
       | Ast.Error => false

end (* functor TypeUtilFn *)
