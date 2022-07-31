(* Copyright (c) 1998 by Lucent Technologies *)

(* buildast.sml
 *
 * Input: a parser tree
 * 
 * Output: a type checked abstract syntax tree, a map from
 *         expression adornments to types, and mappings from
 *         variables (uids) to types and type ids (uids) to types.
 *
 * AUTHORS: Michael Siff (siff@cs.wisc.edu) 
 *          Satish Chandra (chandra@research.bell-labs.com)
 *          Nevin Heintze (nch@research.bell-labs.com)
 *          Dino Oliva (oliva@research.bell-labs.com)
 *          Dave MacQueen (dbm@research.bell-labs.com)
 *
 * TBD:
 *   - needs to be tested for robustness
 *    (particularly type table and expression-type map)
 *   - add casts to constant expr evaluator 
 *)

(* Type checking: minor checks not implemented:
   3. no pointer or arrays of bitfields: most compiler (and lint) don't implement this.
   5. only storage-class specifier in a parameter declaration is register.
 *)


(* Notes: Treatment of function pointers.
   In C, the types Function(...) and Pointer(Function(...))
   are almost interchangeable.  If f is a function, then
   it can be called using ( *f )(args); if x is a function pointer,
   then the function it points to can be called using x(args)
   (Dennis R. says this was introduced by the pcc compiler, and then adopted by ANSI.)
   The auto-promotion of Function(...) and Pointer(Function(...)) has some
   strange consequences: ( ******f ) is just f.

   We deal with this as follows:
   1. all expressions of type Function(...) are immediately 
      promoted to type Pointer(Function(...))
   2. exceptions to (1) involving sizeof and &
      are handled as special cases in the code for unary operations.
   3. derefs of expressions of type Pointer(Function(...)) are eliminated.
   4. & of functions are eliminated.
   5. function parameters of type Function(...) are promoted to  Pointer(Function(...)).
*)

(* Changes to make sometime around April 1st, 99
   2. get rid of redundancy relating to topLevel/global (i.e. remove topLevel param)
      - once it's been tested.
*)

structure BuildAst : BUILD_AST =
struct

  type astBundle =
    {ast: Ast.ast,
     tidtab: Bindings.tidBinding Tidtab.uidtab,
     errorCount: int,
     warningCount: int,
     auxiliaryInfo: {aidtab: Tables.aidtab,
	             implicits: Tables.aidtab,
                     env: State.symtab}}

  (* imported structures w/abbreviations *)
  (* ----------------------------------- *)
  structure SM = SourceMap

  structure Aid = Aid
  structure Tid = Tid
  structure Pid = Pid
	
  structure PT = ParseTree
  structure Sym = Symbol
  structure B = Bindings
  structure PPL = PPLib
  structure S = State
  structure W = Word
  structure TU = TypeUtil
  structure TT = Tidtab
  structure AT = Aidtab
  structure TypeCheckControl = Config.TypeCheckControl

  (* local structures *)
  (* ---------------- *)
  (* DBM: an inefficient version of string binary map *)
  structure IdMap = BinaryMapFn (struct
				   type ord_key = string
				   val compare = String.compare
				 end)

  (* abstract syntax of translation unit in context *)
  type astBundle =
    {ast: Ast.ast,
     tidtab: Bindings.tidBinding Tidtab.uidtab,
     errorCount: int,
     warningCount: int,
     auxiliaryInfo: {aidtab: Tables.aidtab,
	             implicits: Tables.aidtab,
                     env: State.symtab}}

  val insert_explicit_coersions = ref false
  val insert_scaling = ref false
  val reduce_sizeof = ref false
  val reduce_assign_ops = ref false
  val multi_file_mode = ref false
  val local_externs_ok = ref true
  val default_signed_char = ref false

  fun multiFileMode () =
    (insert_explicit_coersions := false;
     insert_scaling :=  false;
     reduce_sizeof :=  false;
     reduce_assign_ops :=  false;
     multi_file_mode :=  true;
     local_externs_ok := true)

  fun compilerMode () =
    (insert_explicit_coersions := true;
     insert_scaling :=  true;
     reduce_sizeof :=  true;
     reduce_assign_ops :=  true;
     multi_file_mode :=  false;
     local_externs_ok := true)

  fun sourceToSourceMode () =
    (insert_explicit_coersions := false;
     insert_scaling :=  false;
     reduce_sizeof :=  false;
     reduce_assign_ops :=  false;
     multi_file_mode :=  false;
     local_externs_ok := true)

  val _ = sourceToSourceMode()   (* default is sourceToSource mode *)

  val perform_type_checking = TypeCheckControl.perform_type_checking
      (* true = do type checking; false = disable type checking;
	 Note: with type checking off, there is still some
	       rudimentary type processing, but no
	       usual unary conversions, usual binary conversions, etc. *)

  val undeclared_id_error = TypeCheckControl.undeclared_id_error
      (* In ANSI C, an undeclared id is an error;
	 in older versions of C, undeclared ids are assumed integer.
	 Default value: true (for ANSI behavior) *)
  val convert_function_args_to_pointers =
        TypeCheckControl.convert_function_args_to_pointers
      (* In ANSI C, arguments of functions goverened by prototype
	 definitions that have type function or array are not
	 promoted to pointer type; however many compilers do this
	 promotion.
	 Default value: true (to get standard behavior) *)
  val storage_size_check = TypeCheckControl.storage_size_check
      (* Declarations and structure fields must have known storage
	 size; maybe you want to turn this check off?
	 Default value: true (to get ANSI behavior). *)

  val allow_non_constant_local_initializer_lists = TypeCheckControl.allow_non_constant_local_initializer_lists 
                  (* Allow non constant local inializers for aggregates and unions.
                      e.g. int x, y, z;
                           int a[] = {x, y, z};
                     This is allowed gcc *)

  val (repeated_declarations_ok, resolve_anonymous_structs) =
    if !multi_file_mode then (true, true) else (false, false)

  fun debugPrBinding (name: string, binding: B.symBinding) =
      (print ("symbol binding: " ^ name ^ 
       (case binding
	 of B.MEMBER _ => " MEMBER"
	  | B.TAG _ => " TAG"
	  | B.TYPEDEF _ => " TYPEDEF"
	  | B.ID _ => " ID")
       ^ "\n"))


  (* some auxiliary functions *)
  (* ---------------------- *)
  fun toId tid = ".anon" ^ (Tid.toString tid)

  fun dt2ct {qualifiers,specifiers,storage} =
      {qualifiers=qualifiers,specifiers=specifiers}

  fun signedNum ik = Ast.Numeric (Ast.NONSATURATE,Ast.WHOLENUM,Ast.SIGNED,ik,Ast.SIGNASSUMED)
  fun unsignedNum ik = Ast.Numeric (Ast.NONSATURATE,Ast.WHOLENUM,Ast.UNSIGNED,ik,Ast.SIGNASSUMED)
  val stdInt = TypeUtil.stdInt

  fun getBindingLoc(B.MEMBER{location,...}) = location
    | getBindingLoc(B.ID{location,...}) = location
    | getBindingLoc(B.TYPEDEF{location,...}) = location
    | getBindingLoc(B.TAG{location,...}) = location


  val bogusTid = Tid.new()
  val bogusUid = Pid.new()
  fun bogusMember sym =
      {name = sym, uid = Pid.new(), location = SourceMap.UNKNOWN,
       ctype = Ast.Error, kind = Ast.STRUCTmem}  (* dbm: is this kind ok? *)

  fun isZeroExp(Ast.EXPR (Ast.IntConst 0, _, _)) = true
    | isZeroExp _ = false

  fun isZeroCoreExp(Ast.IntConst 0) = true
    | isZeroCoreExp _ = false

  fun getCoreExpr(Ast.EXPR (expr, _, _)) = expr

  (*  check if a parse-tree type is of the `tagged' variety - i.e. it
   *  refers to a (struct, union, or enum) type defined elsewhere *)
  fun isTagTy ({specifiers,...}: PT.decltype) =
      let fun sTest (PT.StructTag _) = true
	    | sTest (PT.EnumTag _) = true
	    | sTest _ = false
       in List.exists sTest specifiers
      end

local open Bindings in
(* main function *)
fun makeAst (sizes: Sizes.sizes, stateInfo: S.stateInfo,
	     errorState : Error.errorState) =
let

  (* if there are any parse errors, then don't print any type-checking errors *)
  val _ = if Error.errorCount errorState > 0 
          then (Error.noMoreErrors errorState; Error.noMoreWarnings errorState)
	  else ()

  val globalState as {uidTables={ttab,atab,implicits},...} =
      S.initGlobal(stateInfo, errorState)

  val localState = S.initLocal ()

  val stateFuns = S.stateFuns(globalState, localState)

  val {locFuns =
	{pushLoc, popLoc, getLoc, error, warn},
       tidsFuns =
	{pushTids, resetTids},
       tmpVarsFuns =
	{pushTmpVars, resetTmpVars},
       envFuns =
	{topLevel, pushLocalEnv, popLocalEnv, lookSym, bindSym,
	 lookSymGlobal, bindSymGlobal, lookLocalScope, getGlobalEnv},
       uidTabFuns =
	{bindAid, lookAid=lookAid0, bindTid, lookTid},
       funFuns =
	{newFunction, getReturnTy, checkLabels, addLabel, addGoto}, 
       switchFuns =
	{pushSwitchLabels, popSwitchLabels, addSwitchLabel, addDefaultLabel},
       ...}
      = stateFuns

  val bug = Error.bug errorState
  fun convFunError s _ =
      raise Fail("Fatal Bug: extension conversion function " ^ s ^ " not installed yet!")


  (* refs for extension conversion functions *)
  val refCNVExp = ref(convFunError "CNVExp" : CnvExt.expressionExt -> Ast.ctype * Ast.expression)
  val refCNVStat = ref(convFunError "CNVStat": CnvExt.statementExt -> Ast.statement)
  val refCNVBinop = ref(convFunError "CNVBinop": {binop: ParseTreeExt.operatorExt, arg1Expr: ParseTree.expression,
						  arg2Expr: ParseTree.expression}
			                        -> Ast.ctype * Ast.expression)
  val refCNVUnop = ref(convFunError "CNVUnop": {unop: ParseTreeExt.operatorExt, argExpr: ParseTree.expression}
                                                -> Ast.ctype * Ast.expression)
  val refCNVExternalDecl = ref(convFunError "CNVExternalDecl" : CnvExt.externalDeclExt -> Ast.externalDecl list)
  val refCNVSpecifier = ref(convFunError "CNVSpecifier": {isShadow: bool, rest : ParseTree.specifier list} 
			                                -> CnvExt.specifierExt
			                                -> Ast.ctype)
  val refCNVDeclarator = ref(convFunError "CNVDeclarator":  Ast.ctype * CnvExt.declaratorExt 
			                                 -> Ast.ctype * string option)
  val refCNVDeclaration = ref(convFunError "CNVDeclaration": CnvExt.declarationExt -> Ast.declaration list)

  fun CNVExp x = !refCNVExp x
  fun CNVStat x = !refCNVStat x
  fun CNVBinop x = !refCNVBinop x 
  fun CNVUnop x = !refCNVUnop x
  fun CNVExternalDecl x = !refCNVExternalDecl x
  fun CNVSpecifier x = !refCNVSpecifier x
  fun CNVDeclarator x = !refCNVDeclarator x
  fun CNVDeclaration x = !refCNVDeclaration x

 (* miscellaneous utility functions *)

  (* could be a component of stateFuns *)
  (* indicates a type used before it is defined: structs, unions, enums *)
  (* should never happen for tid bound to a typedef *)
  fun isPartial tid = 
      case lookTid tid
	of SOME{ntype=NONE,...} => true
	 | _ => false

  fun isPartialTy(Ast.StructRef tid | Ast.UnionRef tid) = isPartial tid
    | isPartialTy _ = false


  fun isLocalScope sym = isSome(lookLocalScope sym)

  (* redefine lookAid with error recovery behavior *)
  fun lookAid aid =
      case lookAid0 aid
	of NONE =>
	    (bug ("lookAid: no type for this expression."
			^ Int.toString aid);
	     Ast.Void)
	 | SOME ct => ct

  (* pretty-printer utils *)  (* DBM: not used *)
  fun ppCt () =
      PPL.ppToStrm (PPAst.ppCtype () ttab) TextIO.stdOut

  val ctToString = PPL.ppToString (PPAst.ppCtype () ttab)

  (* identifier convention: loc : Errors.location *)

  val isPointer = TU.isPointer ttab
  val isFunction = TU.isFunction ttab  (* is real function type; excludes pointer to function *)
  val isNonPointerFunction = TU.isNonPointerFunction ttab
  val isNumberOrPointer = TU.isNumberOrPointer ttab
  val isNumber = TU.isNumber ttab
  val isArray = TU.isArray ttab
  fun deref v = 
      (case (TU.deref ttab v)
	 of SOME x => x
	  | NONE => (error
		       ("Cannot dereference type " ^ (ctToString v));
		     Ast.Void))

  val getFunction = TU.getFunction ttab
  val isStructOrUnion= TU.isStructOrUnion ttab
  val isEnum = TU.isEnum ttab
  fun lookupEnum v = 
      (case (TU.lookupEnum ttab v)
	 of SOME x => x
	  | NONE => (bug "lookupEnum: invalid enum type";
		     LargeInt.fromInt 0))

  val equalType = TU.equalType ttab
  val isScalar = TU.isScalar ttab
  val isIntegral = TU.isIntegral ttab
  val usualUnaryCnv = TU.usualUnaryCnv ttab
  val usualBinaryCnv = TU.usualBinaryCnv ttab
  val isConst = TU.isConst ttab
  val isEquable = TU.isEquable ttab
  val isAddable = TU.isAddable ttab
  val isSubtractable = TU.isSubtractable ttab
  val isComparable = TU.isComparable ttab
  val conditionalExp = TU.conditionalExp ttab
  val compatible = TU.compatible ttab
  val functionArgConv = TU.functionArgConv ttab
  val isFunctionPrototype = TU.isFunctionPrototype ttab
  val getCoreType = TU.getCoreType ttab

  fun composite (ty1, ty2) =
      case TU.composite ttab (ty1, ty2)
	of (res, nil) => res
	 | (res, errL) => 
	     (List.map error errL;
	      res)

  val hasKnownStorageSize = TU.hasKnownStorageSize ttab
  val preArgConv = TU.preArgConv ttab
  val cnvFunctionToPointer2Function = TU.cnvFunctionToPointer2Function ttab

  fun checkQuals ty = TU.checkQuals ttab ty

  fun wrapSTMT(coreStmt: Ast.coreStatement) : Ast.statement =
      Ast.STMT (coreStmt, Aid.new (), getLoc())

  fun wrapDECL(coreExtDecl: Ast.coreExternalDecl) : Ast.externalDecl =
      Ast.DECL(coreExtDecl, Aid.new (), getLoc())

  fun wrapEXPR (ty, coreExpr) = 
    let val ty = cnvFunctionToPointer2Function ty
      (* all expressions of type Function are promoted to Pointer(Function)
       * exceptions (&, sizeof) are handled in unops *)
      (* Strictly speaking, arrays should also be converted to pointers here;
         however code using array expressions deal with the array case directly (e.g. Sub, Deref);
         Caution: if we were to make this change, we still need to know it was an array!
         Where is the right place to do this conversion? *)
      val adorn = bindAid ty
    in (ty, Ast.EXPR (coreExpr, adorn, getLoc()))
    end

  val simplifyAssignOps = SimplifyAssignOps.simplifyAssignOps
                            {lookAid=lookAid, getCoreType=getCoreType, wrapEXPR=wrapEXPR,
			     getLoc=getLoc, topLevel=topLevel, bindSym=bindSym, pushTmpVars=pushTmpVars}

  fun mkFunctionCt (retTy, argTys) =
      (if isNonPointerFunction retTy
	   then error "Return type of function cannot be function type."
       else ();
       if isArray retTy
	   then error "Return type of function cannot be array type."
       else ();
       let fun withName f (t, n) = (f t, n)
	   val argTys = 
	   if convert_function_args_to_pointers then
	       List.map (withName preArgConv) argTys
	   else List.map (withName cnvFunctionToPointer2Function) argTys
        in 
	  Ast.Function(retTy, argTys)
       end)

  fun getStorageClass sym =
      case lookSym sym
	of SOME(B.ID{stClass,...}) => SOME stClass
	 | _ => NONE

  fun checkFn (funTy,argTys,exprs) = 
    let val isZeroExprs = List.map isZeroExp exprs
    in
      case TU.checkFn ttab (funTy, argTys, isZeroExprs)
	of (res, nil, args) => (res, args)
	 | (res, errL, args) => 
	     (List.map error errL;
	      (res, args))
    end

  (* DBM: should this go in State? or be defined in terms of a more 
   * primitive operation in State like the former insertOpAid? *)
  fun noteImplicitConversion (Ast.EXPR (_, aid, _), ty) = AT.insert(implicits,aid,ty)

  fun wrapCast (ty, expr as (Ast.EXPR(_, aid', loc'))) =
      if CTypeEq.eqCType(getCoreType(lookAid aid'), getCoreType ty) then expr  (* DBM: gen. equality on types *)
      (* 7/29/99: tentative fix for spurious casts
        old code: if lookAid aid' = ty then expr  (* DBM: gen. equality on types *)
       *)
      else let
	     val aid = bindAid ty
	    in 
	      if !insert_explicit_coersions then
		Ast.EXPR(Ast.Cast(ty, expr), aid, loc')
	      else
		(noteImplicitConversion(expr, ty);
		 expr)
	   end

  fun sizeof ty = 
      LargeInt.fromInt (#bytes (Sizeof.byteSizeOf {sizes=sizes, err=error, warn=warn, bug=bug} ttab ty))

  fun isLval (expr, ty) = 
      case expr
	of Ast.Member(Ast.EXPR (expr'', aid, _), _) => 
	    isLval (expr'', lookAid aid)
	 | (Ast.Id _ | Ast.Sub _ | Ast.Arrow _ | Ast.Deref _) => true
	 | _ => false

  fun checkAssignableLval (expr, ty, s) =
      (* check we can assign to this expression,
       * and generate error messages if not *)
    if isLval (expr, ty) then
      if isConst ty then 
	error 
	  ("Type Error: lhs of assignment is const"
	   ^ (if s = "" then "." else (" in " ^ s ^ ".")))
      else (case expr
	      of Ast.Id _ => 
		   if isArray ty then 
		     error
		       ("Type Error: lhs of assignment is an array (not a modifiable lval)"
			^ (if s = "" then "." else (" " ^ s ^ ".")))
		   else ()
	       | _ => ())
    else
      error
        ("Type Error: lhs of assignment is not an lvalue"
	 ^ (if s = "" then "." else (" " ^ s ^ ".")))

  fun isAssignableTys {lhsTy, rhsTy, rhsExprOpt : Ast.coreExpression option} =
      let val rhsExpr0 = (case rhsExprOpt
			    of SOME rhsExpr => isZeroCoreExp rhsExpr
			     | NONE => false)
       in TU.isAssignable ttab {lhs=lhsTy, rhs=rhsTy, rhsExpr0=rhsExpr0}
      end

  fun checkAssignableTys (x as {lhsTy, rhsTy, rhsExprOpt}) =
      if not(isAssignableTys x) then
	let val lhs = ctToString lhsTy
	    val rhs' = ctToString (usualUnaryCnv rhsTy)
	    val rhs = ctToString rhsTy
	 in error 
	     ("Type Error: rval of type " ^ rhs
	      ^ " cannot be assigned to lval of type " ^ lhs ^ ".")
	end
      else ()
		     
  fun checkAssign {lhsTy,lhsExpr,rhsTy,rhsExprOpt : Ast.coreExpression option} =
      if perform_type_checking then 
	(checkAssignableLval(lhsExpr, lhsTy, "");
	 checkAssignableTys {lhsTy=lhsTy,rhsTy=rhsTy,rhsExprOpt=rhsExprOpt})
      else ()

  fun isTYPEDEF({storage,...} : PT.decltype) =
      if List.exists (fn PT.TYPEDEF => true | _ => false) storage  (* any typedefs? *)
	  then (case storage of
		    [PT.TYPEDEF] => true  (* must be exactly one typedef *)
		  | _ => (error "illegal use of TYPEDEF";  
			  true))
      else false
	  
  fun declExprToDecl errorStr (decr, PT.EmptyExpr) = decr
    | declExprToDecl errorStr (decr, _) = (error errorStr; decr)

  (* checks for illegal rebinding within current local scope, for other
   * than objects and functions *)
  fun checkNonIdRebinding (sym, ty, kind: string) : unit =
      case lookLocalScope sym
	of SOME(B.TYPEDEF{location=loc, ...}) =>
	    (error ("illegal redeclaration of " ^ kind ^ (Sym.name sym) ^
		    ";\n   previously declared as typedef at " ^
		    SM.locToString loc))
	 | SOME(B.MEMBER{location=loc, ...}) =>
	    (error ("illegal redeclaration of " ^ kind ^ (Sym.name sym) ^
		    ";\n   previously declared as member at " ^
		    SM.locToString loc))
	 | SOME(B.TAG{location=loc, ...}) =>
	    (error ("illegal redeclaration of " ^ kind ^ (Sym.name sym) ^
		    ";\n   previously declared as tag at " ^
		    SM.locToString loc))
	 | NONE => () (* not previously bound in local scope *)
	 | _ => bug "checkNonIdRebinding: unexpected binding"


  (* checks for illegal rebinding within current local scope
   * only called in processDecr for "object" declaration *)
  fun checkIdRebinding (sym, newTy, newStatus: Ast.declStatus, {globalBinding}) : Ast.declStatus * Ast.ctype * (Pid.uid option) =
      case (if globalBinding then lookSymGlobal sym else lookLocalScope sym)
	of SOME (B.ID{status=oldStatus,kind,location,ctype=oldTy,uid, ...}) =>
	   if globalBinding orelse topLevel()
	       then let val status =
		            case (newStatus, oldStatus)
			      of (Ast.DEFINED,Ast.DEFINED) =>
				 (error
				   (case kind
				      of Ast.FUNCTION _ =>
				          ("illegal redefinition of identifier "
				           ^ (Sym.name sym) ^
				           ";\n   previously defined as function at " ^
					   SM.locToString location)
				       | Ast.NONFUN =>
				          ("illegal redefinition of identifier "
				           ^ (Sym.name sym) ^
				           ";\n   previously declared with initializer at " ^
					   SM.locToString location));
				  Ast.DEFINED)
			       | (Ast.DEFINED,_) => Ast.DEFINED
			       | (_,Ast.DEFINED) => Ast.DEFINED
			       | (Ast.DECLARED,_) => Ast.DECLARED
			       | (_,Ast.DECLARED) => Ast.DECLARED
			       | _ => Ast.IMPLICIT
			val ty =
			    case kind
			      of Ast.FUNCTION _ =>
				if equalType (newTy, oldTy) then oldTy
				else
				 (case composite(newTy,oldTy)
				    of SOME ty => ty
				     | NONE =>
					(error
					 ("illegal redeclaration of function " ^
					  (Sym.name sym) ^
					  " has type incompatible with previous " ^
					  "declaration at " ^
					  SM.locToString location);
					 newTy))
			       | Ast.NONFUN =>
				  if equalType (newTy, oldTy) then oldTy
				  else
				    (case composite(newTy, oldTy)
				       of SOME ty => ty
				        | NONE =>
					 (error
					  ("illegal redeclaration of identifier "
					   ^ (Sym.name sym) ^
					   ";\n   type incompatible with previous \
					    \declaration at " ^
					   SM.locToString location);
					  newTy))
		    in (status,ty,SOME uid)
		    end
           else (* no redefinition *)
	     (error 
	       ("illegal redeclaration of "^ (Sym.name sym) ^
		" in nested scope;\n   previous declaration at " ^
		SM.locToString location);
	      (newStatus,newTy, NONE))
	 | NONE => (newStatus,newTy, NONE)  (* not previously bound in local scope *)
	 | _ => (error ((Sym.name sym)^" is not a variable");
		 (newStatus,newTy, NONE))  (* not previously bound in local scope *)


  (* code for calling initializer normalizer *)
  fun normalize(ty, expr) = 
      InitializerNormalizer.normalize{lookTid=lookTid, bindAid=bindAid,
				      initType=ty, initExpr=expr}


  (* type check initializer:
       recursively descend into type and initializer, checking as we go.
       NB 1: if type is unions and structs, then don't generate errors when initializer is simple
       NB 2: if type is array then *do* generate errors when initializer is simple  *)


  fun TCInitializer(ctype as (Ast.TypeRef _ | Ast.Qual _), expr) =
        TCInitializer(getCoreType ctype, expr)  (* the following TCInitializer cases expect coretypes *)
    | TCInitializer (Ast.Array(opt, ctype), Ast.Aggregate exprs) = 
	(case (opt, LargeInt.fromInt(List.length exprs))
	   of (NONE, _) =>
	       bug "TCInitializer: array size should be filled in by now?"
	    | (SOME(x, _), y) =>
	       if x = y then ()   (* LargeInt equality *)
	       else if x < y then
		 error "TCInitializer: badly formed array initializer: \
	                \too many initializers"
	       else error "TCInitializer: badly formed array initializer: \
	                   \not enough initializers";
	 List.app (fn e => TCInitializer(ctype, e)) exprs)
    | TCInitializer (Ast.Array _, _) =
	error "badly formed array initializer: expected {"
    | TCInitializer (Ast.StructRef tid, Ast.Aggregate exprs) =
      (case lookTid tid
	 of SOME{ntype=SOME(B.Struct(tid,fields)),...} =>
	      let fun f ((fieldType, _, _) :: l, expr :: exprs) =
			(TCInitializer(fieldType, expr);
			 f (l, exprs))
		    | f (nil, nil) = ()
		    | f (_, nil) =
			error
			  "badly formed struct initializer: not enough initializers"
		    | f (nil, _) =
			error
			  "badly formed struct initializer: too many initializers"
	       in f (fields, exprs)
	      end
	  | NONE => bug "TCInitializer: lookTid failed"
	  | _ => error "TCInitializer: ill-formed StructRef type")
    | TCInitializer (Ast.UnionRef tid, Ast.Aggregate exprs) =
      (case lookTid tid
	 of SOME{ntype=SOME(B.Union(tid,(fieldTy, _)::fields)),...} =>
	      (case exprs
	         of [expr] => TCInitializer(fieldTy, expr)
		  | _ ::  _ =>
		     error
		       "badly formed union initializer: \
		        \initializer has too many elements"
		  | nil =>
		     error "badly formed union initializer: empty initializer")
	  | SOME{ntype=SOME (B.Union(tid,_)), ...} =>
	      error "empty union"
	  | NONE => bug "TCInitializer: lookTid failed"
	  | _ => error "TCInitializer: ill-formed UnionRef type")
    | TCInitializer (ty as (Ast.StructRef _ | Ast.UnionRef _), Ast.Simple(Ast.EXPR(coreExp, aid, _))) =
	if isAssignableTys {lhsTy=ty, rhsTy=lookAid aid, rhsExprOpt=SOME coreExp} 
	  then ()
	else error "type of initializer is incompatible with type of lval"
    | TCInitializer (Ast.Pointer(Ast.Numeric (_,_,_,Ast.CHAR,_)),
		     Ast.Simple(Ast.EXPR(Ast.StringConst _, _, _))) = ()
    | TCInitializer (ty, Ast.Aggregate([Ast.Simple(Ast.EXPR(coreExp, aid, _))])) =
	if isScalar ty then
	  if isAssignableTys {lhsTy=ty, rhsTy=lookAid aid,
			      rhsExprOpt=SOME coreExp} 
	    then ()
	  else error "type of initializer is incompatible with type of lval"
	else
	  error "illegal aggregate initializer"	  
	  
    | TCInitializer (_, Ast.Aggregate _) =
	error "illegal aggregate initializer"
    | TCInitializer (ty, Ast.Simple(Ast.EXPR(coreExp, aid, _))) =
	if isAssignableTys {lhsTy=ty, rhsTy=lookAid aid,
			    rhsExprOpt=SOME coreExp} 
	    then ()
	else error "type of initializer is incompatible with type of lval"

  (* check form of initializer *)				
  fun checkInitializer (ty, initExpr, auto) =
      let 
	val initExpr' = 
	      case initExpr of
	 	Ast.Aggregate _ => if isArray ty orelse (case isStructOrUnion ty of SOME _ => true | NONE => false)
				     then normalize(ty, initExpr)
				   else initExpr
	      | Ast.Simple(Ast.EXPR(Ast.StringConst _, _, _)) => normalize(ty, initExpr)
	      | _ => initExpr
		  (* the purpose of normalize is the handle the case of strings as initializers,
                   * and to pad out curly-brace initializers
		   *)
	(*  old code: 3/10/00
	 *     case (initExpr, auto) of
	 *	(Ast.Aggregate _, _) => normalize(ty, initExpr)
	 *     | (_, false) => normalize(ty, initExpr)
	 *     | (Ast.Simple(Ast.EXPR(Ast.StringConst _, _, _)), _) => normalize(ty, initExpr)
	 *     | (_, true) => initExpr
         *)
	  val ty = case getCoreType ty
		     of Ast.Array(NONE, ctype) => 
			 (case initExpr'
			    of Ast.Aggregate inits =>
			      let val len = List.length inits
				val i = LargeInt.fromInt len
				val (_, expr) = wrapEXPR(stdInt, Ast.IntConst i)
			      in
				if len=0 then warn "Array has zero size." else ();
				Ast.Array(SOME(i, expr), ctype)
			      end
			     | _ => (error
				       "badly formed array initializer: missing \"{\"";
				     ty))
		      | _ => ty
       in TCInitializer(ty, initExpr');
	  (initExpr', ty)
    end

  (* processing declarator parse trees *)

  fun processDeclarator (typ as {qualifiers,specifiers,storage},decr) = 
      let fun vardeclToTypeNameLoc (typ as {qualifiers, specifiers},decr) =
	      let fun mkTyp spc = {qualifiers=[], specifiers=[spc]}
		  fun addQual q = {qualifiers=q::qualifiers, specifiers=specifiers}
	       in case decr
		    of PT.VarDecr x => (typ,SOME x,getLoc())
		     | PT.PointerDecr x =>
			vardeclToTypeNameLoc (mkTyp (PT.Pointer typ),x)
		     | PT.ArrayDecr (x,sz) =>
			vardeclToTypeNameLoc (mkTyp (PT.Array (sz,typ)),x)
		     | PT.FuncDecr (x,lst) =>
			vardeclToTypeNameLoc (mkTyp (PT.Function{retType=typ,params=lst}),x)
		     | PT.QualDecr (q,decr) =>
			vardeclToTypeNameLoc (addQual q, decr)
		     | PT.EmptyDecr => (typ, NONE, getLoc())
		     | PT.EllipsesDecr => (mkTyp PT.Ellipses, SOME("**ellipses**"), getLoc())
		     | PT.MARKdeclarator(loc, decr) =>
			(pushLoc loc;
			 vardeclToTypeNameLoc(typ, decr)
			 before popLoc ())
		     | PT.DecrExt _ => (typ, NONE, getLoc())  (* should call decr extension? *)
	      end
	  val ({qualifiers,specifiers},sOpt, loc) =
	      vardeclToTypeNameLoc ({qualifiers=qualifiers,
				     specifiers=specifiers},
				    decr)
       in ({qualifiers=qualifiers,specifiers=specifiers,storage=storage},sOpt, loc)
      end 

  (* processDecr : 
   *   Ast.ctype * Ast.storageClass * bool
   *   -> (ParseTree.declarator * ParseTree.expression)
   *      * ((Ast.id * Ast.expression) list)
   *   -> ((Ast.id * Ast.expression) list)
  * to be used by both external (global) decls and internal (statement
  * level - within function body) decls. 
  * After type and storage class are specified, designed to be used with
  * a fold function.
  *)

  fun cnvInitExpression(PT.InitList exprs) =
      Ast.Aggregate(map cnvInitExpression exprs)
    | cnvInitExpression(PT.MARKexpression(loc, expr)) =
      (pushLoc loc;
       cnvInitExpression expr
       before popLoc ())
    | cnvInitExpression(expr) =
      Ast.Simple(#2(cnvExpression expr))
										   
  and processDecr (ty,sc,topLevel0) (decr,expr) =
      let val (ty,varNameOpt,loc) = mungeTyDecr (ty, decr)
	  val varName = 
	      case varNameOpt
		of SOME name => name
		 | NONE =>
		    (error
		       "missing declarator in declaration - \
			\filling with <missing_declarator>.";
		     "<missing_declarator>")

	  val hasInitializer = (case expr of
				  PT.EmptyExpr => false
				| _ => true)

	  val varSym = Sym.object varName

	  val _ = if (topLevel0 = topLevel()) then ()
		  else bug "inconsistency of topLevel!"

	  val auto = case (topLevel0, sc)
		       of (true, Ast.AUTO) =>
			   (error "`auto' not allowed in top-level declarations";
			    false)
			| (true, Ast.REGISTER) =>
			   (error "`register' not allowed in top-level declarations";
			    false)
			| (true, _) => true
			| (false, Ast.EXTERN) =>
			   (if !local_externs_ok then ()
			    else error "`extern' not allowed in local declarations";
			    false)
			| (false, Ast.STATIC) => false
			| (false, _) => true
			  (* local declarations are auto unless declared static *)

         (* ISO p71: initExprs must be constant if
	    a) they are in an initilizer list for an object of aggregate or union type
            b) the object has static storage duration			      
          *)
	 (* Note: should really reduce constants arith exprs to simple constants *)
	  fun constCheck(Ast.EXPR((Ast.StringConst _ | Ast.IntConst _ | Ast.RealConst _),_,_)) = true
	    | constCheck(Ast.EXPR(Ast.QuestionColon(e1, e2, e3), _, _))
	    = constCheck e1 andalso constCheck e2 andalso constCheck e3
	    | constCheck(Ast.EXPR(Ast.Binop(_, e1, e2), _, _))
	    = constCheck e1 andalso constCheck e2
	    | constCheck(Ast.EXPR(Ast.Unop(_, e1), _, _)) = constCheck e1
	    | constCheck(Ast.EXPR(Ast.Cast(_, e1), _, _)) = constCheck e1
	    | constCheck(Ast.EXPR(Ast.EnumId _, _, _)) = true
	    | constCheck(Ast.EXPR(Ast.SizeOf _, _, _)) = true
	    | constCheck(Ast.EXPR(Ast.AddrOf _, _, _)) = true
	    | constCheck(Ast.EXPR(Ast.Id id, _, _)) = 
	    (* id must be a function or an array (note: a function pointer won't do) *)
	    let val {ctype, ...} = id
	    in
	      isFunction ctype orelse isArray ctype
	    end
	    | constCheck _ = false
	  fun constCheckIE'(Ast.Simple expr) = constCheck expr
	    | constCheckIE'(Ast.Aggregate exprl)
	    = List.foldl (fn (x, y) => (constCheckIE' x) andalso y) true exprl
	  fun constCheckIE(Ast.Simple expr) = 
	    (if topLevel0 orelse sc = Ast.STATIC orelse sc = Ast.EXTERN
	       then
		 if constCheck expr then ()
		 else error("Illegal initializer: object has static storage duration, but initializer is not constant.")
	     else if isArray ty
	       then 
		 if constCheck expr then ()
		 else error("Illegal initializer: object is an array, but initializer is not constant.")
		  else ())

	    | constCheckIE x = if allow_non_constant_local_initializer_lists orelse constCheckIE' x then ()
			       else error("Illegal initializer: initializer list elements must be constants.")

           (*** Checking initializers: from ISO p72.
	    1. if toplevel or static or extern or array then initializer must be const
	    2. case of type:
 	        scalar: initializer must be a single expression, optionally enclosed in {}
	        aggregate or union:
	        a) apply normalize
	        b) type check 
                   - but don't generate errors due to simple for unions and structs
                   - do generate errors due to simple for arrays
	    *)

	  val (id, ty) = 
	    if isFunction ty then  (* declaring (NOT defining) a function *)
		(* CHECK: sc should be either DEFAULT, or EXTERN or STATIC? *)
	      let val (status, newTy, uidOpt) =
		      checkIdRebinding(varSym, ty, Ast.DECLARED, {globalBinding=true})
		  val uid = case uidOpt of
		    SOME uid => uid
		  | NONE => Pid.new()
		  val id = {name = varSym, uid = uid, location = loc,
			    ctype = newTy, stClass = sc, status = status, global = true,
			    kind = Ast.FUNCTION{hasFunctionDef=false}}
		  val binding = ID id
	       in bindSymGlobal(varSym, binding);
		  (id, newTy) 
	      end
	    else (* not a function type *)
	      let val status = if hasInitializer then Ast.DEFINED else Ast.DECLARED
  		  val hasExtern = (case sc of Ast.EXTERN => true | _ => false)
		    (* if hasExtern then force globalization of this binding *)
		  val (status,ty,uidOpt) =
			checkIdRebinding(varSym, ty, status, {globalBinding=hasExtern})
		  val uid = case uidOpt of SOME uid => uid | NONE => Pid.new()

		  val id = {name = varSym, uid = uid, location = loc,
			    ctype = ty, stClass = sc, status = status, global = topLevel() orelse hasExtern,
			    kind = Ast.NONFUN}
		  (* always rebind, even if there was a previous binding in
		   * scope *)
	       in if hasExtern then bindSymGlobal (varSym, ID id)
		  else bindSym (varSym, ID id);
		  (id, ty)
	      end

          (* Delay processing of initializer until we've added a binding for
	     the variable.  This implements the "left-to-right" processing
	     strategy of C -- i.e. we process the declaration before we process
	     the initializer.
             This means that
                  int x=43;
                  main () {
                    int x = x+2;
                  }
             does not have its intuitive meaning (at least for functional programmers).
             In other words, initializers are not quite let statements!
	     
	     This does lead to a problem: sometimes we don't know the full type
	     of something until we've looked at the initializer 
	       e.g. int [] = {1,2,3};
             So, we might have to fix up the type!
	     *)
 
	  (* DBM: return fixed id as well, to fix Bug 19 *)
	  val (initExprOpt, ty, id) =
	      case expr
		of PT.EmptyExpr => (NONE, ty, id)
		 | _ => 
		    let 
		      val e = cnvInitExpression expr
		      val _ = constCheckIE e
		      val (e',ty') = checkInitializer(ty, e, auto)
		      val id' = 
		       if equalType(ty', ty) then id (* no fix for id required *)
		       else (* fix up type of id *)
			 (case lookSym varSym
			    of SOME(B.ID x) =>
				let val {name, uid, location, ctype, stClass,
					 status, global, kind} = x
				    val newid = {name=name, uid=uid, location=location,
						 ctype=ty', stClass=stClass,
						 status=status, global=global,
						 kind=kind}
				 in bindSym (varSym, ID newid);
				    newid
				end
			     | _ => id)  (* can never arise: id must have ID binding *)
		     in (SOME e', ty', id')
		    end

	  (* Now do storage size check: can't do it earlier, because type might
	     be incomplete, and only completed by processing the initializer. *)

	  val _ = 
	    if storage_size_check then
	      if hasKnownStorageSize ty then ()
	      else (case sc
		      of Ast.EXTERN => ()
		    | _ =>
			error
			("Storage size of `"
			 ^ Sym.name varSym
			 ^ "' is not known (e.g. incomplete type, void)"))
	    else ()

       in (id, initExprOpt)
      end


  (* processTypedef : 
   * Ast.ctype -> ParseTree.declarator -> ()
   * (storage class simply meant to discriminate between top-level (STATIC) and
   * local (AUTO))
   *)
  and processTypedef ty decr =
    if !multi_file_mode then  (* version of processTypede for multi_file_mode *)
    let
      val (ty,nameOpt,loc) = mungeTyDecr (ty, decr)
      val name = 
	  case nameOpt
	    of SOME name => name
	     | NONE => 
		(error
		  "Missing declarator in typedef - filling with missing_typedef_name";
		 "missing_typedef_name")

      val sym = Sym.typedef name

      val tidOpt =
	  (case lookLocalScope sym
	     of SOME(TYPEDEF{ctype=ty, location=loc',...}) => 
		  (case ty
		     of Ast.TypeRef tid => 
			  if repeated_declarations_ok then SOME tid
			  else (error
				  ("Redeclaration of typedef `" ^
				   (Sym.name sym) ^
				   "'; previous declaration at " ^
				   SM.locToString loc');
				NONE)
		      | _ => (error
				("Redeclaration of typedef `" ^
				 (Sym.name sym) ^
				 "'; previous declaration at " ^
				 SM.locToString loc');
			      NONE))
	      | SOME binding =>
		  (error
		     ("Redeclaration of `" ^
		      (Sym.name sym) ^
		      "' as a typedef; previous declaration at " ^
		      SM.locToString (getBindingLoc binding));
		     NONE)
	      | NONE => NONE)  (* not bound locally *)


      val tid = 
	  case tidOpt
	    of SOME tid => tid
	     | NONE => Tid.new () 	(* create a new named type id *)

      val ty' = Ast.TypeRef tid
      (* store actual typdef symbol mapped to named type id *)
      val _ = checkNonIdRebinding(sym, ty', "typedef ")

      val binding = TYPEDEF{name = sym, uid = Pid.new(), location = loc,
			    ctype = ty'}

      (* store named type id mapped to typedef in named-type table *)
     in bindSym(sym, binding);
	bindTid (tid, {name=SOME name, ntype=SOME(B.Typedef (tid,ty)),
		       global = topLevel(), location=getLoc()});
	tid
    end
    else  (* standard version of processTypedef *)
	  (* In time the two version should be combined. *)
    let val (ty,nameOpt,loc) = mungeTyDecr (ty, decr)
	val name = 
	    case nameOpt
	      of SOME name => name
	       | NONE =>
		  (error
		    "Missing declarator in typedef - filling with missing_typedef_name";
		    "missing_typedef_name")
	val sym = Sym.typedef name

	(* create a new named type id *)
	val tid = Tid.new ()
	val ty' = Ast.TypeRef tid

	val _ = checkNonIdRebinding(sym, ty', "typedef ")

	val binding = TYPEDEF{name = sym, uid = Pid.new(), location = loc,
			      ctype = ty'}

	(* store named type id mapped to typedef in named-type table *)
     in bindSym (sym, binding);
	bindTid (tid, {name=SOME name, ntype=SOME (B.Typedef (tid,ty)),
		       global = topLevel(), location=getLoc()});
	tid
    end


    (* like processDeclarator, except it munges a Ast.ctype with
     * a PT.declarator *)
  and mungeTyDecr (ty: Ast.ctype, decr : PT.declarator)
      : Ast.ctype * string option * SourceMap.location =
      case decr
	of PT.VarDecr str => (ty,SOME str,getLoc())
         | PT.PointerDecr decr => mungeTyDecr (Ast.Pointer ty, decr)
	 | PT.ArrayDecr (decr,PT.EmptyExpr) => mungeTyDecr(Ast.Array (NONE, ty), decr)
	 | PT.ArrayDecr (decr,sz) => 
	  let val (i, aexpr) = case evalExpr sz  (* cannot be EmptyExpr *)
	    of
	      (SOME i, _, aexpr, _) => (if i=0 then warn "Array has zero size." else ();
					(i, aexpr))
	    | (NONE, _, aexpr, _) => (error "Array must have constant size.";
				      (0, aexpr))
	  in
  	    mungeTyDecr(Ast.Array (SOME(i, aexpr), ty), decr)
	  end

	 | PT.FuncDecr (decr,lst) =>
	    let fun folder (dt,decr) =
		  let val (dty, argIdOpt, loc) = processDeclarator (dt, decr)
		      val (ty, sc) = cnvType (false, dty)
		      fun mkId n = { name = Sym.object n,
				     uid = Pid.new (),
				     location = loc,
				     ctype = ty,
				     stClass = sc,
				     status = Ast.DECLARED,
				     kind = Ast.NONFUN,
				     global = false }
		   in (ty, Option.map mkId argIdOpt)
		  end
		val argTys = List.map folder lst
	     in mungeTyDecr(mkFunctionCt(ty, argTys), decr) 
	    end
	 | PT.QualDecr (PT.CONST,decr) => 
	    let val ty' = Ast.Qual (Ast.CONST,ty)
		(* dpo: is this check necessary?
		 * Doesn't the 2nd call get the same info? *)
		val {redundantConst, ...} = checkQuals ty
		val {redundantConst=redundantConst', ...} = checkQuals ty'
	     in if not redundantConst andalso redundantConst'
		    then error "Duplicate `const'."
		else ();
		mungeTyDecr (ty', decr)
	    end
	 | PT.QualDecr (PT.VOLATILE,decr) => 
	    let val ty' = Ast.Qual (Ast.VOLATILE,ty)
		val {redundantVolatile, ...} = checkQuals ty
		val {redundantVolatile=redundantVolatile', ...} = checkQuals ty'
	     in if not(redundantVolatile) andalso redundantVolatile'
		   then error "Duplicate `volatile'."
		else ();
		mungeTyDecr (ty', decr)
	    end
	 | PT.EllipsesDecr => (Ast.Ellipses, SOME "**ellipses**", getLoc())
	 | PT.EmptyDecr => (ty, NONE, getLoc())
	 | PT.MARKdeclarator(loc, decr) =>
	    (pushLoc loc;
	     mungeTyDecr(ty, decr)
	     before popLoc ())
	 | PT.DecrExt ext =>
	    let val (t,n) = CNVDeclarator (ty, ext) in (t,n,getLoc()) end
 
 
  (* --------------------------------------------------------------------
   * cnvExternalDecl : ParseTree.externalDecl -> Ast.externalDecl list
   *
   * Converts a parse-tree top-level declaration into an ast top-level
   * declaration by adding the necessary symbols and types to the
   * environment and recursively converting statements of function bodies.
   * -------------------------------------------------------------------- *)

  and cnvExternalDecl (PT.ExternalDecl(PT.DeclarationExt ext)) =
      let val declarations = CNVDeclaration ext
      in
	List.map (fn x => wrapDECL(Ast.ExternalDecl x)) declarations
      end
      
    | cnvExternalDecl (PT.ExternalDecl(PT.MARKdeclaration (loc,decl))) =
      (pushLoc loc;
       cnvExternalDecl(PT.ExternalDecl decl)
       before popLoc ())

    | cnvExternalDecl (PT.ExternalDecl(PT.Declaration(dt as {qualifiers, specifiers, storage},
						      declExprs)))   : Ast.externalDecl list =
      (* The following code is almost identical to corresponding case in processDecls ...
         Any changes made here should very likely be reflected in changes to the processDecls code. *)
      if isTYPEDEF dt then
	  let val ct = {qualifiers=qualifiers, specifiers=specifiers}
	      val decls = List.map (declExprToDecl "initializers in typedef") declExprs
	  in  (* global typedefs *)
	      if List.null decls then (warn "empty typedef"; [])
	      else
		  let val ty = cnvCtype (false, ct)
		      val tidl = List.map (processTypedef ty) decls
		  in List.map (fn x => wrapDECL(Ast.ExternalDecl(Ast.TypeDecl{shadow=NONE, tid=x}))) tidl
		  end
	  end
      else  (* global variable and struct declarations *)
      let val isShadow = List.null declExprs andalso isTagTy dt
	  (* isShadow does not necessarily mean "shadows a previous definition";
	     rather, it refers to empty type declarations of the form
		     struct t;
		     enum e;
	     Of course, the real use of these declarations is 
	     for defining mutually recursive structs/unions
	     that reuse previously defined ids i.e. for shadowing....
	     Note: if we had
		     struct t x;
		   then this would not be a shadow,
		   hence the null declExprs test.
	   *)
	  val (ty,sc) = cnvType (isShadow, dt)
       in if isShadow
	    then let fun getTid (Ast.StructRef tid) = SOME({strct=true}, tid)
		       | getTid(Ast.UnionRef tid) = SOME({strct=false}, tid)
		       | getTid(Ast.Qual(_, ct)) = getTid ct  (* ignore qualifiers *)
	               | getTid _ = NONE (* don't deref typerefs *)
		 in
		   case getTid ty of
		     SOME(strct, tid) => [wrapDECL(Ast.ExternalDecl(Ast.TypeDecl{shadow=SOME strct, tid=tid}))]
		   | NONE => []
		 end
	  else
	    let val idExprs = List.map (processDecr(ty,sc,true)) declExprs
	     in List.map (fn x => wrapDECL(Ast.ExternalDecl(Ast.VarDecl x))) idExprs
	    end
      end

    | cnvExternalDecl (PT.FunctionDef {retType as {qualifiers,specifiers,storage},
				       funDecr, krParams: PT.declaration list, body}) =
      (* function definitions *)
      let
	val (funTy, tagOpt, funLoc) = processDeclarator (retType, funDecr)
	val funName = case tagOpt
		        of SOME tag => tag
		         | NONE =>
			  (bug
			     "Missing function name - \
			      \filling with missing_function_name";
			   "missing_function_name")
	val (retType, args) =
	    case funTy
	      of {specifiers=[PT.Function {retType,params}],...} => (retType, params)
	       | _ =>(error "ill-formed function declaration";
		      ({qualifiers=[],specifiers=[]}, nil))

	val retType' = cnvCtype (false,retType)

	val sc = cnvStorage storage

	(* check validity of storage class *)
	val _ = case sc
	          of Ast.DEFAULT => ()
		   | Ast.EXTERN => ()
		   | Ast.STATIC => ()
		   | _ => (error "`auto' and `register' are not allowed \
			               \in function declarations")

	val argTyIdOpts = List.map processDeclarator args
        fun unzip3((x, y, z) :: l) = 
	    let val (xl, yl, zl) = unzip3 l
	    in
              (x :: xl, y :: yl, z :: zl)
	    end
	  | unzip3 nil = (nil,nil, nil)

        fun zip3(x :: xl, y :: yl, z :: zl) = (x, y, z) :: (zip3(xl, yl, zl))
	  | zip3 _ = nil
			   
	val (argTys, argIdOpts, locs) = unzip3 argTyIdOpts

	fun noDeclType{specifiers=nil,qualifiers=nil,storage=nil} = true
	  | noDeclType _ = false

        val krParamsAdmitted = List.all noDeclType argTys  (* if true, K&R params are admitted *)

	(* enter a local scope - push a new symbol table *)
	val _ = pushLocalEnv ()

	(* insert (and convert) argument types in this symbol table *)
	(* this needs to be done left to right because the first
	 * argument could define a type used in later args *)
	val argTyScList = List.map (fn ty => cnvType(false,ty)) argTys

	(* create a (ctype * storageClass) IdMap.map *)
	val argIds' = 
	  let
	    fun iter ((SOME s) :: l) = (s :: (iter l))
	      | iter (NONE :: l) = (warn "unnamed function argument";
				    nil)
	      | iter nil = nil
	  in
	    case argTyIdOpts of
	      [({specifiers=[PT.Void], qualifiers=nil, storage=nil}, NONE, _)] => nil
	         (* special case of function definition f(void) {...} *)
	    | _ => iter argIdOpts
	  end

	(* zipped list will be size of shorter list - if one is shorter *)
	val argTyScIdLocList = zip3 (argTyScList, argIds', locs)
	fun folder ((tySc,id,loc),mp) = IdMap.insert (mp, id, (tySc,false,loc))
	    (* false component means hasn't been matched with K&R parameters spec *)
	val argMap = List.foldl folder IdMap.empty argTyScIdLocList

        (* check if krParams are ok *)
        val _ = if null krParams orelse krParamsAdmitted then ()
	        else error "mixing of K&R params and prototype style params not allowed"

	(* rectify additional types from K&R style parameters *)
	val argMap =
	  let
	    fun folder (decl,argMap) = 
	     (case decl
             of PT.MARKdeclaration(loc,decl') =>
		  (pushLoc loc;
		   folder(decl',argMap) before
		   popLoc())
	      | PT.DeclarationExt _ =>
		  (error "Declaration extensions not permitted in K&R parameter declarations";
		   argMap)
	      | PT.Declaration(decltype as {storage,...}, decrExprs) =>
		if isTYPEDEF decltype then (error "typedef in function parameter declaration";
					    argMap)
		else let val decrs = List.map (declExprToDecl "initializer in function declaration") decrExprs
			 val (ty,sc) = cnvType (false, decltype)
			 fun folder' (decr, argMap) = 
			   let val (ty, sOpt, loc) = mungeTyDecr (ty, decr)
			       val s = 
				 case sOpt
				   of SOME s =>
				       (case IdMap.find (argMap,s)
					  of NONE =>
					      (error "K&R parameter not in function's identifier list";
					       s)
					   | SOME (_,matched,_) =>
					      if matched then
						(error ("repeated K&R declaration for parameter "^ s);
						 s)
					      else s)
				    | NONE =>
				       (error "Unnamed K&R style parameter - \
					       \filling with unnamed_KR_parameter";
					"<unnamed_KR_parameter>")
			       val argMap = IdMap.insert
				             (argMap, s, ((ty,sc),true,loc))
			   in argMap
			   end
		     in List.foldl folder' argMap decrs
	        end)
	  in
	    List.foldl folder argMap krParams
	  end

	fun mapper id = 
	    let val (p, loc) = 
		  case IdMap.find (argMap, id)
		    of SOME (p,_,loc) => (p, loc)
		     | NONE => (bug "mapper: inconsistent arg map";
				((Ast.Error, Ast.DEFAULT), SM.UNKNOWN))
	     in (p, id, loc)
	    end

	val argTyScIdLocList' = List.map mapper argIds'

	fun checkStorageClass ((_,Ast.REGISTER),_, _) = ()
	  | checkStorageClass ((_,Ast.DEFAULT),_, _) = ()  (* DBM: ??? *)
	  | checkStorageClass _ =
	      error "Only valid storage class for function parameters is `register'."

	val _ = List.map checkStorageClass argTyScIdLocList'

	(* insert function name in global scope *)
	val argTys' = #1 (ListPair.unzip (#1 (unzip3 argTyScIdLocList')))

	(* insert the arguments in the local symbol table *)
	val argPids =
	    let fun bindArg ((ty,sc),name,loc) = 
		    let 
		        val ty = preArgConv ty (* array and function replaced by pointers *)
		        val sym = Sym.object name
			val kind = Ast.NONFUN
			  (* argument types cannot have function type: 
			     even if declared as function types,
			     they are treated as function pointers. *)
			val id = {name = sym, uid = Pid.new(), location = loc,
				  ctype = ty, stClass = sc, status=Ast.DECLARED,
				  kind = kind, global = false}
			val _ = case lookLocalScope sym of
				  NONE => ()
				| SOME _ => error ("Repeated function parameter " ^ (Sym.name sym))
		     in bindSym(sym, ID id);
			id
		    end
	     in List.map bindArg argTyScIdLocList'
	    end

        (* ASSERT: argument type list is null iff not a prototype style defn *)
	val funTy' = mkFunctionCt (retType',
				   if null krParams then
				       ListPair.zip (argTys', map SOME argPids)
				   else nil)
	val funSym = Sym.func funName
	val (status, newTy, uidOpt) =
	     checkIdRebinding(funSym, funTy', Ast.DEFINED, {globalBinding=true})
	val uid = case uidOpt of
	  SOME uid => uid
	| NONE => Pid.new()
	val funId = {name = funSym, uid = uid, location = funLoc,
		     ctype = funTy', stClass = sc, status = status,
		     kind = Ast.FUNCTION{hasFunctionDef = true}, global = true}
	val binding = ID funId

	val _ = bindSymGlobal(funSym, binding)
        (* note: we've already pushed a local env for the function args, so 
           we are no longer at top level -- we must use bindSymGlobal here! *)

	(* set new function context (labels and returns) *)
	val _ = newFunction retType'
	(* get new type declarations (tids) from retType and argTys *)
	val newtids = resetTids ()

	val bodyStmt = cnvStatement body
	(* note: what one might think of as an empty function body would
	 * actually be a compound statement consisting of an empty list
	 * of statements - thus all functions consist of one statement. *)

       in popLocalEnv ();
	  case checkLabels ()
	    of NONE => ()
	     | SOME (lab,loc) => 
	       Error.error(errorState, loc,
		 "Label " ^ ((Sym.name lab))
		 ^ "used but not defined.");
	  (List.map (fn x => wrapDECL(Ast.ExternalDecl(Ast.TypeDecl({shadow=NONE, tid=x})))) newtids) @
	  [wrapDECL(Ast.FunctionDef (funId, argPids, bodyStmt))]
      end

    | cnvExternalDecl (PT.MARKexternalDecl (loc,extDecl)) =
       (pushLoc loc;
	cnvExternalDecl extDecl
	before popLoc ())
    | cnvExternalDecl (PT.ExternalDeclExt extDecl) =
       CNVExternalDecl extDecl

  (* --------------------------------------------------------------------
   * cnvStatement : ParseTree.statement -> Ast.statement ternary_option
   *
   * Converts a parse-tree statement into an ast statement by adding the
   * necessary symbols and types to the environment and recursively converting
   * statements and expressions.
   *
   * A statement could be a type (or struct/union/enum) declaration which
   * only effects the environment, so return type is Ast.statement list
   * where the empty list is returned for such declarations.
   * A parse-tree statement can also be a variable declaration which 
   * declares multiple variables in which case the result will be multiple 
   * Ast statements.  All other cases will result in one Ast.statement
   * being returned.
   *
   * In the parse tree, most (in principle all) statements have their
   * locations marked by being wrapped in a MARKstatement constructor.
   * In the ast, each core statement is wrapped by a STMT constructor
   * which also contains the location in the source file from where
   * the statement came. This is reflected in the structure of the
   * function: each MARKstatement causes the marked location to pushed
   * onto the stack in the environment, the wrapped statement is
   * recursively converted, then wrapped in a STMT constructor with the
   * location; finally the location is popped off the location stack in
   * the environment.
   * -------------------------------------------------------------------- *)

  and processDecls ((PT.Decl decl) :: rest, astdecls: Ast.declaration list list)
       : Ast.declaration list * PT.statement list = 
      let fun processDeclaration (PT.Declaration(dt as {qualifiers, specifiers, ...}, declExprs)) =
	   (* The following code is almost identical to corresponding case in cnvExternalDecl    *)
	   (* but we have deal with struct definitions -- cnvExternalDecl doesn't *)
	   (* have to deal with them because makeAst' catches these at top level *)
           (* Any changes made here should very likely be reflected in changes to the cnvExternalDecl code. *)
	  if isTYPEDEF dt then
	      let val ct = {qualifiers=qualifiers, specifiers=specifiers}
		  val decrs = List.map (declExprToDecl "initializer in typedef") declExprs
	      in
		  if List.null decrs
		      then (warn "empty typedef";
			    astdecls)
		  else
		      let val ty = cnvCtype (false, ct)
			  val tidl = List.map (processTypedef ty) decrs
			  val newtids = resetTids ()
		      in (List.map (fn tid => Ast.TypeDecl{shadow=NONE, tid=tid}) tidl) ::
			  (List.map (fn tid => Ast.TypeDecl{shadow=NONE, tid=tid}) newtids) :: astdecls
		      (* note: must process declarations left to right since we
		       * could have e.g. int i=45, j = i; *)
		      end
	      end
	  else
	      let val isShadow = List.null declExprs andalso isTagTy dt
		  val (ty,sc) = cnvType (isShadow, dt)
		  (* ASSERT: null(tidsContext) *)
	          (* ASSERT: not at top level (i.e. topLevel() => false) *)
	      in if isShadow
		   then let fun getTid (Ast.StructRef tid) = SOME({strct=true}, tid)
			      | getTid(Ast.UnionRef tid) = SOME({strct=false}, tid)
			      | getTid(Ast.Qual(_, ct)) = getTid ct  (* ignore qualifiers *)
			      | getTid _ = NONE (* don't deref typerefs *)
			in
			  (case getTid ty of
			       SOME(strct, tid) => [Ast.TypeDecl{shadow=SOME strct, tid=tid}]
			     | NONE => []) ::
			  (List.map (fn tid => Ast.TypeDecl{shadow=NONE, tid=tid}) (resetTids ()))(*should always be null*)
			  :: astdecls
			end
		 else let
			val idExprs =
			  List.map (processDecr (ty,sc,false)) declExprs
			(* note: must process declarations left to right since we
			 * could have e.g. int i=45, j = i; *)
			val newtids = resetTids ()
		      in (List.map Ast.VarDecl idExprs) ::
			(List.map (fn tid => Ast.TypeDecl{shadow=NONE, tid=tid}) newtids) :: astdecls
		      (* DBM: push decl lists onto astdecls in reverse order since
		       * astdecls will be reversed before flattening *)
		      end
	      end
	| processDeclaration(PT.DeclarationExt ext) =
	      let val declarations = CNVDeclaration ext
	      in declarations :: astdecls
	      end
	| processDeclaration(PT.MARKdeclaration(newloc, decl)) =
	      (pushLoc newloc;
	       processDeclaration decl
	       before popLoc ())
      in
	  processDecls(rest, processDeclaration decl)
      end

    | processDecls((PT.MARKstatement (newloc,stmt as PT.Decl _)) :: rest,
		   astdecls) =
                (pushLoc newloc;
		 processDecls(stmt :: rest, astdecls)
		 before popLoc ())

    | processDecls((PT.MARKstatement (newloc,stmt as PT.MARKstatement _)) :: rest,
		   astdecls ) =
		 processDecls(stmt :: rest, astdecls)

    | processDecls (rest, astdecls) = (List.concat(rev astdecls), rest)

  (* cnvStatement : PT.statement -> Ast.statement *)
  and cnvStatement (stmt: PT.statement): Ast.statement =
    (case stmt
       of PT.Expr PT.EmptyExpr => wrapSTMT(Ast.Expr NONE)
	| PT.Expr e => 
	   let val (_, e') = cnvExpression e
	    in wrapSTMT(Ast.Expr(SOME e'))
	   end
	| PT.Compound stmts =>
	   (pushLocalEnv ();
	    let val (decls,rest) = processDecls(stmts,[])
		val stmts = List.map cnvStatement rest
		val newtids = resetTids ()
		val newTmps = resetTmpVars()
		val tmpdecls = List.map (fn pid => Ast.VarDecl(pid, NONE)) newTmps
		val typedecls = List.map (fn tid => Ast.TypeDecl{shadow=NONE, tid=tid}) newtids
	     in wrapSTMT(Ast.Compound(decls@tmpdecls@typedecls,stmts))
	    end
	    before popLocalEnv ())
	| PT.Decl _ =>
	    (* shouldn't occur; process decls anyway, but discard them *)
	    (error "unexpected declaration";
	     processDecls([stmt],[]);
	     (* may violate assertion topLevel() = false for processDecls *)
	     wrapSTMT(Ast.ErrorStmt))
	| PT.While (expr, stmt) =>
	   let val (exprTy, expr') = cnvExpression expr
	       val stmt = cnvStatement stmt
	    in if perform_type_checking andalso not(isScalar exprTy)
	       then error
		     "Type Error: condition of while statement is not scalar."
	       else ();
	       wrapSTMT(Ast.While (expr',stmt))
	   end
	| PT.Do (expr, stmt) =>
	   let val (exprTy, expr') = cnvExpression expr
	       val stmt = cnvStatement stmt
	    in if perform_type_checking andalso not(isScalar exprTy)
	       then error
		      "Type Error: condition of do statement is not scalar."
	       else ();
	       wrapSTMT(Ast.Do (expr',stmt))
	   end
	| PT.For (expr1,expr2,expr3,stmt) => 
	   let val expr1' = 
		   (case expr1
		      of PT.EmptyExpr => NONE
		       | _ => SOME(#2 (cnvExpression expr1)))
	       val expr2' =
		   (case expr2
		      of PT.EmptyExpr => NONE
		       | _ =>
			  let val (exprTy,expr2') = cnvExpression expr2
			  in if perform_type_checking andalso not(isScalar exprTy)
			     then error
			       "Type Error: condition of for statement is not scalar."
			     else ();				 
			     SOME expr2'
			  end)
	       val expr3' =
		   (case expr3
		      of PT.EmptyExpr => NONE
		       | _ => SOME(#2 (cnvExpression expr3)))
	       val stmt = cnvStatement stmt
	    in wrapSTMT(Ast.For (expr1',expr2',expr3',stmt))
	   end
	| PT.Labeled (s,stmt) =>
	   let val stmt = cnvStatement stmt
	       val labelSym = Sym.label s
	       val label = addLabel(labelSym, getLoc())
	    in wrapSTMT(Ast.Labeled (label, stmt))
	   end
	| PT.CaseLabel (expr, stmt) =>
	   let val n = case expr of
	     PT.EmptyExpr => (error "Non-constant case label."; 0)
	   | _ => (case evalExpr expr of  (* cannot be EmptyExpr *)
		     (SOME i, _, _, sizeofFl) =>
		       (if sizeofFl andalso not(!reduce_sizeof)
			  then warn("sizeof in case label not preserved in source-to-source mode.")
			else ();
		       i)
		   | (NONE, _, _, _) => (error "Non-constant case label."; 0))
	   in case addSwitchLabel n
	     of NONE => ()
	   | SOME msg => error msg;
	   wrapSTMT(Ast.CaseLabel (n, (cnvStatement stmt)))
	   end
	| PT.DefaultLabel stmt => 
	   let val stmt = cnvStatement stmt
	    in case addDefaultLabel ()
		 of NONE => ()
		  | SOME msg => error msg;
	       wrapSTMT(Ast.DefaultLabel (stmt))
	   end
	| PT.Goto s => 
	   let val labSym = Sym.label s
	       val label = addGoto(labSym, getLoc())
	    in wrapSTMT(Ast.Goto label)
	   end
	| PT.Break => wrapSTMT(Ast.Break)
	| PT.Continue => wrapSTMT(Ast.Continue)
	| PT.Return expr => 
	   let val (exprTy, expr') = 
		   case expr
		     of PT.EmptyExpr => (Ast.Void, NONE)
		      | _ => 
			 let val (ty,expr) = cnvExpression expr
			  in (ty, SOME expr)
			 end
	       val returnTy = getReturnTy ()
	       val _ = 
		 if perform_type_checking then
		   (case returnTy
		      of SOME returnTy =>
			 if isAssignableTys{lhsTy=returnTy,
					    rhsTy=exprTy,
					    rhsExprOpt=case expr'
							 of SOME expr'' =>
							     SOME(getCoreExpr expr'')
							  | NONE => NONE}
			   then ()
			 else
			   let val lhs = ctToString returnTy
			       val rhs = ctToString exprTy
			   in case expr of
				 PT.EmptyExpr => warn "missing return value."
				   (* lcc gives this a warning: check ISO standard... *)
			       | _ => error
				   (     "Type Error: returning expression has illegal type " ^ rhs
				    ^ ".\n            Function has return type " ^ lhs ^ "."
				    )
			   end
		       | NONE => ())
		 else ()
	    in wrapSTMT((Ast.Return expr'))
	   end
	| PT.IfThen (expr,stmt) =>
	   let val (exprTy, expr') = cnvExpression expr
	       val stmt = cnvStatement stmt
	    in if perform_type_checking andalso not(isScalar exprTy)
		 then error
			"Type Error: condition of if statement is not scalar."
	       else ();
	       wrapSTMT(Ast.IfThen (expr',stmt))
	   end
	| PT.IfThenElse (expr, stmt1, stmt2) =>
	   let val (exprTy, expr') = cnvExpression expr
	       val stmt1 = cnvStatement stmt1
	       val stmt2 = cnvStatement stmt2
	    in if perform_type_checking andalso not(isScalar exprTy)
		then error
		      "Type Error: condition of if statement is not scalar."
	       else ();
	       wrapSTMT(Ast.IfThenElse (expr', stmt1, stmt2))
	   end
	| PT.Switch (expr, stmt) =>
	   let val (exprTy, expr') = cnvExpression expr
	       val _ =
		 if perform_type_checking andalso not(isIntegral exprTy)
		   then error 
			 "The controlling expression of switch statement \
			  \is not of integral type."
		 else ()		   
	       val _ = pushSwitchLabels ()
	       val stmt = cnvStatement stmt
	    in popSwitchLabels ();
	       wrapSTMT(Ast.Switch(expr',stmt))
	   end
	| PT.StatExt stmt =>
	   CNVStat stmt
	| PT.MARKstatement (newloc,stmt) => 
	   (pushLoc newloc;
	    cnvStatement stmt
	    before popLoc ()))


  (* --------------------------------------------------------------------
   * cnvExpression : ParseTree.expression -> Ast.ctype * Ast.expression
   *
   * Converts a parse-tree expression into an ast expression by
   * recursively converting subexpressions. 
   *
   * In the ast, each core statement is wrapped by an EXPR constructor
   * which also contains the nearest marked location in the source file
   * from which the expression came. This is reflected in the structure
   * of the function: each parse-tree expression is converted into an ast
   * core expression and then wrapped in EXPR along with the current
   * location indicated by the environment and a unique
   * adornment. Subsequently each ast expression can be referred to by
   * its adornment. Along the way, the type of each expression is
   * calculated and stored in the environment in a map from expression
   * adornments to types. 
   * 
   * The fact that types are computed for each expression does _not_ mean
   * that this is a type checker. The bare minimum type checking is done
   * to allow for the expression-adornment-type map to be built.  (* DBM ??? *)
   * -------------------------------------------------------------------- *)

  and cnvExpression expr = 
    let
      fun numberOrPointer (ty, s) =
	  if isNumberOrPointer ty then ()
	  else error ("Type Error: operand of " ^ s ^
		      " must be a number or a pointer.")

      fun number (ty, s) =
	  if isNumber ty then ()
	  else error("Type Error: operand of " ^ s ^ " must be a number.")

      fun mkBinopExp((ty1, ty2, resTy), expr1, expr2, binop) = 
	let val resTy = getCoreType resTy
	in
	  wrapEXPR(resTy, Ast.Binop (binop, wrapCast(ty1, expr1), wrapCast(ty2, expr2)))
	end
      
      fun mkUnopExp((ty, resTy), expr, unop) = 
	let val resTy = getCoreType resTy
	in
	  wrapEXPR(resTy, Ast.Unop (unop, wrapCast(ty, expr)))
	end
      
      fun mkBinaryAssignOpExp((newTy1, newTy2, resTy), ty1, expr1, ty2, expr2, assignOp, simpleOp) =
	let val _ = checkAssign {lhsTy=ty1, lhsExpr=getCoreExpr expr1, rhsTy=resTy, rhsExprOpt=NONE}
	  fun getTy(Ast.EXPR(_, adorn, _)) = getCoreType(lookAid adorn)
	in 
	  if !reduce_assign_ops then
	    simplifyAssignOps(processBinop, simpleOp, {preOp=true}, expr1, expr2)
	  else
	    (if CTypeEq.eqCType(getTy expr1, getCoreType newTy1) then ()
	     else noteImplicitConversion(expr1, newTy1);
	     if CTypeEq.eqCType(getTy expr2, getCoreType newTy2) then ()
	     else noteImplicitConversion(expr2, newTy2);
	     mkBinopExp((ty1, ty2, ty1), expr1, expr2, assignOp)) (* result type is (getCoreType ty1) *)
	end
      
      and mkUnaryAssignOpExp((newTy1, newTy2, resTy), ty1, expr1, preOp, assignOp, simpleOp) =
	let 
	  val (oneTy, one) = wrapEXPR(stdInt, Ast.IntConst 1)  (* implicit one constant
                                                                -- all unaryassignops use one *)
          val expr2 = one
          val ty2 = oneTy
	  val _ = checkAssign {lhsTy=ty1, lhsExpr=getCoreExpr expr1, rhsTy=resTy, rhsExprOpt=NONE}
	in
	  if !reduce_assign_ops then
	    simplifyAssignOps(processBinop, simpleOp, preOp, expr1, expr2)
	  else
	    mkUnopExp((ty1, ty1), expr1, assignOp) (* result type is (getCoreType ty1) *)
	end
      
      and scaleExpr (size: LargeInt.int, expr as Ast.EXPR(_, adorn, _)) =
	let 
	  val ty1 = lookAid adorn
	  val expr1 = expr
	  val ty2 = stdInt
	  val (_, expr2) = wrapEXPR(ty2, Ast.IntConst size)
	in
	  processBinop(ty1, expr1, ty2, expr2, PT.Times)
	end

      and scalePlus(ty1, expr1, ty2, expr2) = (* scale integer added to pointer *)
	case (!insert_scaling, isPointer ty1, isPointer ty2) of
	  (true, true, false) => let val (ty2, expr2) = scaleExpr(sizeof(deref ty1), expr2)
			   in
			     (ty1, expr1, ty2, expr2)
			   end
	| (true, false, true) => let val (ty1, expr1) = scaleExpr(sizeof(deref ty2), expr1)
			   in
			     (ty1, expr1, ty2, expr2)
			   end
	| _ => (ty1, expr1, ty2, expr2) (* no change *)

      and scaleMinus(ty1, ty2, expr2) = (* scale integer subtracted from pointer *)
	case (!insert_scaling, isPointer ty1, isPointer ty2) of
	  (true, true, false) => let val (ty2, expr2) = scaleExpr(sizeof(deref ty1), expr2)
			   in
			     (ty2, expr2)
			   end
	| _ => (ty2, expr2) (* no change *)

      and plusOp (ty1, ty2) =  (* type check plus *)
	if perform_type_checking then
	  (case isAddable {ty1=ty1, ty2=ty2}
	     of SOME{ty1, ty2, resTy} => (ty1, ty2, resTy)
	   | NONE => (error
		      "Type Error: Unacceptable operands of \"+\" or \"++\".";
		      (ty1, ty2, ty1)))
	else
	  (ty1, ty2, ty1)
	  
      and minusOp (ty1, ty2) =
	if perform_type_checking then
	  (case isSubtractable {ty1=ty1, ty2=ty2} of
	     SOME{ty1, ty2, resTy} => (ty1, ty2, resTy)
	   | NONE => (error 
		      "Type Error: Unacceptable operands of \"-\" or \"--\".";
		      (ty1, ty2, ty1)))
	else
	  (ty1, ty2, ty1)
	  
      and processBinop(ty1, expr1, ty2, expr2, expop) =
	let
	  fun eqOp(ty1, exp1, ty2, exp2) = (* see H&S p208 *)
	    if perform_type_checking then
	      (case isEquable {ty1=ty1, exp1Zero=isZeroExp exp1,
			       ty2=ty2, exp2Zero=isZeroExp exp2}
		 of SOME ty => (ty, ty, signedNum Ast.INT)
	       | NONE =>
		   (error
		    "Type Error: bad types for arguments of eq/neq operator.";
		    (ty1, ty2, signedNum Ast.INT)))
	    else (ty1, ty2, signedNum Ast.INT)
	      
	  fun comparisonOp(ty1, ty2) = (* see H&S p208 *)
	    if perform_type_checking then
	      (case isComparable {ty1=ty1, ty2=ty2} of 
		 SOME ty => (ty, ty, signedNum Ast.INT)
	       | NONE => (error
			  "Type Error: bad types for arguments of \
			   \comparison operator.";
			  (ty1, ty2, signedNum Ast.INT)))
	    else (ty1, ty2, signedNum Ast.INT)
	      
	  fun logicalOp2(ty1, ty2) =  (* And and Or *)
	    let val stdInt = signedNum Ast.INT
	    in if perform_type_checking then
	      if isNumberOrPointer ty1
		andalso isNumberOrPointer ty2
		then (stdInt, stdInt, stdInt)
	      else
		(error
		 "Type Error: Unacceptable argument of logical operator.";
		 (ty1, ty2, signedNum Ast.INT))
	       else (ty1, ty2, signedNum Ast.INT)
	    end
	  
	  fun integralOp(ty1, ty2) =
	    if perform_type_checking then
	      if isIntegral ty1 andalso isIntegral ty2
		then (case usualBinaryCnv (ty1, ty2) of
			SOME ty => (ty, ty, ty)
		      | NONE => 
			  (bug "cnvExpression: integralOp.";
			   (ty1, ty2, signedNum Ast.INT)))
	      else
		(error
		 "Type Error: arguments of mod, shift and \
		  \bitwise operators must be integral numbers.";
		 (ty1, ty2, signedNum Ast.INT))
	    else (ty1, ty2, signedNum Ast.INT)
	      
	  fun mulDivOp(ty1, ty2) =
	    if perform_type_checking then
	      if isNumber ty1
		andalso isNumber ty2
		then (case usualBinaryCnv (ty1, ty2) of
			SOME ty => (ty, ty, ty)
		      | NONE => 
			  (bug
			   "usualBinaryCnv should \
			    \succeed for numeric types.";
			   (ty1, ty2, signedNum Ast.INT)))
	      else
		(error 
		 "Type Error: arguments of mul and div must be numbers.";
		 (ty1, ty2, signedNum Ast.INT))
	    else
	      (ty1, ty2, ty1)
	      
	in case expop
	  of PT.Plus => 
	    let val (ty1, expr1, ty2, expr2) = scalePlus(ty1, expr1, ty2, expr2)
	      val resTy = plusOp(ty1, ty2)
	    in
	      mkBinopExp(resTy, expr1, expr2, Ast.Plus)
	    end
	| PT.Minus => 
	    let val (ty2, expr2) = scaleMinus(ty1, ty2, expr2)
	      val resTy = minusOp(ty1, ty2)
	    in
	      mkBinopExp(resTy, expr1, expr2, Ast.Minus)
	    end
	| PT.Times => mkBinopExp(mulDivOp(ty1, ty2), expr1, expr2, Ast.Times)
	| PT.Divide => mkBinopExp(mulDivOp(ty1, ty2), expr1, expr2, Ast.Divide)
	| PT.Mod => mkBinopExp(integralOp(ty1, ty2), expr1, expr2, Ast.Mod)
	| PT.Eq => mkBinopExp(eqOp(ty1, expr1, ty2, expr2), expr1, expr2, Ast.Eq)
	| PT.Neq => mkBinopExp(eqOp(ty1, expr1, ty2, expr2), expr1, expr2, Ast.Neq)
	| PT.Gt => mkBinopExp(comparisonOp(ty1, ty2), expr1, expr2, Ast.Gt)
	| PT.Lt => mkBinopExp(comparisonOp(ty1, ty2), expr1, expr2, Ast.Lt)
	| PT.Gte => mkBinopExp(comparisonOp(ty1, ty2), expr1, expr2, Ast.Gte)
	| PT.Lte => mkBinopExp(comparisonOp(ty1, ty2), expr1, expr2, Ast.Lte)
	| PT.And => mkBinopExp(logicalOp2(ty1, ty2), expr1, expr2, Ast.And)
	| PT.Or => mkBinopExp(logicalOp2(ty1, ty2), expr1, expr2, Ast.Or)
	| PT.BitOr => mkBinopExp(integralOp(ty1, ty2), expr1, expr2, Ast.BitOr)
	| PT.BitAnd => mkBinopExp(integralOp(ty1, ty2), expr1, expr2, Ast.BitAnd)
	| PT.BitXor => mkBinopExp(integralOp(ty1, ty2), expr1, expr2, Ast.BitXor)
	| PT.Lshift => mkBinopExp(integralOp(ty1, ty2), expr1, expr2, Ast.Lshift)
	| PT.Rshift => mkBinopExp(integralOp(ty1, ty2), expr1, expr2, Ast.Rshift)
	| PT.PlusAssign => mkBinaryAssignOpExp(plusOp(ty1, ty2), ty1, expr1, ty2, expr2, Ast.PlusAssign, PT.Plus)
	| PT.MinusAssign => mkBinaryAssignOpExp(minusOp(ty1, ty2), ty1, expr1, ty2, expr2, Ast.MinusAssign, PT.Minus)
	| PT.TimesAssign => mkBinaryAssignOpExp(mulDivOp(ty1, ty2), ty1, expr1, ty2, expr2, Ast.TimesAssign, PT.Times)
	| PT.DivAssign => mkBinaryAssignOpExp(mulDivOp(ty1, ty2), ty1, expr1, ty2, expr2, Ast.DivAssign, PT.Divide)
	| PT.ModAssign => mkBinaryAssignOpExp(integralOp(ty1, ty2), ty1, expr1, ty2, expr2, Ast.ModAssign, PT.Mod)
	| PT.XorAssign => mkBinaryAssignOpExp(integralOp(ty1, ty2), ty1, expr1, ty2, expr2, Ast.XorAssign, PT.BitXor)
	| PT.OrAssign => mkBinaryAssignOpExp(integralOp(ty1, ty2), ty1, expr1, ty2, expr2, Ast.OrAssign, PT.BitOr)
	| PT.AndAssign => mkBinaryAssignOpExp(integralOp(ty1, ty2), ty1, expr1, ty2, expr2, Ast.AndAssign, PT.BitAnd)
	| PT.LshiftAssign => mkBinaryAssignOpExp(integralOp(ty1, ty2), ty1, expr1, ty2, expr2, Ast.LshiftAssign, PT.Lshift)
	| PT.RshiftAssign => mkBinaryAssignOpExp(integralOp(ty1, ty2), ty1, expr1, ty2, expr2, Ast.RshiftAssign, PT.Rshift)
	| PT.OperatorExt binop =>
	    (bug "Operator extension (binop case) should be dealt with at top level case";
	     wrapEXPR(Ast.Error, Ast.ErrorExpr))
	    
	| _ => (bug "[BuildAst.cnvExpression] \
	             \Binary operator expected.";
		wrapEXPR(Ast.Error, Ast.ErrorExpr))
	end

      fun processUnop(ty, expr, unop) = 
	let fun simpleUnOp(expop, s) =
	  let val newTy = usualUnaryCnv ty
	  in if perform_type_checking then
	    if isNumber newTy then ()
	    else error ("Type Error: operand of " ^ s ^ " must be a number.")
	   else ();
           mkUnopExp((ty, newTy), expr, expop)
	  end
	  fun logicalOp1 ty1 =  (* Not *)
	    let val stdInt = signedNum Ast.INT
	    in if perform_type_checking then
	      if isNumberOrPointer ty1
		then (stdInt, stdInt)
	      else
		(error
		 "Type Error: Unacceptable argument of logical operator.";
		 (ty1, signedNum Ast.INT))
	       else (ty1, signedNum Ast.INT)
	    end
	in
	  case unop of 
	    PT.PostInc => mkUnaryAssignOpExp(plusOp(ty, stdInt), ty, expr, {preOp=false}, Ast.PostInc, PT.Plus)
	  | PT.PreInc => mkUnaryAssignOpExp(plusOp(ty, stdInt), ty, expr, {preOp=true}, Ast.PreInc, PT.Plus)
	  | PT.PostDec => mkUnaryAssignOpExp(minusOp(ty, stdInt), ty, expr, {preOp=false}, Ast.PostDec, PT.Minus)
	  | PT.PreDec => mkUnaryAssignOpExp(minusOp(ty, stdInt), ty, expr, {preOp=true}, Ast.PreDec, PT.Minus)
	  | PT.Uplus => simpleUnOp(Ast.Uplus, "unary op +")
	  | PT.Negate => simpleUnOp(Ast.Negate, "unary op +")
	  | PT.Not => mkUnopExp(logicalOp1 ty, expr, Ast.Not)
	  | PT.BitNot => simpleUnOp(Ast.BitNot, "unary op ~")
	  | _ => (bug "BuildAst.cnvExpression \
	               \Unary operator expected";
		  wrapEXPR(Ast.Error, Ast.ErrorExpr))
	end
      
      fun cnvExpr expr = (* returns (Ast.ctype * AST.CoreExpr) *)
	(case expr 
	   of PT.EmptyExpr => 
	       (bug "cnvExpression: PT.EmptyExpr";
		wrapEXPR(Ast.Error, Ast.ErrorExpr))
                (* DBM: no more Ast.Empty_exp ??? *)
            | PT.MARKexpression(loc, expr) =>
	       (pushLoc loc;
		cnvExpression expr
		before popLoc ())
	    | PT.IntConst i =>
	       wrapEXPR(signedNum Ast.INT, Ast.IntConst i)
	    | PT.RealConst r =>
	       wrapEXPR(signedNum Ast.DOUBLE, Ast.RealConst r)
	    | PT.String s =>  
	       let val t = if (!default_signed_char) 
			   then signedNum Ast.CHAR
			   else unsignedNum Ast.CHAR
		   val ct = Ast.Pointer t
	       in wrapEXPR(ct,Ast.StringConst s) end
	    | PT.Id s => 
	       (* should id of type function be immediately converted
		* to pointer to function? *)
		(case lookSym (Sym.object s)
		   of SOME(ID(id as {ctype=ty,...})) => 
		      wrapEXPR(ty, Ast.Id id)
		    | SOME(MEMBER(member as {ctype=ty,kind,...})) => 
		      (* could it be an enum constant? *)
		      (* note: an enum const is inserted as EnumConst,
		       * but is in same namespace as Object *)
		      (case kind
			 of Ast.ENUMmem i => 
			      wrapEXPR(ty, Ast.EnumId(member,i))
		          | Ast.STRUCTmem => 
			      (error ("struct member used as id: " ^ s);
			       wrapEXPR(Ast.Error, Ast.ErrorExpr))
		          | Ast.UNIONmem => 
			      (error ("union member used as id: " ^ s);
			       wrapEXPR(Ast.Error, Ast.ErrorExpr)))
		    | NONE => (* implicit declaration *)
		      let val ty = signedNum Ast.INT
			  val sym = Sym.object s
			  val id = {name = sym, uid = Pid.new(), location = getLoc(),
				    ctype = ty, stClass = Ast.DEFAULT, status = Ast.IMPLICIT,
				    kind = Ast.NONFUN, global = topLevel()}
		       in bindSym(sym, B.ID(id(*,B.OBJ{final=false}*)));
			  (if undeclared_id_error then error else warn)
			     (s ^ " not declared");
			  wrapEXPR(ty, Ast.Id id)
		      end
		    | SOME binding =>
		      (bug ("cnvExpression: bad id binding for "^s) ;
		       debugPrBinding(s,binding);
		      wrapEXPR(Ast.Error, Ast.ErrorExpr)))

            | PT.Unop (PT.OperatorExt unop, expr) => 
		   CNVUnop {unop=unop, argExpr=expr} 
	    | PT.Unop (PT.SizeofType typeName, _) => 
	       let val ty = cnvCtype (false,typeName)
	       in if storage_size_check then 
		   if hasKnownStorageSize ty then ()
		   else error "Cannot take sizeof an expression of unknown size."
		  else ();
		  if !reduce_sizeof then
		    let val ast = Ast.IntConst(sizeof ty)
		    in wrapEXPR(Ast.Numeric (Ast.NONSATURATE,Ast.WHOLENUM,Ast.UNSIGNED,Ast.INT,Ast.SIGNASSUMED),
				ast)
		    end
		  else
		    wrapEXPR(Ast.Numeric (Ast.NONSATURATE,Ast.WHOLENUM,Ast.UNSIGNED,Ast.INT,Ast.SIGNASSUMED),
			     Ast.SizeOf ty)
	       end
	    | PT.Unop (expop, expr_parseTree) => 
	       let val (ty, expr) = cnvExpression (expr_parseTree)
                    (* ASSERT: expr_parseTree cannot be PT.EmptyExpr *)
	       in case expop
		    of PT.Sizeof =>
		      (let fun checkForFun(PT.Id s) =
			       (case lookSym (Sym.object s)
				  of SOME(B.ID{ctype=Ast.Function _,...}) =>
				       error "Cannot take sizeof a function."
				   | _ => ())
			     | checkForFun(PT.MARKexpression(loc, expr)) = checkForFun expr
			     | checkForFun _ = ()
		       in
			 checkForFun expr_parseTree
		       end;
		       if storage_size_check then 
			 if hasKnownStorageSize ty then ()
			 else error
				"Cannot take sizeof an expression of unknown size."
		       else ();
		       if !reduce_sizeof then
			 let val ast = Ast.IntConst(sizeof ty)
			 in
			   wrapEXPR(Ast.Numeric (Ast.NONSATURATE,Ast.WHOLENUM,Ast.UNSIGNED,Ast.INT,Ast.SIGNASSUMED), ast)
			 end
		       else
			 wrapEXPR(Ast.Numeric (Ast.NONSATURATE,Ast.WHOLENUM,Ast.UNSIGNED,Ast.INT,Ast.SIGNASSUMED),
				  Ast.SizeOf ty)
			 )
		     | PT.AddrOf =>
			 let val coreExpr = getCoreExpr expr
			   val ty = 
			     if isLval(coreExpr, ty) then
			       case coreExpr of
				 Ast.Id {ctype=idCtype, stClass, ...} =>
				   (if stClass = Ast.REGISTER 
				      then error "Cannot take address of register variable."
				    else ();
				    if isFunction idCtype then ty (* ty already pointer to fn *)
				    else Ast.Pointer ty)
			       | _ => Ast.Pointer ty
			     else (error
				   "Cannot take address of non-lval expression.";
				   Ast.Pointer ty)
			 in
			   wrapEXPR(ty, Ast.AddrOf expr)
			 end

(**** old code: delete in due course 
			    let fun checkId(PT.Id s) =
				 (case getStorageClass (Sym.object s)
				    of SOME Ast.REGISTER => 
					error
					  "Cannot take address of register variable."
				     | _ => ();
				  if isFunction ty then 
				    (case ty
				       of Ast.Pointer _ => wrapEXPR(ty, getCoreExpr expr)
					| _ => wrapEXPR(Ast.Pointer ty, getCoreExpr expr))
	     (* Bug fix from Satish: 2/4/99
		 It should be just "ty" in place of "Pointer ty", because we convert
		 all function types to pointer types at the end of cnvExpr, by
		 calling cnvFunctionToPointer2Function.
	      Conservative coding: above deals with case when function may
	      *not* have pointer around it.
	      *)
				  else wrapEXPR(Ast.Pointer ty, Ast.AddrOf expr))
				  | checkId(PT.MARKexpression(loc, expr)) = checkId expr
				  | checkId _ = wrapEXPR(Ast.Pointer ty, Ast.AddrOf expr)
			    in
			      checkId expr_parseTree
			    end
			  else
			    (error
			       "Cannot take address of non-lval expression.";
			     wrapEXPR(Ast.Pointer ty, Ast.AddrOf expr))
end old code ******)
		     | PT.Star => wrapEXPR(deref ty, Ast.Deref expr)
			 (* Used to explicitly squash *f, but this is incorrect.
			    Note 1: this happens automatically for type.
			       If I have *f and f has type=pointer(function),
			       then deref ty give us type=function,
			       and then wrapEXPR gives us back pointer(function).
			    Note 2: the real semantic processing of what star
			       achieves operationally is defined in simplify. *)
		     | PT.OperatorExt unop => 
			   (bug "Operator extension (unop case) should be dealt with at top level case";
			    wrapEXPR(Ast.Error, Ast.ErrorExpr))

		     | _ => processUnop(ty, expr, expop)
	       end

            | PT.Binop (PT.OperatorExt binop, expr1, expr2) => 
	          CNVBinop {binop=binop, 
			    arg1Expr=expr1,
			    arg2Expr=expr2}
	    | PT.Binop (expop, expr1, expr2) => 
	     let val (ty1, expr1') = cnvExpression (expr1)
	     in case expop
		  of PT.Dot =>
		    let
		      val s = 
			let fun getId (PT.Id str) = str
			      | getId (PT.MARKexpression(loc, expr)) = getId expr
			      | getId _ = (error "Identifier expected - filling with missing_id";
					   "<missing_id>")
			in
			  getId expr2
			end

		      val m as {ctype,...} =
			(case isStructOrUnion ty1
			   of SOME tid => 
			       let val sym = Sym.member (tid, s)
				in case lookSym sym
				     of SOME(MEMBER m) => m
				      | _ =>
					(if isPartial tid then
					   error
					    "Can't access fields in incomplete type."
					 else error ("Field " ^ s ^ " not found.");
					 (* get garbage pid to continue *)
					 bogusMember sym)
			       end
			    | NONE =>
			       (error
				  ("Field " ^ s ^
				   " not found; expression does not have structure \
				    \or union type.");
				(* get garbage pid to continue *) 
				bogusMember(Sym.member(bogusTid,"s"))))
		     in wrapEXPR(ctype, Ast.Member (expr1', m))
		    end
		| PT.Arrow =>
		    let
		      val s = 
			let fun getId (PT.Id str) = str
			      | getId (PT.MARKexpression(loc, expr)) = getId expr
			      | getId _ = (error "Identifier expected - filling with missing_id";
					   "<missing_id>")
			in
			  getId expr2
			end
		      val tyDeref = deref ty1
		      val m as ({ctype,...}: Ast.member) =
			(case isStructOrUnion tyDeref
			   of SOME tid => 
				let val sym = Sym.member (tid, s)
				 in case lookSym sym
				      of SOME(B.MEMBER m) => m
				       | NONE =>
					  (if isPartial tid then
					     error
					       "Can't access fields in incomplete type."
					   else error ("Field " ^ s ^ " not found.");
					   (* get garbage pid to continue *)
					   bogusMember sym)
				       | _ => (error (s^" is not a member");
					       bogusMember sym)
				end
			    | NONE =>
			       (error
				 ("Field " ^ s ^
				  " not found; expression does not have structure \
				    \or union type.");
				(* get garbage pid to continue *)
				bogusMember(Sym.member(bogusTid,"s"))))
		     in wrapEXPR(ctype, Ast.Arrow (expr1', m))
		    end
		| PT.Sub =>
		    let val (ty2, expr2') = cnvExpression (expr2)
			val ty = 
			    if isPointer ty1 then deref ty1
			    else if isPointer ty2 then deref ty2
			    else (error "Array/ptr expected.";
				  Ast.Error)
		     in wrapEXPR(ty, Ast.Sub (expr1', expr2'))
		    end
		| PT.Comma =>
		    let val (ty2, expr2') = cnvExpression (expr2)
		     in wrapEXPR(ty2, Ast.Comma (expr1', expr2'))
		    end
		| PT.Assign =>
		    let val (exprTy, expr2') = cnvExpression (expr2)
			val _ = checkAssign {lhsTy=ty1, lhsExpr=getCoreExpr expr1',
					     rhsTy=exprTy,
					     rhsExprOpt=SOME(getCoreExpr expr2')}
			val resultTy = getCoreType ty1
			val (expr2') = wrapCast (resultTy, expr2')
		     in wrapEXPR(resultTy, Ast.Assign (expr1', expr2'))
		        (* type of result is the unqualified type of the left
		         * operand: H&S p 221. *)
		    end
		| _ => let val (ty2, expr2') = cnvExpression (expr2)
		       in processBinop (ty1, expr1', ty2, expr2', expop)
		       end
	     end
	 | PT.QuestionColon (expr1, expr2, expr3) => 
	     let
	       val (exprTy, expr1') = cnvExpression (expr1)
	       val _ =
		 if perform_type_checking andalso not(isScalar exprTy) 
		   then error
			 "Type Error: condition of question-colon statement is not scalar."
		 else ()
	       val (ty2, expr2') = cnvExpression (expr2)
	       val (ty3, expr3') = cnvExpression (expr3)
	       val ty4 = (case conditionalExp {ty1=ty2,exp1Zero=isZeroExp expr2',
					       ty2=ty3,exp2Zero=isZeroExp expr3'}
			    of SOME ty => ty
			     | NONE => 
				(error
				   "Type Error: Unacceptable operands of question-colon.";
				 ty2))
	       val (expr2') = wrapCast (ty4, expr2')
	       val (expr3') = wrapCast (ty4, expr3')
	     in
	       wrapEXPR(ty4, Ast.QuestionColon (expr1',expr2',expr3'))
	     end
	 | PT.Call (expr, exprs) => 
	     let
	       val (funTy, expr', prototype) = 
		 let fun checkId (PT.Id s) =
			  let val funId as ({ctype=funTy,...}: Ast.id) =
				(case lookSym (Sym.func s)
				   of SOME(ID id) => id
				    | NONE => 
				      (* if ANSI C then this should be an error... *)
				      let val ty = mkFunctionCt (signedNum Ast.INT,[])
					  val varSym = Sym.object s
					  val id = {name = varSym, uid = Pid.new(),
						    location = getLoc(),status=Ast.IMPLICIT,
						    ctype = ty, stClass = Ast.EXTERN,
						    kind = Ast.FUNCTION{hasFunctionDef=false},
						    global = true} (* is is a function, so it is global! *)
					  val binding = ID id
				       in (* force insertion of symbol at top level *)
					  bindSymGlobal(varSym, binding);
					  (if Config.TypeCheckControl.undeclared_fun_error
					       then error else warn)
					    ("function " ^ s ^ " not declared");
					  id
				      end
				    | _ => (error (s^" is not a function");
					    {name = Sym.func s, uid = Pid.new(),
					     location = SourceMap.UNKNOWN,
					     ctype = Ast.Error, global = topLevel(),
					     stClass = Ast.DEFAULT, status = Ast.IMPLICIT,
					     kind = Ast.FUNCTION{hasFunctionDef=false}}))
			      val adorn = bindAid funTy
			   in (funTy, Ast.EXPR (Ast.Id funId, adorn, getLoc()),
			       isFunctionPrototype funTy)
			  end
		       | checkId(PT.MARKexpression(loc, expr)) =
			  (pushLoc loc;
			   checkId expr
			   before popLoc ())
		       | checkId _ =
			  let val (funTy, expr) = cnvExpression expr
			      val prototype = isFunctionPrototype funTy
			   in (funTy, expr, prototype)
			  end
		 in
		   checkId expr
		 end

	       val tyExprList = List.map cnvExpression exprs
	       val (argTys, exprs) = ListPair.unzip tyExprList

	       fun cnvArgs (expr :: exprs, ty :: tys) = 
		     let val (expr) = wrapCast (ty, expr)
			 val (exprs) = cnvArgs (exprs, tys)
		      in expr :: exprs
		     end
		 | cnvArgs (nil, nil) = nil
		 | cnvArgs _ =
		    (bug "type list and expression list must be same size";
		     nil)

	       val (retTy, exprs) =
		   if perform_type_checking
		   then if prototype 
			then let val (retTy, cnvArgTys) =
				     checkFn (funTy, argTys, exprs)
				 val (exprs) = cnvArgs (exprs, cnvArgTys)
			      in (retTy, exprs)
			     end
			else let val cnvArgTys = List.map (functionArgConv) argTys
				 val retTy =
				      case getFunction funTy
					of SOME(retTy,_) => retTy
					 | NONE =>
					    (error
					      "Called object is not a function.";
					     Ast.Error)
				 val (exprs) = cnvArgs (exprs, cnvArgTys)
			      in (retTy, exprs)
			     end
		   else let val retTy = case getFunction funTy
					  of SOME(retTy,_) => retTy
					   | NONE => Ast.Void
			 in (retTy, exprs)
			end
	     in
	       wrapEXPR(retTy, Ast.Call(expr', exprs))
	     end
	 | PT.Cast (ct, expr) => (* TODO: should check consistency of cast *)
	     let val ty = cnvCtype (false, ct)
		 val (_, expr') = cnvExpression expr
	      in wrapEXPR(ty, Ast.Cast (ty, expr'))
	     end
	 | PT.InitList exprs => 
	     let fun process e = #2(cnvExpression e)
		 val exprs = List.map process exprs
	     in (* PT.InitList should only occur within declarators as
		 * an aggregate initializer.  It is handled in processDecr. *)
		 bug "cnvExpression: unexpected InitList";
		 wrapEXPR(Ast.Error, Ast.ErrorExpr)
	     end

	 | PT.ExprExt expr => CNVExp expr
	 )
     in cnvExpr expr
    end

  (* --------------------------------------------------------------------
   * cnvType : bool * PT.ctype -> Ast.ctype
   *
   * Converts a parse-tree type into an ast type, adding new type and
   * symbol (e.g. enumerated values and field identifiers) into the
   * environment.
   *
   * The boolean first argument is a flag indicating if this type is a
   * `shadow' - that is a struct/enum/union tag type used to refer
   * to a future struct/union/enum declaration rather than one defined in
   * an outer scope.
   * 
   * Named types (i.e. structs/unions/enums/typedefs) are represented by
   * indexes into the named-type table. That table maps these indexes to
   * the actual struct/union/enum/typedef. This allows for for such a
   * type to be resolved without having to do multiple enquiries into the
   * symbol-table stack. By convention, an explicitly tagged type will be
   * stored redundantly in the symbol table: once as its explicit tag and
   * once as a manufactured one corresponding to the unique named type id
   * generated by Tidtab.new.
   * -------------------------------------------------------------------- *)

  and cnvCtype (isShadow: bool, ty: PT.ctype) : Ast.ctype =
      let fun cnvSpecifier specifiers =
	  let val signed = ref (NONE : Ast.signedness option)
	      val frac   = ref (NONE : Ast.fractionality option)
	      val sat    = ref (NONE : Ast.saturatedness option)
	      val kind   = ref (NONE : Ast.intKind option)
	      fun cnvSpecList (spec :: specL) =
		  (case spec 
		     of PT.Signed =>
			 (case !kind
			    of SOME (Ast.FLOAT | Ast.DOUBLE | Ast.LONGDOUBLE) => 
				 error "illegal combination of signed with float/double/long double"
			     | _ => ();
			  case !signed
			    of NONE => (signed := SOME Ast.SIGNED)
			     | SOME _ => error "Multiple signed/unsigned")
		      | PT.Unsigned =>
			 (case !kind
			    of SOME (Ast.FLOAT | Ast.DOUBLE | Ast.LONGDOUBLE) => 
				 error "illegal combination of unsigned with float/double/long double"
			     | _ => ();
			  case !signed
			    of NONE => (signed := SOME Ast.UNSIGNED)
			     | SOME _ => error "Multiple signed/unsigned")
		      | PT.Char =>
			 (case !kind
			    of NONE => (kind := SOME Ast.CHAR)
			     | SOME ct =>
				error (case ct
					 of Ast.CHAR => "duplicate char specifier"
					  | _ => "illegal use of char specifier"))
		      | PT.Short =>
			 (case !kind
			    of (NONE | SOME Ast.INT) => (kind := SOME Ast.SHORT)
			     | SOME ct =>
				error (case ct
					 of Ast.SHORT => "duplicate short specifier"
					  | _ => "illegal use of short specifier"))
		      | PT.Int =>
			 (case !kind
			    of NONE => (kind := SOME Ast.INT)
			     | SOME (Ast.SHORT | Ast.LONG | Ast.LONGLONG) => ()
			     | SOME ct =>
				error (case ct
					 of Ast.INT => "duplicate int specifier"
					  | _ => "illegal use of int specifier"))
		      | PT.Long =>
			 (case !kind
			    of NONE => (kind := SOME Ast.LONG)
			     | SOME Ast.LONG => (kind := SOME Ast.LONGLONG)
			     | SOME Ast.INT => (kind := SOME Ast.LONG)
			     | SOME ct =>
				error (case ct
					 of Ast.LONGLONG => "triplicate long specifier"
					  | _ => "illegal use of long specifier"))
		      | PT.Float =>
			 (case !signed
			    of NONE => ()
			     | SOME _ => error "illegal combination of signed/unsigned with float";
			  case !kind
			    of NONE => (kind := SOME Ast.FLOAT)
			     | SOME ct =>
				 error (case ct
					  of Ast.FLOAT => "duplicate float specifier"
					   | _ => "illegal use of float specifier"))
		      | PT.Double =>
			 (case !signed
			    of NONE => ()
			     | SOME _ => error "illegal combination of signed/unsigned with double";
			  case !kind
			    of NONE => (kind := SOME Ast.DOUBLE)
			     | SOME Ast.LONG => (kind := SOME Ast.LONGDOUBLE)
			     | SOME ct =>
				error (case ct
					 of Ast.DOUBLE => "duplicate double specifier"
					  | _ => "illegal use of double specifier"))
		      | PT.Fractional =>
			 (case !frac
			    of NONE => (frac := SOME Ast.FRACTIONAL)
			     | SOME _ => error "Multiple fractional or wholenum")
		      | PT.Wholenum =>
			 (case !frac
			    of NONE => (frac := SOME Ast.WHOLENUM)
			     | SOME _ => error "Multiple fractional or wholenum")
		      | PT.Saturate =>
			 (case !sat
			    of NONE => (sat := SOME Ast.SATURATE)
			     | SOME _ => error "Multiple saturate or nonsaturate")
		      | PT.Nonsaturate =>
			 (case !sat
			    of NONE => (sat := SOME Ast.NONSATURATE)
			     | SOME _ => error  "Multiple saturate or nonsaturate")
		      | _ => error("Illegal combination of type specifiers.");
		   cnvSpecList specL)
		| cnvSpecList [] = 
		  let val numKind = case !kind
		                      of NONE => Ast.INT
				       | SOME numKind => numKind
		      val frac = case !frac
			           of NONE => Ast.WHOLENUM
				    | SOME frac => frac
		      val (sign,decl)
                               = case (!signed, numKind) 
			           of (NONE, Ast.CHAR) => 
                                      if (!default_signed_char) 
				      then (Ast.SIGNED, Ast.SIGNASSUMED)
				      else (Ast.UNSIGNED, Ast.SIGNASSUMED)
				      (* according to H&S p115,
				       * char can be signed or unsigned *)
				    | (NONE, _) => (Ast.SIGNED, Ast.SIGNASSUMED)
				    | (SOME sign, _) => (sign, Ast.SIGNDECLARED)
		      val sat = case !sat
			          of NONE => Ast.NONSATURATE
				   | SOME sat => sat
		  in
		      Ast.Numeric (sat,frac,sign,numKind,decl)
		  end
	      fun noMore [] _  = ()
		| noMore _ err = error (err ^ " cannot be combined with a specifier.")
	  in case specifiers
	      (* singleton cases: these should appear solo *)
	       of PT.Void :: l => (noMore l "Void"; Ast.Void)
	        | PT.Ellipses :: l => (noMore l "Ellipse"; Ast.Ellipses)
		| (PT.Array (expr, ty)) :: l =>
		   let val _ = noMore l "Array"
		     val opt = case expr of
		       PT.EmptyExpr => NONE
		     | _ => (case evalExpr expr of  (* cannot be EmptyExpr *)
			       (SOME i, _, expr', _) =>
				 (if i=0 then warn "Array has zero size." else ();
				  SOME(i, expr'))
			     | (NONE, _, expr', _) => (error "Array size must be constant expression.";
						      SOME(0, expr'))) 
		     val ty' = cnvCtype (false, ty)
		   in Ast.Array (opt, ty')
		   end
		| (PT.Pointer ty) :: l =>
		   let val _ = noMore l "Pointer"
		       val ty' = cnvCtype (false, ty)
		    in Ast.Pointer ty'
		   end
		| (PT.Function {retType,params}) :: l =>
		   let val _ = noMore l "Function"
		       val retTy = cnvCtype (false, retType)
		       fun process (dt, decl) =
			   let
			       (*dpo: ignore storage class in translating type *)
			       val (dty, argIdOpt, loc) =
				   processDeclarator (dt, decl)
			       val (ty, sc) = cnvType (false, dty)
			       fun mkId n = { name = Sym.object n,
					      uid = Pid.new (),
					      location = loc,
					      ctype = ty,
					      stClass = sc,
					      status = Ast.DECLARED,
					      kind = Ast.NONFUN,
					      global = false }
						 
			   in
			       (ty, Option.map mkId argIdOpt)
			   end
		       val argTys = List.map process params
		    in mkFunctionCt (retTy, argTys)
		   end
	      
		(* ------------- Enumerated Types ---------------- 
		 * If enum tag is explicitly mentioned:
		 *   if partially defined then use that named type
		 *   identifier;
		 *   otherwise, if it has never been mentioned or if
		 *   it has been mentioned for a completely defined
		 *   type (so that this definition is new for an
		 *   inner scope) then create a new named type id
		 *   and store a reference to it in the current
		 *   symbol table.
		 * Otherwise, this is an `anonynmous' enum type: create a
		 * new named type id and store a reference to it in the
		 * current symbol table.
		 *)
	
		| (PT.Enum {tagOpt,enumerators,trailingComma}) :: l =>
		  let 
		      val _ = noMore l "Enum"
		      (* check for trailing comma warning/error *)
		      val _ =
			if trailingComma then
			  if #error Config.ParseControl.trailingCommaInEnum
			  then error "trailing comma in enum declaration"
			  else if #warning Config.ParseControl.trailingCommaInEnum
			  then warn "trailing comma in enum declaration"
			  else ()
			else ()
		      val (tid, alreadyDefined) =
			  (* alreadyDefined for multi-file analysis mode *)
			  (case tagOpt
			     of SOME tagname => 
			     let
			       val sym = Sym.tag tagname
			       val tidFlagOpt = 
				   (case lookLocalScope sym
				      of SOME(TAG{ctype=ty,location=loc',...}) => 
					 (case ty 
					    of Ast.EnumRef tid => 
						if isPartial tid
						then SOME{tid=tid, alreadyDefined=false}
						else if repeated_declarations_ok
						then SOME{tid=tid, alreadyDefined=true}
						else
						  (error
						    ("Redeclaration of enum tag `" ^ 
						     tagname ^
						     "'; previous declaration at " ^
						     SM.locToString loc');
						   NONE)
					     | _ =>
					       (error
						 ("Redeclaration of enum tag `" ^
						  tagname ^
						  "'; previous declaration was not an " ^
						  "enum tag and appeared at " ^
						  SM.locToString loc');
						NONE))
				       | NONE => NONE
				       | _ => (error (tagname^ " is not an enum tag");
					       NONE))
			     in case tidFlagOpt
				  of SOME{tid, alreadyDefined} =>
				      (tid, alreadyDefined)
				   | NONE => 
				      let val tid = Tid.new ()
					  val ty = Ast.EnumRef tid
				       in bindSym(sym, TAG{name=sym,uid=Pid.new(),
							   location=getLoc(), ctype=ty});
				          bindTid (tid, {name=tagOpt, ntype=NONE,
							 global=topLevel(), location=getLoc()});
					  (tid, false)
				      end
			     end
			      | NONE => 
				let val (tid,alreadyDefined) = 
					if !multi_file_mode andalso (topLevel ()) then
					(* in multi_file_mode, give identical top-level
					 * enums the same tid *)
					  case AnonymousStructs.findAnonStructEnum ty
					    of SOME tid => (tid,true)
					     | NONE =>
						let val tid = Tid.new ()
						 in AnonymousStructs.addAnonTid(ty, tid);
						    (tid,false)
						end	
					else		    
					  let val tid = Tid.new ()
					  (* in standard mode, allocate new tid *)
					   in (tid, false)
					  end
				in if alreadyDefined then ()
				   else bindTid (tid, {name=tagOpt, ntype=NONE,
						       global=topLevel(), location=getLoc()});
				   (tid, alreadyDefined)
				end)

		      (* add each enum value into symbol table (and evaluate it);
		         prevVal passes the enum value from one enum entry to the next
			 so that 
			    enum {e1,e2,e3=4,e4};
                         gives
			    enum {e1=0,e2=1,e3=4,e4=5};
                       *)
		      fun process prevVal nil = nil
			| process prevVal ((name,e) :: l) =
			  let val constValOpt = 
			    case e of
			      PT.EmptyExpr => NONE
			    | _ => (case evalExpr e of
				      (SOME i, _, _, sizeofFl) => 
					(if sizeofFl andalso not(!reduce_sizeof)
					   then warn("sizeof in enum value " ^
						     "not preserved in source-to-source mode.")
					 else ();
					 SOME i)
				    | (NONE, _, _, _) =>
                                        (error "Enum value must be constant expression.";
					 NONE))
			      val constVal =
				  case constValOpt
				    of SOME n => n
				     | NONE => prevVal + 1
			      val sym = Sym.enumConst name
			      val ty = Ast.EnumRef tid
			      val _ = checkNonIdRebinding(sym, ty, "enum constant ")

			      val member = {name = sym, uid = Pid.new(),
					    location = getLoc(), ctype=ty,
					    kind = Ast.ENUMmem constVal}
			      val binding = B.MEMBER member
				
			      val _ = bindSym (sym, binding)
			   in 
			      (member, constVal) :: (process constVal l)
			  end
		  in if alreadyDefined then ()
		     else 
			 let val idIntList = process (LargeInt.fromInt ~1) enumerators
			     val namedTy = B.Enum (tid,idIntList)
			  in bindTid (tid, {name=tagOpt, ntype=SOME namedTy,
					   global=topLevel(), location=getLoc()});
			     pushTids tid
			 end;
		     Ast.EnumRef tid
		  end
		      

		(* ------------- Structs and  Unions ---------------- 
		 * Very similar to rules for converting enums. *)
		| (PT.Struct {isStruct, tagOpt, members}) :: l =>
		  let val _ = noMore l "Struct"
		      val (tid, alreadyDefined) = 
			(case tagOpt
			   of SOME tagname => 
			       let
				 val sym = Sym.tag tagname
				 val tidFlagOpt = 
				   (case lookLocalScope sym
				      of SOME(TAG{ctype=ty,location=loc',...}) => 
					 (case ty
					    of (Ast.UnionRef tid | Ast.StructRef tid) => 
					       if isPartial tid
					       then SOME{tid=tid, alreadyDefined=false}
					       else if repeated_declarations_ok
					       then SOME{tid=tid, alreadyDefined=true}
					       else (error("Redeclaration of type tag `"
						       ^ tagname
						       ^ "'; previous declaration at "
						       ^ SM.locToString loc');
						     NONE)
					     | _ =>
						(error("Redeclaration of type tag `"
						   ^ tagname ^
						   "'; previous declaration was not a "
						   ^ "type tag and appeared at "
						       ^ SM.locToString loc');
						 NONE))
				       | NONE => NONE
				       | _ => (bug "cnvExpression: tag symbol 2"; NONE))
				in case tidFlagOpt
				     of SOME{tid, alreadyDefined} => 
					 (tid, alreadyDefined)
				      | NONE => (* create a partial tid *)
					 let val tid = Tid.new ()
					     val ty = if isStruct then Ast.StructRef tid
						      else Ast.UnionRef tid
					  in bindSym(sym, TAG{name=sym,uid=Pid.new(),
							      location=getLoc(),
							      ctype=ty});
					     bindTid(tid, {name=NONE, ntype=NONE,
							   global=topLevel(), location=getLoc()});
					     (tid, false)
					 end
			       end
			    | NONE => 
			      let
				val (tid,alreadyDefined) = 
				  if !multi_file_mode andalso (topLevel ()) then
				     (* in multi_file_mode, give identical top-level
				      * structs the same tid 
				      *)
				      case AnonymousStructs.findAnonStructEnum ty
					of SOME tid => (tid, true)
					 | NONE => 
					   let val tid = Tid.new ()
					   in AnonymousStructs.addAnonTid(ty, tid);
					      (tid, false)
					   end
				  else 
				    let val tid = Tid.new ()
				     in (tid,false)
				    end
			       in if alreadyDefined then ()
				  else bindTid (tid, {name=NONE, ntype=NONE,
						      global=topLevel(), location=getLoc()});
				  (tid, alreadyDefined)
			      end)

		      (* add members to symbol table, evaluate bit fields
		       * when present *)
		      fun process1 (ct, declExprs) =
			  let
			    val ty = cnvCtype (false, ct)
			    fun process2 (decr,expr)
				 : Ast.ctype * Ast.member option * LargeInt.int option = 
			      let
				val (ty', memNameOpt, loc) = mungeTyDecr (ty, decr)
				val sizeOpt = 
				  case expr of
				    PT.EmptyExpr => NONE
				      (* nch: fix: check bitfield types -- see checks in sizeof *)
				  | _ => (case evalExpr expr of
					    (SOME i, _, _, false) => SOME i
					  | (SOME i, _, _, true) => 
					      (if !reduce_sizeof then ()
					       else warn ("sizeof in bitfield specification " ^
							  "not preserved in source-to-source mode");
					       SOME i)
					  | (NONE, _, _, _) => 
						 (error "Bitfield size must be constant expression";
						  NONE))
				val memberOpt : Ast.member option = 
				    (case memNameOpt
				       of SOME id' => 
					  let val sym = Sym.member (tid,id')
					      val _ =
					       checkNonIdRebinding(sym, ty',
							      "struct/union member ");
					      val _ =
					       if isPartialTy ty'
					       then error("Member `" ^ id' 
							  ^ "' has incomplete type.")
					       else ();
					      val _ =
					       if isNonPointerFunction ty'
					       then error("Member `" ^  id'
							^ "' has function type.")
					       else ();
					      val member = {name = sym,
							    uid = Pid.new(),
							    location = loc,
							    ctype = ty',
							    kind = if isStruct
							      then Ast.STRUCTmem
						              else Ast.UNIONmem}
					   in bindSym(sym,MEMBER member);
					      SOME member
					    (* DBM: FIELDs? *)
					  end
					| NONE => NONE)
			       in (ty', memberOpt, sizeOpt)
			      end (* fun process2 *)
			   in map process2 declExprs
			  end (* fun process1 *)

		      (* union members are more restricted than struct members *)
		      fun checkUnionMember (ty: Ast.ctype, NONE: Ast.member option,
					    _ : LargeInt.int option) =
			  (error "union member has no name";
			   (ty,bogusMember(Sym.member(tid,"<noname>"))))
			| checkUnionMember (ty,SOME m,SOME _) =
			  (error "union member has size spec";
			   (ty,m))
			| checkUnionMember (ty,SOME m,NONE) = (ty,m)

		   in if alreadyDefined then ()
		      else
			let val members = List.map process1 members
			    val members = List.concat members
			    val namedTy =
				if isStruct then B.Struct(tid, members)
				else B.Union(tid, map checkUnionMember members)
			    val binding : B.tidBinding =
				{name = tagOpt, ntype = SOME namedTy,
				 global = topLevel(), location = getLoc()}
			in bindTid (tid, binding);
			   pushTids tid
			end;
		      (if isStruct then Ast.StructRef else Ast.UnionRef) tid
		  end

		| (PT.TypedefName s) :: l => 
		   (* type symbol is added at the point of declaration: see
		    * cnvExternalDecl (case ExternalDecl(TypeDecl) and cnvStatement (case
		    * Decl(TypeDecl) *)
		   (noMore l "Typedef";
		    case lookSym (Sym.typedef s)
		      of SOME(TYPEDEF{ctype,...}) => ctype
		       | _ => (error("typedef " ^ s ^ " has not been defined.");
			       Ast.Error))

		| (PT.StructTag {isStruct,name=s}) :: l => 
		   let val _ = noMore l "Struct"
		       val sym = Sym.tag s
		       val tyOpt =
			   case lookSym sym
			     of SOME(TAG{ctype,...}) => SOME ctype
			      | NONE => NONE
			      | _ => (bug "cnvExpression: bad tag 3"; NONE)
		    in if not (isSome tyOpt) orelse 
			  (isShadow andalso not (isLocalScope sym)) then
			 let val tid = Tid.new ()
			     val ty = (if isStruct then Ast.StructRef else Ast.UnionRef) tid
			  in bindSym(sym, TAG{name=sym,uid=Pid.new(),
					      location=getLoc(), ctype=ty});
			     bindTid (tid, {name=SOME s, ntype=NONE,
					    global=topLevel(), location=getLoc()});
			     ty
			 end
		       else valOf tyOpt  (* guaranteed to be SOME *)
		   end

		| (PT.EnumTag s) :: l => (* nearly idenitical to struct tag case *)
		   let val _ = noMore l "Enum"
		       val sym = Sym.tag s
		       val tyOpt = 
			   case lookSym sym
			     of SOME(TAG{ctype,...}) => SOME ctype
			      | NONE => (if TypeCheckControl.partial_enum_error
					   then error("incomplete enum " ^ s)
					 else ();
					 NONE)
			      | _ => (bug "cnvExpression: bad tag 3"; NONE)
		    in if not (isSome tyOpt) orelse 
			(isShadow andalso not (isLocalScope sym)) then
			(* if this is explicitly a shadow or a enum tag not seen
			 * before then create a new named type identifier and
			 * record that this type is partially (incompletely)
			 * defined *)
			 let val tid = Tid.new ()
			     val ty = Ast.EnumRef tid
			  in bindSym(sym, TAG{name=sym,uid=Pid.new(),
					      location=getLoc(), ctype=ty});
			     bindTid (tid, {name=SOME s, ntype=NONE,
					    global=topLevel(), location=getLoc()});
			     ty
			 end
		      (* otherwise return the type already established in
		       * environment *)
		       else valOf tyOpt
		   end

		| (PT.SpecExt xspec) :: rest => 
		   CNVSpecifier {isShadow=isShadow, rest=rest} xspec
		| l => cnvSpecList l
	  end

	  val {qualifiers, specifiers} = ty
       in cnvQualifiers (cnvSpecifier specifiers) qualifiers
      end

  and cnvType (isShadow: bool, {storage,qualifiers,specifiers}: PT.decltype)
        : Ast.ctype * Ast.storageClass =
      let val sc = cnvStorage storage
	  val ct = cnvCtype (isShadow,{qualifiers=qualifiers,specifiers=specifiers})
       in (ct,sc)
      end

  and cnvQualifiers ty []            = ty
    | cnvQualifiers ty [PT.CONST]    = Ast.Qual (Ast.CONST, ty)
    | cnvQualifiers ty [PT.VOLATILE] = Ast.Qual (Ast.VOLATILE, ty)
    | cnvQualifiers ty (PT.VOLATILE :: PT.VOLATILE :: _) =
       (error "Duplicate `volatile'."; ty)
    | cnvQualifiers ty (PT.CONST :: PT.CONST :: _) = 
       (error "Duplicate 'const'."; ty)
    | cnvQualifiers ty (_ :: _ :: _ :: _) = 
       (error "too many 'const/volatile' qualifiers."; ty)
       (* See: ISO-C Standard, p. 64 for meaning of const volatile. *)
    | cnvQualifiers ty (_ :: _ :: nil) = ty



  (* --------------------------------------------------------------------
   * cnvStorage : PT.storage list -> Ast.storageClass option
   *
   * Converts a parse-tree storage class into an ast storage class. The
   * only subtlety is the case where no parse-tree storage class has been
   * given in which case the default (supplied by second argument) ast
   * storage class is used.
   *      
   * For rules for storage classes, see K&R A8.1
   * -------------------------------------------------------------------- *)

  and cnvStorage []            = Ast.DEFAULT
    | cnvStorage [PT.STATIC]   = Ast.STATIC
    | cnvStorage [PT.EXTERN]   = Ast.EXTERN
    | cnvStorage [PT.REGISTER] = Ast.REGISTER
    | cnvStorage [PT.AUTO]     = Ast.AUTO
    | cnvStorage [PT.TYPEDEF]  =
      (error "illegal use of TYPEDEF";
       Ast.DEFAULT)
    | cnvStorage _ = 
      (error "Declarations can contain at most one storage class\
              \ (static, extern, register, auto).";
       Ast.DEFAULT)

  (* --------------------------------------------------------------------
   * evalExpr : ParseTree expr -> int option
   *
   * Converts parse-tree expressions to integer constants where possible;
   * NONE used for cases where no constant can be computed or when no
   * expression is given. A new environment is returned because it is
   * possible to embed definitions of struct/union/enum types within
   * sizeofs and casts. 
   * -------------------------------------------------------------------- *)

  and evalExpr e = (* evalExpr should not be called with PT.EmptyExpr *)
    let
      val encounteredSizeof = ref false
      val (eTy, e') = cnvExpression (e)
      fun evalAstExpr (Ast.EXPR (coreExpr,adorn, _)) =
	  case coreExpr
	    of Ast.IntConst i => SOME i
	     | Ast.Unop (unop, e) => evalUnaryOp (unop, e)
	     | Ast.Binop (binop, e, e') => evalBinaryOp (binop, e, e')
	     | Ast.QuestionColon (e0,e1,e2) =>
		(case evalAstExpr e0
		   of SOME 0 => evalAstExpr e2
		    | SOME _ => evalAstExpr e1
		    | NONE    => NONE)
	     | Ast.Cast (ct,e) => 
		let val eTy = lookAid adorn
		 in if compatible (ct, eTy) then ()
		    else warn "evalExpr: cast not handled yet";
		    evalAstExpr e
		end
	     | Ast.EnumId (_, i) => SOME i
             | Ast.SizeOf ct => (encounteredSizeof := true;
				 SOME(sizeof ct))
	     | _ => NONE

      and evalBinaryOp (binop, e, e') =
	let val opt = evalAstExpr e
	    val opt' = evalAstExpr e'
	in
	  if isSome opt andalso isSome opt' then
	    let val i  = valOf opt
		val i' = valOf opt'
	    in case binop
		 of Ast.Plus   => SOME (i + i')
		  | Ast.Minus  => SOME (i - i')
		  | Ast.Times  => SOME (i * i')
		  | Ast.Divide => SOME (LargeInt.quot (i,i'))
		  | Ast.Mod    => SOME (LargeInt.rem (i,i'))
		  | Ast.Gt     => SOME (if i > i' then 1 else 0)
		  | Ast.Lt     => SOME (if i < i' then 1 else 0)
		  | Ast.Gte    => SOME (if i >= i' then 1 else 0)
		  | Ast.Lte    => SOME (if i <= i' then 1 else 0)
		  | Ast.Eq     => SOME (if i = i' then 1 else 0)
		  | Ast.Neq    => SOME (if i <> i' then 1 else 0)
		  | Ast.And    => SOME (if i<>0 andalso i'<>0 then 1 else 0)
		  | Ast.Or     => SOME (if i<>0 orelse i'<>0 then 1 else 0)
		  | Ast.BitOr  => 
		      SOME (W.toLargeInt (W.orb (W.fromLargeInt i, W.fromLargeInt i')))  
		  | Ast.BitXor => 
		      SOME (W.toLargeInt (W.xorb (W.fromLargeInt i, W.fromLargeInt i')))  
		  | Ast.BitAnd => 
		      SOME (W.toLargeInt (W.andb (W.fromLargeInt i, W.fromLargeInt i')))  
		  | Ast.Lshift => 
		      SOME (W.toLargeInt (W.<< (W.fromLargeInt i, W.fromLargeInt i')))  
		  | Ast.Rshift =>
		      SOME (W.toLargeInt (W.>> (W.fromLargeInt i, W.fromLargeInt i')))
	       | _      => NONE
	    end
	  else
	    NONE
	end

      and evalUnaryOp (unop, e) =
	let
	  val opt  = evalAstExpr e  
	in
	  if isSome opt then
	    let
	      val i  = valOf opt
	    in case unop
		 of Ast.Negate => SOME (~i)
		  | Ast.Not    => SOME (if i = 0 then 1 else 0)
		  | Ast.Uplus  => SOME i
		  | Ast.BitNot => SOME (W.toLargeInt (W.notb (W.fromLargeInt i)))
		  | _      => NONE
	    end
	  else NONE
	end
     in (evalAstExpr e', eTy, e', !encounteredSizeof)
    end

  (* --------------------------------------------------------------------
   * makeAst' : ParseTree.external_decl list * Error.errorState -> Ast.ast 
   * 
   * Converts a parse tree into an ast, by recursively converting
   * each delcaration in the list.
   * -------------------------------------------------------------------- *)

  (* initializing extension conversion functions *)

  val _ = 
   let val coreFuns = {stateFuns=stateFuns,
		       mungeTyDecr=(fn (ty, decr) =>
				       let val (ctype, name, _) =
					     mungeTyDecr(ty,decr)
				        in (ctype, name) end),
		       (* since we added location in the output of mungeTyDecr and
			* we don't want to change the extension interface *)
		       cnvType=cnvType,
		       cnvExpression=cnvExpression,
		       cnvStatement=cnvStatement,
		       cnvExternalDecl=cnvExternalDecl,
		       wrapEXPR=wrapEXPR,
		       wrapSTMT=wrapSTMT,
		       wrapDECL=wrapDECL} 
     val {CNVExp, CNVStat, CNVBinop, CNVUnop, CNVExternalDecl,
	    CNVSpecifier, CNVDeclarator, CNVDeclaration} = CnvExt.makeExtensionFuns coreFuns
   in
     refCNVExp := CNVExp;
     refCNVStat := CNVStat;
     refCNVBinop := CNVBinop;
     refCNVUnop := CNVUnop;
     refCNVExternalDecl := CNVExternalDecl;
     refCNVSpecifier := CNVSpecifier;
     refCNVDeclarator := CNVDeclarator;
     refCNVDeclaration := CNVDeclaration
   end

  fun makeAst' extDecls =
      let val _ = if !multi_file_mode then print "Warning: multi_file_mode on\n"
		  else ()
	  val _ = Sizeof.reset()
	  (* this is the top-level call for this structure;
	   * must reset sizeof memo table *)
	  val astExtDecls = 
	      let fun process x = 
		      let val astExtDecl = cnvExternalDecl x
			  val newtids = resetTids ()
		       in (List.map (fn x => wrapDECL(Ast.ExternalDecl(Ast.TypeDecl{shadow=NONE, tid=x})))
			            newtids)
			   @ astExtDecl
		      end
	       in List.map process extDecls
	      end
	  val astExtDecls = List.concat astExtDecls
	  val errorCount = Error.errorCount errorState
	  val warningCount = Error.warningCount errorState
       in
	  {ast=astExtDecls, tidtab=ttab, errorCount=errorCount, warningCount=warningCount, 
	   auxiliaryInfo = {aidtab=atab, implicits=implicits, env=getGlobalEnv()}}
          (* DBM: will we want to reuse errorState? *)
      end (* fun makeAst' *)

 in
    makeAst'
end (* fun makeAst *)

end (* local open Bindings *)

end (* structure BuildAst *)
