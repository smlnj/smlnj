(* Copyright (c) 1998 by Lucent Technologies *)

(* simplify-assign-ops.sml
 *
 * Main Function: simplifyAssignOp {lookAid, getCoreType, wrapEXPR, getLoc, topLevel, bindSym}
                             (processBinop, opn, {preOp}, expr1, expr2)
 *
 *       processBinop -- function to call for typechecking and building binop expressions
 *       opn -- an Ast binary operation 
 *       {preOp} -- true if operation should be performed before result
 *                  e.g. ++x becomes simplifyAssignOp(+, {preOp=true}, x, 1)
 *                  e.g. x+=e becomes simplifyAssignOp(+, {preOp=true}, x, e)
 *               -- false if operation should be done after result
 *                  e.g. x++ becomes simplifyAssignOp(+, {preOp=false}, x, 1)
 *       expr1, expr2 -- expressions
 *   function returns an equivalent simplified expr.
 * 
 * Issues: 
 *  1. copying must maintain unique pid invariant.
 *  2. copying of rvals (simpleDup) versus lvals (duplicateLval) versus (duplicateRval)
 *  3. must be careful with types of new variables (bug #1)
 *     e.g. 
 *        struct { int count[3]; } *p;
 *        ....
 *        p->count[i]++;
 *     generates 
 *        int tmp1[3], tmp2; tmp1=p->count, tmp2=tmp1[i], tmp1[i]=tmp2, tmp2;
 *
 * AUTHORS: Nevin Heintze (nch@research.bell-labs.com)
 *
 * TBD: More testing...
 *)

structure SimplifyAssignOps =
struct

  (* Note: lvals are either
              Ast.Id
              Ast.Sub
              Ast.Arrow
              Ast.Deref
              Ast.Dot where first arg is an lval
   *)
 fun simplifyAssignOps {lookAid, getCoreType, wrapEXPR, getLoc, topLevel, bindSym, pushTmpVars} =
 let
  fun wrapEXPR' x = let val (ty, expr) = wrapEXPR x in expr end

  fun combineExprs'(x1, x2 as Ast.EXPR(_, adorn, _)) = wrapEXPR'(getCoreType(lookAid adorn), Ast.Comma(x1, x2))

  fun combineExprs(NONE, x) = x
    | combineExprs(SOME x1, x2) = combineExprs'(x1, x2)

  fun combineExprsOpt(NONE, x) = x
    | combineExprsOpt(x, NONE) = x
    | combineExprsOpt(SOME x1, SOME x2) = SOME(combineExprs'(x1, x2))

  fun getExprTy(Ast.EXPR(_, adorn, _)) = getCoreType(lookAid adorn)

  (* Can't just introduce id of type ty: may not be legal to do assignment (e.g. for arrays).
     So, first convert arrays to pointers, functions to pointers, and eliminate qualifiers.
     Potential problem: elimination of volatile qualifiers on temporary variables?
   *)
  fun niceTy ty =  
    (case getCoreType ty of  
         Ast.Array (_, arrayTp) => Ast.Pointer arrayTp
       | Ast.Function x => Ast.Pointer ty
       | _ => ty)

  fun simpleDup expr =  (* given e, return: (tmp = e, tmp, tmp) *)
    let
      val ty = getExprTy expr
      val sym = Symbol.object "tmp"
      val id = {name=sym, uid = Pid.new(), location = getLoc(),
		ctype = niceTy ty, stClass = Ast.DEFAULT, status = Ast.DECLARED,
		kind = Ast.NONFUN, global = topLevel()}
      val _ = pushTmpVars id
      val _ = bindSym(sym, Bindings.ID id)
      val exprNewVar = wrapEXPR'(ty, Ast.Id id)
    in
      {assigns=SOME(wrapEXPR'(ty, Ast.Assign(exprNewVar, expr))),
       var1=wrapEXPR'(ty, Ast.Id id),
       var2=wrapEXPR'(ty, Ast.Id id)}
    end

  fun duplicateRval (expr as Ast.EXPR(Ast.Id _, _, _)) = {assigns=NONE, var1=expr, var2=expr}
    | duplicateRval expr = simpleDup expr
  
  fun duplicateLval expr =  (* copy lval, factoring out side-effecting expressions *)
    let 
        fun dup(mkExp, expr) = 
	  let
	    val {assigns, var1, var2} = duplicateRval expr
	  in
	    {assigns=assigns,
	     copy1= mkExp var1,
	     copy2= mkExp var2}
	  end

        fun dup2(mkExp, expr1, expr2) =
	  let
	    val {assigns=assigns1, var1=var1a, var2=var1b} = duplicateRval expr1
	    val {assigns=assigns2, var1=var2a, var2=var2b} = duplicateRval expr2
	    val assigns = combineExprsOpt(assigns1, assigns2)
	  in
	    {assigns=assigns,
	     copy1=mkExp(var1a, var2a),
	     copy2=mkExp(var1b, var2b)}
	  end
    in
      case expr of 
	Ast.EXPR(Ast.Id pid, _, _) => {assigns=NONE,
				       copy1=expr,
				       copy2=wrapEXPR'(getExprTy expr, Ast.Id pid)}
      | Ast.EXPR(Ast.Arrow(expr1, member), adorn, loc) =>
	  dup(fn e => wrapEXPR'(lookAid adorn, Ast.Arrow(e, member)), expr1)
      | Ast.EXPR(Ast.Deref(expr1), adorn, loc) =>
	  dup(fn e => wrapEXPR'(lookAid adorn, Ast.Deref e), expr1)
      | Ast.EXPR(Ast.Sub(expr1, expr2), adorn, loc) =>
	  dup2(fn e => wrapEXPR'(lookAid adorn, Ast.Sub e), expr1, expr2)

      | Ast.EXPR(Ast.Member(expr1, member), _, _) =>
	  let
	    val ty = getExprTy expr
	    val {assigns, copy1, copy2} = duplicateLval(expr1)
	  in
	    {assigns=assigns,
	     copy1=wrapEXPR'(ty, Ast.Member(copy1, member)),
	     copy2=wrapEXPR'(ty, Ast.Member(copy2, member))}
	  end
      | Ast.EXPR(_, adorn, loc) => 
	   (* not an lval --> just use simple duplication (should never occur, unless error) *)
	  let val {assigns, var1, var2} = duplicateRval expr
	  in
	    {assigns=assigns, copy1=var1, copy2=var2}
	  end
    end

  fun simplifyAss(processBinop, opn, {preOp=true}, expr1, expr2) =   (* e.g. ++x; ++( *p ); x += 5; *p += 5; *)
    let val {assigns, copy1, copy2} = duplicateLval expr1
      fun procBinop x = let val (ty, expr) = processBinop x in expr end
      val newExpr = Ast.Assign(copy1, procBinop(getExprTy copy2, copy2, getExprTy expr2, expr2, opn))
      val newExpr = wrapEXPR'(getExprTy expr1, newExpr)
      val finalExpr = combineExprs(assigns, newExpr)
    in
      (getExprTy finalExpr, finalExpr)
    end
    | simplifyAss(processBinop, opn, {preOp=false}, expr1, expr2) =   (* e.g. x++; ( *p )++;  *)
    let val {assigns, copy1, copy2} = duplicateLval expr1
        val {assigns=assigns2, var1, var2} = simpleDup copy1
	fun procBinop x = let val (ty, expr) = processBinop x in expr end
	val newExpr = Ast.Assign(copy2, procBinop(getExprTy var1, var1, getExprTy expr2, expr2, opn))
	val newExpr = wrapEXPR'(getExprTy expr1, newExpr)
	val finalExpr = combineExprs(assigns, combineExprs(assigns2, combineExprs'(newExpr, var2)))
    in
      (getExprTy finalExpr, finalExpr)
    end

 in simplifyAss
 end
end