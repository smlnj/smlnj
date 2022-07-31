(* Copyright (c) 1998 by Lucent Technologies *)

(* Used to copy Ast structures while preserving unique adornments.
 *)

structure CopyAst : COPYAST =
struct 

  open Ast

  type aidtab = Tables.aidtab

  fun copyOpt copier aidtab NONE = NONE
    | copyOpt copier aidtab (SOME v) = SOME (copier aidtab v)

  fun copyAid aidtab oldaid = 
      let val newaid = Aid.new ()
       in case Aidtab.find (aidtab,oldaid)
	    of NONE => () (* DBM: is this anomalous, should there be a warning? *)
	     | SOME ctype => Aidtab.insert (aidtab, newaid, ctype);
	  newaid
      end

  fun copyAst aidtab extdecls =
      List.map (copyExtDecl aidtab) extdecls

  and copyExtDecl aidtab (DECL (coreExtDecl,aid,loc)) =
      let val aid = copyAid aidtab aid
	  val coreExtDecl = copyCoreExtDecl aidtab coreExtDecl
      in DECL (coreExtDecl,aid,loc) end

  and copyCoreExtDecl aidtab =
      fn ExternalDecl decl => ExternalDecl (copyDecl aidtab decl)
       | FunctionDef (id,ids,stmt) => FunctionDef (id,ids,copyStmt aidtab stmt)
       | ExternalDeclExt ext => 
	     ExternalDeclExt(CopyAstExt.copyExtDeclExt (copyExpr,copyStmt,copyExtDecl) aidtab ext)

  and copyDecl aidtab =
      fn VarDecl (id,initExprOpt) => VarDecl (id,copyOpt copyInitExpr aidtab initExprOpt)
       | typedecl => typedecl (* TypeDecl *)

  and copyStmt aidtab (stmt as STMT (coreStmt,aid,loc)) =
      let val aid = copyAid aidtab aid
	  val coreStmt = copyCoreStmt aidtab coreStmt
       in STMT (coreStmt,aid,loc)
      end

  and copyCoreStmt aidtab = 
      fn Expr exprOpt => Expr (copyOpt copyExpr aidtab exprOpt)
       | Compound (decls,stmts) => Compound (map (copyDecl aidtab) decls, List.map (copyStmt aidtab) stmts)
       | While (expr,stmt) => While (copyExpr aidtab expr, copyStmt aidtab stmt)
       | Do (expr,stmt) => Do (copyExpr aidtab expr, copyStmt aidtab stmt)
       | For (exprOpt0,exprOpt1,exprOpt2,stmt) => 
           For (copyOpt copyExpr aidtab exprOpt0,
		copyOpt copyExpr aidtab exprOpt1,
		copyOpt copyExpr aidtab exprOpt2,
		copyStmt aidtab stmt)
       | Labeled (pid,stmt) => Labeled (pid,copyStmt aidtab stmt)
       | CaseLabel (li,stmt) => CaseLabel (li,copyStmt aidtab stmt)
       | DefaultLabel stmt => DefaultLabel (copyStmt aidtab stmt)
       | Return exprOpt => Return (copyOpt copyExpr aidtab exprOpt)
       | IfThen (expr,stmt) => IfThen (copyExpr aidtab expr,copyStmt aidtab stmt)
       | IfThenElse (expr,stmt0,stmt1) =>
	   IfThenElse (copyExpr aidtab expr,copyStmt aidtab stmt0,copyStmt aidtab stmt1)
       | Switch (expr,stmt) => Switch (copyExpr aidtab expr,copyStmt aidtab stmt)
       | StatExt ext => 
	   StatExt(CopyAstExt.copyStmtExt (copyExpr,copyStmt,copyExtDecl) aidtab ext)
       | stmt => stmt

  and copyExpr aidtab (EXPR (coreExpr,aid,loc)) =
      let val aid = copyAid aidtab aid
	  val coreExpr = copyCoreExpr aidtab coreExpr
       in EXPR (coreExpr,aid,loc)
      end

  and copyCoreExpr aidtab =
      fn Call (expr,exprs) =>
           Call (copyExpr aidtab expr,List.map (copyExpr aidtab) exprs)
       | QuestionColon (expr0,expr1,expr2) =>
	   QuestionColon (copyExpr aidtab expr0, 
			  copyExpr aidtab expr1,
			  copyExpr aidtab expr2)
       | Assign (expr0,expr1) =>
	   Assign (copyExpr aidtab expr0, copyExpr aidtab expr1)
       | Comma (expr0,expr1) => Comma (copyExpr aidtab expr0, copyExpr aidtab expr1)
       | Sub (expr0,expr1) => Sub (copyExpr aidtab expr0,copyExpr aidtab expr1)
       | Member (expr,pid) => Member (copyExpr aidtab expr,pid)
       | Arrow (expr,pid) => Arrow (copyExpr aidtab expr,pid)
       | Deref expr => Deref (copyExpr aidtab expr)
       | AddrOf expr => AddrOf (copyExpr aidtab expr)
       | Binop (binop,expr0,expr1) =>
	  Binop (binop,copyExpr aidtab expr0, copyExpr aidtab expr1)
       | Unop (unop,expr) => Unop (unop,copyExpr aidtab expr)
       | Cast (ctype,expr) => Cast (ctype,copyExpr aidtab expr)
       | ExprExt ext =>
	   ExprExt(CopyAstExt.copyExprExt (copyExpr,copyStmt,copyExtDecl) aidtab ext)
       | expr => expr

  and copyInitExpr aidtab =
      fn Simple expr => Simple (copyExpr aidtab expr)
       | Aggregate inits => Aggregate (map (copyInitExpr aidtab) inits)

end (* functor CopyAstFn *)
