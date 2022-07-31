(* Copyright (c) 1998 by Lucent Technologies *)

structure EqAst (*: EQAST*) =
struct 

  structure Ast = Ast
  structure CT = CType
  structure ECT = EqCType
  structure PPL = PPLib
  structure PPA = PPAst
  structure EAT = EqAstExt
  structure PT = Pidtab

  open Ast

  exception internalFail

  val myFold = ECT.myFold

  val trace = ref false

  fun tracer pp (ttab1,ttab2) (v1,v2) =
      ( print "\nChecking:  "
      ; PPL.ppToStrm (pp () ttab1) TextIO.stdOut v1
      ; print "\nand:  "
      ; PPL.ppToStrm (pp () ttab2) TextIO.stdOut v2
      ; print "\n"
      ) 

  fun PTinserts pidmap =
      myFold (fn () => fn (v1,v2) => Pidtab.insert (pidmap,v1,v2)) ()

  fun TTinserts tidmap =
       myFold (fn () => fn (v1,v2) => Tidtab.insert (tidmap,v1,v2)) ()

  fun eqOpt f tabs maps (NONE,NONE) = ()
    | eqOpt f tabs maps (SOME v1,SOME v2) = f tabs maps (v1,v2)
    | eqOpt f tabs maps _ = raise internalFail

  fun eqAst (edecls1,ttab1,edecls2,ttab2) =
      let val tl1 = Tidtab.listItems ttab1
	  val tl2 = Tidtab.listItems ttab2
      in if List.length tl1 = List.length tl2
	     then eqExternalDecls (ttab1,ttab2) (edecls1,edecls2)
	 else raise ECT.eqFail
      end

  and eqExternalDecls tabs decs = 
      let val maps = (Tidtab.uidtab (),Pidtab.uidtab ())
      in  getExternalTypeBindings tabs maps decs
	 ; myFold (eqExternalDecl tabs) maps decs
      end

  and getExternalTypeBindings tabs maps decs =
      myFold (getExternalTypeBinding tabs) maps decs

  (* dpo: this needs to be fixed to declare types/functions and then check *)

  and getExternalTypeBinding tabs maps edeclPair = ()

  and eqExternalDecl tabs maps (DECL (coreDecl1,_,_),DECL (coreDecl2,_,_)) =
      eqExternalCoreDecl tabs maps (coreDecl1,coreDecl2)

  and eqExternalCoreDecl (tabs as (ttab1,ttab2)) (maps as (tidmap,pidmap)) coreDeclPair =
      ( if !trace then tracer PPA.ppCoreExternalDecl tabs coreDeclPair
	else ()
      ; case coreDeclPair
	  of (ExternalDecl decl1,ExternalDecl decl2) => 
	      eqDecl tabs maps (decl1,decl2)
	   | (FunctionDef (id1,ids1,stmt1),FuncDecl (id2,ids2,stmt2)) =>
		 let val pids1 = map (fn {uid,...} => uid) (id1::ids1)
		     val pids2 = map (fn {uid,...} => uid) (id2::ids2)
		 in   PTinserts pidmap (pids1,pids2)
		    ; eqStmt tabs maps (stmt1,stmt2)
		 end
	   | _ => raise ECT.eqFail
      )

  and eqStmt (tabs as (ttab1,ttab2)) maps (stmtPair as (STMT (coreStmt1,_,_),STMT (coreStmt2,_,_))) =
      ( if !trace then tracer PPA.ppStatement tabs stmtPair else ()
      ; eqCoreStmt tabs maps (coreStmt1,coreStmt2)
      )
    handle internalFail => 
	 ( print "\nThese two statements are not condidered equal:"
	 ; PPL.ppToStrm (PPA.ppStatement () ttab1) TextIO.stdOut (#1 stmtPair)
	 ; print "\nand:"
	 ; PPL.ppToStrm (PPA.ppStatement () ttab2) TextIO.stdOut (#2 stmtPair)
	 ; print "\n"
	 ; raise ECT.eqFail
	 )

  and eqDecl tabs (maps as (tidmap,pidmap)) declPair = 
      case declPair
	of (TypeDecl tid1,TypeDecl tid2) =>
	    ECT.getTidBindings tabs maps (tid1,tid2)
         | (VarDecl ({uid=pid1,...},initExpOpt1)
	   ,VarDecl ({uid=pid2,...},initExpOpt2)) =>
	       if eqInitExprOpt tabs maps (initExpOpt1,initExpOpt2)
	       then Pidtab.insert (pidmap,pid1,pid2)
	       else raise internalFail
	 | _ => raise internalFail 

  and eqDecls tabs maps declsPair = 
      ECT.myFold
      (fn () => fn declPair => eqDecl tabs maps declPair)
      ()
      declsPair

  and eqInitExpr tabs maps initExpPair = 
      case initExpPair
	of (Simple exp1,Simple exp2) => eqExpr tabs maps (exp1,exp2)
	 | (Aggregate initExps1,Aggregate initExps2) => 
	       ECT.myFold
	         (fn () => fn iePair => eqInitExpr tabs maps iePair)
		 ()
		 (initExps1,initExps2)

  and eqInitExprOpt tabs = eqOpt eqInitExpr tabs

  and eqCoreStmt (tabs as (ttab1,ttab2)) (maps as (tidmap,pidmap)) coreStmtPair = 
    (case coreStmtPair
      of (Expr expOpt1,Expr expOpt2) => 
	   if eqExpr tabs maps (expOpt1,expOpt2) then ()
	   else raise internalFail
       | (Compound (decls1,stmts1),Compound (decls2,stmts2)) =>
	   ( eqDecls (ttab1,ttab2) maps (decls1,decls2)
	   ; eqStmts tabs maps (stmts1,stmts2)
	   )
       | (While (exp1,stmt1),While (exp2,stmt2)) => 
	   if eqExpr tabs maps (exp1,exp2)
	       then eqStmt tabs maps (stmt1,stmt2)
	   else raise internalFail
       | (Do (exp1,stmt1),Do (exp2,stmt2)) => 
	   if eqExpr tabs maps (exp1,exp2) 
	       then eqStmt tabs maps (stmt1,stmt2)
	   else raise internalFail
       | (For (expOpt1_1,expOpt1_2,expOpt1_3,stmt1)
	 ,For (expOpt2_1,expOpt2_2,expOpt2_3,stmt2)) =>
	   if eqExprOpt tabs maps (expOpt1_1,expOpt2_1) andalso
	      eqExprOpt tabs maps (expOpt1_2,expOpt2_2) andalso
	      eqExprOpt tabs maps (expOpt1_3,expOpt2_3) 
	       then eqStmt tabs maps (stmt1,stmt2)
	   else raise internalFail		     
       | (Labeled (pid1,stmt1),Labeled (pid2,stmt2)) =>
	   let val pidmap = Pidtab.insert (pidmap,pid1,pid2)
	   in eqStmt tabs (tidmap,pidmap) (stmt1,stmt2) end
       | (CaseLabel (li1,stmt1),CaseLabel (li2,stmt2)) => 
	   if li1 = li2 then eqStmt tabs maps (stmt1,stmt2)
	   else raise internalFail
       | (DefaultLabel stmt1,DefaultLabel stmt2) =>
	   eqStmt tabs maps (stmt1,stmt2)
       | (Goto pid1,Goto pid2) => 
	   if ECT.eqPid pidmap (pid1,pid2) then maps
	   else raise internalFail
       | (Break,Break) => maps
       | (Continue,Continue) => maps
       | (Return expOpt1,Return expOpt2) => 
	   if eqExprOpt tabs maps (expOpt1,expOpt2) then ()
	   else raise internalFail
       | (IfThen (exp1,stmt1),IfThen (exp2,stmt2)) => 
	   if eqExpr tabs maps (exp1,exp2)
	       then eqStmt tabs maps (stmt1,stmt2)
	   else raise internalFail
       | (IfThenElse (exp1,stmt1_1,stmt1_2),IfThenElse (exp2,stmt2_1,stmt2_2)) => 
	   if eqExpr tabs maps (exp1,exp2)
	       then eqStmts tabs maps ([stmt1_1,stmt1_2],[stmt2_1,stmt2_2]) 
	   else raise internalFail
       | (Switch (exp1,stmt1),Switch (exp2,stmt2)) =>
	   if eqExpr tabs maps (exp1,exp2)
	       then eqStmt tabs maps (stmt1,stmt2)
	   else raise internalFail
       | (StatExt se1,StatExt se2) => EAT.eqStatementExt tabs maps (se1,se2)
       | _ => raise internalFail)

  and eqStmts tabs maps = myFold (eqStmt tabs) maps

  and eqExprOpt tabs = eqOpt eqExpr tabs

  and eqExpr (tabs as (ttab1,ttab2)) maps (expPair as (EXPR (coreExpr1,_,_),EXPR (coreExpr2,_,_)))  = 
      ( if !trace then tracer PPA.ppExpression tabs expPair else ()
      ; if eqCoreExpr tabs maps (coreExpr1,coreExpr2) then true else raise internalFail
      ) handle internalFail => 
	    ( print "\nThese two expressions are not condidered equal:"
	    ; PPL.ppToStrm (PPA.ppExpression () ttab1) TextIO.stdOut (#1 expPair)
	    ; print "\n and"
	    ; PPL.ppToStrm (PPA.ppExpression () ttab2) TextIO.stdOut (#2 expPair)
	    ; print "\n"
	    ; raise ECT.eqFail
	    )

  and eqCoreExpr tabs (maps as (tidmap,pidmap)) coreExprPair = 
      case coreExprPair
	of (IntConst li1,IntConst li2) => li1 = li2
	 | (RealConst r1,RealConst r2) => Real.== (r1,r2)
	 | (StringConst s1,StringConst s2) => s1=s2
	 | (Call (exp1,exps1),Call (exp2,exps2)) => 
	       eqExprs tabs maps (exp1::exps1,exp2::exps2)
	 | (QuestionColon (e1_1,e1_2,e1_3),QuestionColon (e2_1,e2_2,e2_3)) =>
	       eqExprs tabs maps ([e1_1,e1_2,e1_3],[e2_1,e2_2,e2_3])
	 | (Assign (e1_1,e1_2),Assign (e2_1,e2_2)) =>
	       eqExprs tabs maps ([e1_1,e1_2],[e2_1,e2_2])
	 | (Comma (e1_1,e1_2),Comma (e2_1,e2_2)) =>
	       eqExprs tabs maps ([e1_1,e1_2],[e2_1,e2_2])
	 | (Sub (e1_1,e1_2),Sub (e2_1,e2_2)) =>
	       eqExprs tabs maps ([e1_1,e1_2],[e2_1,e2_2])
	 | (Member (exp1,pid1),Member (exp2,pid2)) =>
	       ECT.eqPid pidmap (pid1,pid2) andalso eqExpr tabs maps (exp1,exp2)
	 | (Arrow (exp1,pid1),Arrow (exp2,pid2)) =>
	       ECT.eqPid pidmap (pid1,pid2) andalso eqExpr tabs maps (exp1,exp2)
	 | (Deref exp1,Deref exp2) => eqExpr tabs maps (exp1,exp2)
	 | (AddrOf exp1,AddrOf exp2) => eqExpr tabs maps (exp1,exp2)
	 | (Binop (binop1,e1_1,e1_2),Binop (binop2,e2_1,e2_2)) => 
	       binop1 = binop2 andalso eqExprs tabs maps ([e1_1,e1_2],[e2_1,e2_2])
	 | (Unop (unop1,exp1),Unop (unop2,exp2)) => 
	       unop1 = unop2 andalso eqExpr tabs maps (exp1,exp2)
	 | (Cast (ctype1,exp1),Cast (ctype2,exp2)) =>
	     (ECT.eqCtype tidmap (ctype1,ctype2) handle _ => false)
	     andalso eqExpr tabs maps (exp1,exp2)
	 | (Id pid1,Id pid2) =>
	       ECT.eqPid pidmap (pid1,pid2)
	 | (EnumId (pid1,li1),EnumId (pid2,li2)) => 
	       li1 = li2 andalso ECT.eqPid pidmap (pid1,pid2)
	 | (ExprExt ee1,ExprExt ee2) => EAT.eqExpressionExt tabs maps (ee1,ee2)
	 | (ErrorExpr,ErrorExpr) => true
	 | _ => raise internalFail

  and eqExprs tabs maps = ECT.eqList (eqExpr tabs maps)

end (* structure EqAst *)
