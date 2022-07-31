structure CnvExt : CNVEXT = struct

  type coreConversionFuns = 
	{
	 stateFuns : State.stateFuns,
	 mungeTyDecr: (Ast.ctype*ParseTree.declarator ->Ast.ctype * string option),

	 cnvType : bool*ParseTree.decltype -> Ast.ctype*Ast.storageClass,
	 cnvExpression: ParseTree.expression -> Ast.ctype * Ast.expression,
	 cnvStatement : ParseTree.statement -> Ast.statement,
	 cnvExternalDecl: ParseTree.externalDecl -> Ast.externalDecl list,

	 wrapEXPR: (Ast.ctype*Ast.coreExpression -> Ast.ctype*Ast.expression),
	 wrapSTMT: Ast.coreStatement -> Ast.statement,
	 wrapDECL: Ast.coreExternalDecl -> Ast.externalDecl
	 }

  type expressionExt = (ParseTree.specifier, ParseTree.declarator, ParseTree.ctype, ParseTree.decltype,
			ParseTree.operator, ParseTree.expression, ParseTree.statement)
                       ParseTreeExt.expressionExt

  type statementExt = (ParseTree.specifier, ParseTree.declarator, ParseTree.ctype, ParseTree.decltype,
		       ParseTree.operator, ParseTree.expression, ParseTree.statement)
	              ParseTreeExt.statementExt

  type externalDeclExt = (ParseTree.specifier, ParseTree.declarator, ParseTree.ctype, ParseTree.decltype,
		          ParseTree.operator, ParseTree.expression, ParseTree.statement)
	                 ParseTreeExt.externalDeclExt 

  type specifierExt = (ParseTree.specifier, ParseTree.declarator, ParseTree.ctype, ParseTree.decltype,
		       ParseTree.operator, ParseTree.expression, ParseTree.statement)
	              ParseTreeExt.specifierExt

  type declaratorExt = (ParseTree.specifier, ParseTree.declarator, ParseTree.ctype, ParseTree.decltype,
		        ParseTree.operator, ParseTree.expression, ParseTree.statement)
	               ParseTreeExt.declaratorExt

  type declarationExt = (ParseTree.specifier, ParseTree.declarator, ParseTree.ctype, ParseTree.decltype,
		        ParseTree.operator, ParseTree.expression, ParseTree.statement)
	               ParseTreeExt.declarationExt

  type extensionFuns = 
      {CNVExp: expressionExt -> Ast.ctype * Ast.expression,
       CNVStat: statementExt -> Ast.statement,
       CNVBinop: {binop: ParseTreeExt.operatorExt, arg1Expr: ParseTree.expression, arg2Expr: ParseTree.expression}
                 -> Ast.ctype * Ast.expression,
       CNVUnop: {unop: ParseTreeExt.operatorExt, argExpr: ParseTree.expression}
                 -> Ast.ctype * Ast.expression,
       CNVExternalDecl: externalDeclExt -> Ast.externalDecl list,
       CNVSpecifier: {isShadow: bool, rest : ParseTree.specifier list} 
                 -> specifierExt
                 -> Ast.ctype,
       CNVDeclarator: Ast.ctype * declaratorExt 
                 -> Ast.ctype * string option,
       CNVDeclaration: declarationExt -> Ast.declaration list}

  exception CnvExt of string

  fun CNVExp _ = raise (CnvExt "No proper extensions to expressions")

  fun CNVStat _ = raise (CnvExt "No proper extensions to statements")

  fun CNVBinop _ = raise (CnvExt "No proper extensions to binops")

  fun CNVUnop _ =  raise (CnvExt "No proper extensions to unnops")

  fun CNVExternalDecl _ = raise (CnvExt "No proper extensions to external decls")
    
  fun CNVSpecifier _ _ = raise (CnvExt "No proper extensions to specifiers")

  fun CNVDeclarator _ = raise (CnvExt "No proper extensions to declarators")

  fun CNVDeclaration _ = raise (CnvExt "No proper extensions to declarations")

  fun makeExtensionFuns _ = {CNVExp = CNVExp,
			     CNVStat = CNVStat,
			     CNVBinop = CNVBinop,
			     CNVUnop = CNVUnop,
			     CNVExternalDecl = CNVExternalDecl,
			     CNVSpecifier = CNVSpecifier,
			     CNVDeclarator = CNVDeclarator,
			     CNVDeclaration = CNVDeclaration}

  (* prototypical use of makeExtensionsFuns for non-trival extensions:
     
     fun makeExtensions {stateFuns, mungeTyDecr, cnvType, cnvExpression, cnvStatement, cnvExternalDecl,
                         wrapEXPR, wrapSTMT, wrapDECL} =
       let 
           fun raiseError ... (* local helper function *)
           val ....   (* local helper function *)
            ... etc ... (* more local helper functions *)

           fun CNVExp args = ..
           fun CNVStat args = ..
            ... etc ...
       in {CNVExp = CNVExp, CNVStat = CNVStat, ....}
       end
   *)


end

(****************

structure CnvExt : CNVEXT = struct

  type cnv = {stateFuns: State.stateFuns,
	      cnvType: bool * ParseTree.decltype -> Ast.ctype * Ast.storageClass,
	      mungeTyDecr: Ast.ctype * ParseTree.declarator -> Ast.ctype * string option,
	      cnvExpression: ParseTree.expression -> Ast.ctype * Ast.expression,
	      cnvStatement: ParseTree.statement -> Ast.statement}

  exception CnvExt of string

  val intTy = Ast.Numeric (Ast.NONSATURATE,Ast.WHOLENUM,Ast.SIGNED,Ast.INT)

  fun CNVExp _ _ = raise (CnvExt "No proper extensions to expressions")

  fun CNVStat _ _ = raise (CnvExt "No proper extensions to statements")

  fun CNVBinop _ _ = raise (CnvExt "No proper extensions to binops")

  fun CNVUnop _ _ =  raise (CnvExt "No proper extensions to unnops")

  fun CNVExternalDecl _ _ = raise (CnvExt "No proper extensions to external decls")
    
  fun CNVSpecifier _ _ = raise (CnvExt "No proper extensions to specifiers")

  fun CNVDeclarator _ _ = raise (CnvExt "No proper extensions to declarators")

  fun CNVDeclaration _ _ = raise (CnvExt "No proper extensions to declarations")
end
 ***************)