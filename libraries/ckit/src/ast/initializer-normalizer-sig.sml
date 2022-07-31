(* initializer-normalizer-sig.sml *)

signature INITIALIZER_NORMALIZER =
sig

  val normalize : {lookTid : Tid.uid -> Bindings.tidBinding option,
		   bindAid : Ast.ctype -> Aid.uid,
		   initType : Ast.ctype,
		   initExpr : Ast.initExpression}
                  -> Ast.initExpression

end
