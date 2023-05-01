(* specialsyms.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)
structure SpecialSymbols = struct
  local
    structure S = Symbol
  in
    val paramId = S.strSymbol "<param>"
    val functorId = S.fctSymbol "<functor>"
    val hiddenId = S.strSymbol "<hidden>"
    val tempStrId = S.strSymbol "<tempStr>"
    val tempFctId = S.fctSymbol "<tempFct>"
    val fctbodyId = S.strSymbol "<fctbody>"
    val anonfsigId = S.fsigSymbol "<anonfsig>"
    val resultId = S.strSymbol "<resultStr>"
    val returnId = S.strSymbol "<returnStr>"
    val internalVarId = S.varSymbol "<InternalVar>"
  end
end
