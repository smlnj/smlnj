(* specialsyms.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)

structure SpecialSymbols =
struct
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

  val appstriargId = S.strSymbol "<AppStrI_arg>"   (* for rpath in call of elabStr, AppStrI case *)
  val basefctId = S.strSymbol "<BaseFct>"          (* for rpath in call of elabStr, BaseFct case *)

  val errorId = S.strSymbol "<ErrorId>"
  val errorFctId = S.fctSymbol "<ErrorFctId>"
  val errorTycId = S.tycSymbol "<ErrorTycId>"
  val aStrId = S.strSymbol "<A_Id>"
  val bStrId = S.strSymbol "<B_Id>"
  val cStrId = S.strSymbol "<C_Id>"
  val dStrId = S.strSymbol "<D_Id>"
  val eStrId = S.strSymbol "<E_Id>"

end (* top local *)
end (* stucture SpecialSymbols *)
