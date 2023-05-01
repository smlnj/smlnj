(* Admin/control/matchcomp-control.sig *)
(* (C) The Fellowship of SML/NJ (https://www.smlnj.org)
 *
 * match compiler controls (signature for MatchCompControl)
 *)

signature MATCHCOMP_CONTROL =
sig

  val mcdebugging : bool ref
  val mcstats : bool ref
  val printProtoAndor : bool ref
  val printAndor : bool ref
  val printDecisionTree : bool ref
  val printMatchAbsyn : bool ref
  val printMatch : bool ref
  val printRet : bool ref

  val bindNoVariableWarn : bool ref
  val bindNonExhaustiveWarn : bool ref
  val bindNonExhaustiveError : bool ref
  val matchNonExhaustiveWarn : bool ref
  val matchNonExhaustiveError : bool ref
  val matchRedundantWarn : bool ref
  val matchRedundantError : bool ref

end (* signature MATCHCOMP_CONTROL *)
