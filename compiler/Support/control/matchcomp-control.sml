(* Admin/control/matchcomp-control.sml *)
(* (C) The Fellowship of SML/NJ (https://www.smlnj.org)
 *
 * match compiler controls (structure MatchCompControl: MATCHCOMP_CONTROL)
 *)


structure MatchCompControl : MATCHCOMP_CONTROL =
struct

  val mcdebugging = ref false (* "MatchComp debugging" *)
  val mcstats = ref false (* "match compiler timing and stats" *)
  val printProtoAndor = ref false (* "andor print mode" *)
  val printAndor = ref false (* "andor print mode" *)
  val printDecisionTree = ref false (* "decision tree print mode" *)
  val printMatchAbsyn = ref false (* "match absyn print mode" *)
  val printMatch = ref false (* "print argument match" *)
  val printRet = ref false (* "return print mode" *)

  val bindNoVariableWarn =
      ref false (* "whether to warn if no variables get bound" *)
  val bindNonExhaustiveWarn =
      ref true (* "whether to warn on non-exhaustive bind" *)
  val bindNonExhaustiveError =
      ref false (* "whether non-exhaustive bind is an error" *)
  val matchNonExhaustiveWarn =
      ref true (* "whether to warn on non-exhaustive match" *)
  val matchNonExhaustiveError =
      ref false (* "whether non-exhaustive match is an error" *)
  (* matchExhaustiveError overrides matchExhaustiveWarn *)

  val matchRedundantWarn =
      ref true (* "whether to warn on redundant matches" *)
  val matchRedundantError =
      ref true (* "whether a redundant match is an error" *)
  (* matchRedundantError overrides matchRedundantWarn *)

end (* structure MatchCompControl *)
