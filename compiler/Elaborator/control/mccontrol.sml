(* Elaborator/control/mccontrol.sml *)

(* Match compiler controls *)
structure MCControl : MC_CONTROL =
struct

    val priority = [10, 10, 4]
    val obscurity = 2
    val prefix = "compiler-mc"

    val registry = ControlRegistry.new { help = "match compiler settings" }

    val _ = BasicControl.nest (prefix, registry, priority)

    val bool_cvt = ControlUtil.Cvt.bool

    val nextpri = ref 0

    fun flag (name: string, help: string, default) =
	let val r = ref default
	    val p = !nextpri
	    val ctl = Controls.control
		        {name = name,
			 pri = [p],
			 obscurity = obscurity,
			 help = help,
			 ctl = r}
	 in nextpri := p + 1;
	    ControlRegistry.register
		registry
		{ ctl = Controls.stringControl bool_cvt ctl,
		  envName = SOME (ControlUtil.EnvName.toUpper "COMPILER_MC_" name) };
	    r
	end

    val mcdebugging = flag ("matchcomp-debugging", "MatchComp debugging", false)
    val mcstats = flag ("mcstats", "match compiler timing and stats", false)
    val printProtoAndor = flag ("print-protoandor", "andor print mode", false)
    val printAndor = flag ("print-andor", "andor print mode", false)
    val printDecisionTree = flag ("print-decisiontree", "decision tree print mode", false)
    val printMatchAbsyn = flag ("print-match-absyn", "match absyn print mode", false)
    val printMatch = flag ("printMatch", "print argument match", false)
    val printRet = flag ("print-ret", "return print mode", false)

    val bindNoVariableWarn =
	flag ("nobind-warn", "whether to warn if no variables get bound",
	      false)
    val bindNonExhaustiveWarn =
	flag ("warn-non-exhaustive-bind",
	      "whether to warn on non-exhaustive bind", true)
    val bindNonExhaustiveError =
	flag ("error-non-exhaustive-bind",
	      "whether non-exhaustive bind is an error", false)
    val matchNonExhaustiveWarn =
	flag ("warn-non-exhaustive-match",
	      "whether to warn on non-exhaustive match", true)
    val matchNonExhaustiveError =
	flag ("error-non-exhaustive-match",
	      "whether non-exhaustive match is an error", false)
    (* matchExhaustiveError overrides matchExhaustiveWarn *)
    val matchRedundantWarn =
	flag ("warn-redundant", "whether to warn on redundant matches", true)
    val matchRedundantError =
	flag ("error-redundant", "whether a redundant match is an error", true)
    (* matchRedundantError overrides matchRedundantWarn *)

end (* structure Control_MC *)
