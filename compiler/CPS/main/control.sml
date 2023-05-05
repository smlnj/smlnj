(* compiler/CPS/main/control.sml *)

(* CPS controls (including some used in FLINT?) *)

structure CPSControl : CPSCONTROL =
struct

    val priority = [10, 11, 2]
    val obscurity = 6
    val prefix = "cg"

    val registry = ControlRegistry.new { help = "code generator settings" }

    val _ = BasicControl.nest (prefix, registry, priority)

    val b = ControlUtil.Cvt.bool
    val i = ControlUtil.Cvt.int
    val r = ControlUtil.Cvt.real
    val sl = ControlUtil.Cvt.stringList

    val nextpri = ref 0

    fun new (c, n, h, d) = let
	  val r = ref d
	  val p = !nextpri
	  val ctl = Controls.control {
		  name = n,
		  pri = [p],
		  obscurity = obscurity,
		  help = h,
		  ctl = r }
	  in
	    nextpri := p + 1;
	    ControlRegistry.register
		registry
		{ ctl = Controls.stringControl c ctl,
		  envName = SOME (ControlUtil.EnvName.toUpper "CG_" n) };
	    r
	  end

val closureStrategy = new (i, "closure-strategy", "?", 0)	(* see CPS/clos/closure.sml *)
val cpsopt = new (sl, "cpsopt", "cps optimizer phases",
		      ["first_contract", "eta", "zeroexpand", "last_contract"])
           (* ["first_contract", "eta", "uncurry", "etasplit", cycle_expand", "eta", "last_contract" ] *)
val rounds = new (i, "rounds", "max # of cpsopt rounds", 10)
val betacontract = new (b, "betacontract", "?", true)
val eta = new (b, "eta", "?", true)
val selectopt = new (b, "selectopt", "enable contraction of record select", true)
val dropargs = new (b, "dropargs", "?", true)
val deadvars = new (b, "deadvars", "?", true)
val flattenargs = new (b, "flattenargs", "?", false)
val extraflatten = new (b, "extraflatten", "?", false)
val switchopt = new (b, "switchopt", "?", true)
val handlerfold = new (b, "handlerfold", "?", true)
val branchfold = new (b, "branchfold", "?", false)
val arithopt = new (b, "arithopt", "?", true)
val betaexpand = new (b, "betaexpand", "?", true)
val unroll = new (b, "unroll", "?", true)
val invariant = new (b, "invariant", "?", true)
val lambdaprop = new (b, "lambdaprop", "?", false)
val unroll_recur = new (b, "unroll-recur", "?", true)
val sharepath = new (b, "sharepath", "?", true)
val staticprof = new (b, "staticprof", "?", false)
val debugcps = new (b, "debugcps", "?", false)
val bodysize = new (i, "bodysize", "?", 20)
val reducemore = new (i, "reducemore", "?", 15)
val comment = new (b, "comment", "?", false)
val knownGen = new (i, "known-gen", "?", 0)
val knownClGen = new (i, "known-cl-gen", "?", 0)
val escapeGen = new (i, "escape-gen", "?", 0)
val calleeGen = new (i, "callee-gen", "?", 0)
val etasplit = new (b, "etasplit", "?", true)
val uncurry = new (b, "uncurry", "enable uncurrying optimization", true)
val ifidiom = new (b, "if-idiom", "enable if-idiom optimization", true)
val comparefold = new (b, "comparefold", "enable optimization of conditional tests", true)
val debugLits = new (b, "debug-lits", "print results of literal lifting", false)
val newLiterals = new (b, "new-literals", "use new literal representation", false)
val deadup = new (b, "deadup", "?", true)
val printit = new (b, "printit", "whether to show CPS", false)
val printClusters = new (b, "print-clusters", "whether to print clusters prior to codegen", false)
val debugSpill = new (b, "debug-cps-spill", "enable CPS spill debugging", false)
val debugSpillInfo = new (b, "debug_cps_spill_info", "enable CPS spill info", false)

end (* structure CPSControl *)
