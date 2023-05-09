(* compiler/CPS/main/control.sml *)

(* CPS controls (including some used in FLINT?) *)

structure CPSControl : CPSCONTROL =
struct

  val {newBool, newInt, newString, newStrings} = MakeControls.make {name = "CPS", priority = [1]}

  val closureStrategy = newInt ("closure-strategy", "?", 0)	(* see CPS/clos/closure.sml *)
  val cpsopt = newStrings ("cpsopt", "cps optimizer phases",
			  ["first_contract", "eta", "zeroexpand", "last_contract"])
        (* ["first_contract", "eta", "uncurry", "etasplit", cycle_expand", "eta", "last_contract" ] *)
  val rounds = newInt ("rounds", "max # of cpsopt rounds", 10)
  val betacontract = newBool ("betacontract", "?", true)
  val eta = newBool ("eta", "?", true)
  val selectopt = newBool ("selectopt", "enable contraction of record select", true)
  val dropargs = newBool ("dropargs", "?", true)
  val deadvars = newBool ("deadvars", "?", true)
  val flattenargs = newBool ("flattenargs", "?", false)
  val extraflatten = newBool ("extraflatten", "?", false)
  val switchopt = newBool ("switchopt", "?", true)
  val handlerfold = newBool ("handlerfold", "?", true)
  val branchfold = newBool ("branchfold", "?", false)
  val arithopt = newBool ("arithopt", "?", true)
  val betaexpand = newBool ("betaexpand", "?", true)
  val unroll = newBool ("unroll", "?", true)
  val invariant = newBool ("invariant", "?", true)
  val lambdaprop = newBool ("lambdaprop", "?", false)
  val unroll_recur = newBool ("unroll-recur", "?", true)
  val sharepath = newBool ("sharepath", "?", true)
  val staticprof = newBool ("staticprof", "?", false)
  val debugcps = newBool ("debugcps", "?", false)
  val bodysize = newInt ("bodysize", "?", 20)
  val reducemore = newInt ("reducemore", "?", 15)
  val comment = newBool ("comment", "?", false)
  val knownGen = newInt ("known-gen", "?", 0)
  val knownClGen = newInt ("known-cl-gen", "?", 0)
  val escapeGen = newInt ("escape-gen", "?", 0)
  val calleeGen = newInt ("callee-gen", "?", 0)
  val etasplit = newBool ("etasplit", "?", true)
  val uncurry = newBool ("uncurry", "enable uncurrying optimization", true)
  val ifidiom = newBool ("if-idiom", "enable if-idiom optimization", true)
  val comparefold = newBool ("comparefold", "enable optimization of conditional tests", true)
  val debugLits = newBool ("debug-lits", "print results of literal lifting", false)
  val newLiterals = newBool ("new-literals", "use new literal representation", false)
  val deadup = newBool ("deadup", "?", true)
  val printit = newBool ("printit", "whether to show CPS", false)
  val printClusters = newBool ("print-clusters", "whether to print clusters prior to codegen", false)
  val debugSpill = newBool ("debug-cps-spill", "enable CPS spill debugging", false)
  val debugSpillInfo = newBool ("debug_cps_spill_info", "enable CPS spill info", false)

end (* structure CPSControl *)
