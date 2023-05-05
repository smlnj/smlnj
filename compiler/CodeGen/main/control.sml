(* compiler/CodeGen/main/control.sml *)

(* Code generation controls (including some used in FLINT?) *)

structure CodeGenControl : CODEGENCONTROL =
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

    val boxedconstconreps = ElabDataControl.boxedconstconreps
    val spillGen = new (i, "spill-gen", "?", 0)
    val debugRep = new (b, "debug-rep", "?", false)
    val printCFG = new (b, "print-cfg", "whether to convert to CFG and print it", false)
    val dumpCFG = new (b, "dump-cfg", "whether to convert to CFG and pickle it", false)
    val verifyLLVM = new (b, "verify-llvm", "enable verification of generated LLVM code", false)

end (* structure CodeGenControl *)
