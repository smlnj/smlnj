structure TypecheckTest =
  TestFn (val testDir = "../../valid-programs"
	  val outDir = "results"
	  val trans = (fn ({ast,...}: BuildAst.astBundle) => ast))
