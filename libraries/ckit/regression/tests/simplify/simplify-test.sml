local 
  val _ = BuildAst.compilerMode()  (* make sure we are in compiler mode *)
  fun trans ({ast={ast,tidtab,...},aidtab,implicits,...}: BuildAst.programInfo) =
      let 
	val {ast,...} = SimplifyAst.simplifyAst(ast,tidtab,aidtab, implicits)
       in ast
      end
in 
  structure SimplifyTest =
    TestFn (val testDir = "../../valid-programs"
	    val outDir = "results"
	    val trans = trans)
end

