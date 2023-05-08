(* compiler/CodeGen/main/control.sml *)

(* Code generation controls (including some used in FLINT?) *)

structure CodeGenControl : CODEGENCONTROL =
struct

  val {newBool, newInt, ...} = MakeControls.make {name = "CodeGen", priority = [1]}

  val boxedconstconreps = ElabDataControl.boxedconstconreps

  val debugRep = newBool ("debug-rep", "?", false)

  val printCFG = newBool ("print-cfg", "whether to convert to CFG and print it", false)

  val dumpCFG = newBool ("dump-cfg", "whether to convert to CFG and pickle it", false)

  val verifyLLVM = newBool ("verify-llvm", "enable verification of generated LLVM code", false)

end (* structure CodeGenControl *)
