(* compiler/CodeGen/main/control.sig *)

(* general code-generation controls *)

signature CODEGENCONTROL =
sig

    val boxedconstconreps : bool ref
    val spillGen : int ref
    val debugRep : bool ref
    val printCFG : bool ref
    val dumpCFG : bool ref
    val verifyLLVM : bool ref

end (* signature CODEGENCONTROL *)
