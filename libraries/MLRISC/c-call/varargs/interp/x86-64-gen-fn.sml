(* x86-64-gen.sml
 *
 * X86-64-specific portion of the interpreter.
 *)

functor X86_64GenFn (
    structure T : MLTREE
    val push : T.rexp -> T.stm
    val leave : T.stm
  ) = struct

    structure T = T
    structure C = AMD64Cells
    structure CB = CellsBasis
    structure CTy = CType
    structure Gen = GenFn (
	structure T = T
	val gprs = C.rax :: CCall.CCs.gprParamRegs
	val fprs = CCall.CCs.fprParamRegs
	val gprWidths = [8, 16, 32, 64]
	val fprWidths = [32, 64]
	val spReg = T.REG(64, C.rsp)
	val defaultWidth = 64
	val callerSaves = CCall.callerSaveRegs
	val callerSavesF = CCall.callerSaveFRegs)

  (* default bit width *)
    val defTy = 64
    val defSzB = defTy div 8
    fun lit i = T.LI (T.I.fromInt (defTy, i))

  (* get the ith argument in the calling sequence *)
    fun getArg i = T.REG(64, List.nth(CCall.CCs.gprParamRegs, i))

  (* MLRISC code for the x86 64 vararg interpreter *)
    fun gen () = let
	   val largsReg = C.newReg()
	   val frameSzB = 1024*4-2*4
	   val interpFunPtr = getArg 0
	   val endOfLargs = getArg 2
           in
	      List.concat [
	      (* preserve callee-save registers *)
	        [T.LIVE calleeSaveRegs],
              (* the abi specifies that rax contains *at least* the number of floating-point arguments *)
	        [T.MV(defTy, C.rax, lit (List.length CCall.CCs.fprParamRegs))],
		[push (T.REG(64, C.rbp)),
		 T.COPY (defTy, [C.rbp], [C.rsp])],		   
		[T.MV(defTy, argsReg, args)],
	        Gen.gen {interpFunPtr=interpFunPtr, largsReg=largsReg, endOfLargs=endOfLargs},
		[leave],
	        [T.LIVE calleeSaveRegs],
		[T.RET []]
	      ]
	   end

    fun main (cmd, args) = let
	   val _ = Label.reset()
	   val lab = Label.global Consts.varargInterpreter
	   val stms = gen()
	   val asmOutStrm = TextIO.openOut "vararg-interp-x86-64.s"
	   val _ = TextIO.output(asmOutStrm, Consts.header^"\n")
	   fun doit () = X86MLRISCGen.dumpOutput(X86MLRISCGen.codegen'(lab, stms, [T.GPR (T.REG (defTy, C.rax))]))
	   val _ = AsmStream.withStream asmOutStrm doit ()
	   val _ = TextIO.closeOut asmOutStrm
           in
	      0
	   end

  end (* X86_64Gen *)
