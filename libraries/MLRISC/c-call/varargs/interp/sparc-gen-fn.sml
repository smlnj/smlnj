(* sparc-gen-fn.sml
 *
 * Sparc-specific portion of the interpreter.
 *)

functor SparcGenFn (
	structure Gen : C_CALL_GEN
	structure MLRISCGen : MLRISC_GEN
          where T = Gen.T
	val gprs : Gen.T.reg list
	val fprs : Gen.T.reg list
	val callerSaves : Gen.T.reg list
	val callerSavesF : Gen.T.reg list
	val calleeSaves : Gen.T.reg list
	val calleeSavesF : Gen.T.reg list
	val oRegs : Gen.T.reg list
  ) = struct

    val defTy = 32

    structure T = MLRISCGen.T
    structure C = SparcCells
    structure CB = CellsBasis
    structure Consts = VarargConstants

    fun gpr r = T.REG(32, r)
    fun fpr r = T.FREG(32, r)

    structure InterpGen = GenFn (
	structure T = Gen.T
	val gprs = gprs
	val fprs = fprs
	val gprWidths = [8, 16, 32]
	val fprWidths = [32, 64]
	val defaultWidth = 32
	val spReg = gpr(List.nth(oRegs, 6))
	val callerSaves = callerSaves
	val callerSavesF = callerSavesF
	structure CCallGen = Gen
	structure SA = Gen.SA)

    fun getArg n = T.REG(32, List.nth(oRegs, n))

    val calleeSaves = List.map (T.GPR o gpr) calleeSaves @ List.map (T.FPR o fpr) calleeSavesF

    fun gen () = let
	  val largsReg = C.newReg()
	  val frameSzB = 1024*4-2*4
	  val interpFunPtr = getArg 0
	  val endOfLargs = getArg 2
          in
	     List.concat [

	     (* preserve callee-save registers *)
(*	       [T.LIVE calleeSaves],*)

	       [T.MV(defTy, largsReg, getArg 1)],

	       InterpGen.gen {interpFunPtr=interpFunPtr, largsReg=largsReg, endOfLargs=endOfLargs},

(*	       [T.LIVE calleeSaves],*)

	       [T.RET []]
	     ]
	   end

    fun main (cmd, args) = let
	   val _ = Label.reset()
	   val lab = Label.global Consts.varargInterpreter
	   val stms = gen()
	   val asmOutStrm = TextIO.openOut "vararg-interp-sparc.s"
	   val _ = TextIO.output(asmOutStrm, Consts.header^"\n")
	   fun doit () = MLRISCGen.gen(lab, stms, [T.GPR (T.REG (32, List.nth(oRegs, 0)))])
	   val _ = AsmStream.withStream asmOutStrm doit ()
	   val _ = TextIO.closeOut asmOutStrm
           in
	      0
	   end

  end (* SparcGenFn *)
