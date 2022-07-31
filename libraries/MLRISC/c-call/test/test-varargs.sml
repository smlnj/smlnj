structure TestVarargs =
  struct

  local
    structure T = X86MLTree
    structure C = X86Cells
  in
    structure X86GenInterp = X86GenFn(
                   fun push e = T.EXT(X86InstrExt.PUSHL e)
		   val leave = T.EXT X86InstrExt.LEAVE
		    structure MLRISCGen =
		      struct
		        structure T = T
		        fun gen (l, stms, mlrs) =
			      X86MLRISCGen.dumpOutput(X86MLRISCGen.codegen'(l, stms, mlrs))
		      end
		    structure Gen = X86CCall.Gen
		  )
  end

  local
    structure T = SparcMLTree
    structure C = SparcCells
    val GP = C.GPReg
    val FP = C.FPReg

    fun greg r = GP r
    fun oreg r = GP (r + 8)
    fun ireg r = GP (r + 24)
    fun freg r = FP r
    fun reg32 r = T.REG (32, r)
    fun freg64 r = T.FREG (64, r)
    fun LI i = T.LI (T.I.fromInt (32, i))
    val g_regs = List.map greg [1, 2, 3, 4, 5, 6, 7]
    val o_regs = List.map oreg [0, 1, 2, 3, 4, 5, 6, 7]
    val a_regs = List.take(o_regs, 6)
    val l_reg = oreg 7
    val f_regs = List.map freg
		     [0, 2, 4, 6, 8, 10, 12, 14,
		      16, 18, 20, 22, 24, 26, 28, 30]
  in
    structure SparcGenInterp = SparcGenFn(
			   structure Gen = SparcCCall.Gen
			   structure MLRISCGen = 
			     struct
			       structure T = T
			       fun gen (l, stms, mlrs) =
				     SparcMLRISCGen.dumpOutput(SparcMLRISCGen.gen(l, stms, mlrs))
			     end
			     val gprs = g_regs
			     val fprs = []
			     val callerSaves = g_regs @ a_regs
			     val calleeSaves = []
			     val callerSavesF = f_regs
			     val calleeSavesF = []
			     val oRegs = o_regs
			 )
  end

    structure VC = VarargCall
    structure V = Vararg
    structure DL = DynLinkage
    fun main's s = DL.lib_symbol (DL.main_lib, s)
    val printf_h = main's "printf"
    fun call args = VC.X86VarargCall.dispatchLowlevelCall(printf_h, args)

    fun ex1 () = call [V.STRING_ARG "test123\n"]
    fun ex2 () = call [V.STRING_ARG "%d %s\n", V.SINT_ARG 1024, V.STRING_ARG "xxx"]
    fun ex3 () = call [V.STRING_ARG "%d %s %d\n", V.SINT_ARG 1024, V.STRING_ARG "xxx", V.SINT_ARG 222333]


  end
