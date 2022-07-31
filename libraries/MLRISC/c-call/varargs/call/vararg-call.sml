(* vararg-call.sml
 *
 * Infrastructure for calling varargs functions for each supported architecture.
 *)

structure VarargCall =
  struct

    val regNum = CellsBasis.physicalRegisterNum

  (* x86 *)
    local
    structure X86SA = StagedAllocationFn (
                         type reg_id = int
			 datatype loc_kind = datatype CLocKind.loc_kind
			 val memSize = 8)
    structure X86Convention = X86CConventionFn(
			        type reg_id = int
				val eax = regNum X86Cells.eax
				val edx = regNum X86Cells.edx
				val st0 = regNum X86Cells.ST0
				structure SA = X86SA)
    in
    structure X86VarargCall = VarargCallFn (
			          val params = X86Convention.params
				  val returns = X86Convention.returns
				  val store0 = X86Convention.store0
				  val bitWidthOfPointer = 32
				  val alignBOfPointer = 4
				  val alignBOfInt = 4
				  val alignBOfDouble = 4
				  val kindOfInt = CLocKind.GPR
				  val kindOfPointer = CLocKind.GPR
				  val kindOfDouble = CLocKind.FPR
				  structure SA = X86SA
			      )
    end (* x86 *)

  (* x86 64 *)
    local
    structure X86_64SA = StagedAllocationFn (
                         type reg_id = int
			 datatype loc_kind = datatype CLocKind.loc_kind
			 val memSize = 8)
    structure X86_64Convention = X86_64CConventionFn(
			        type reg_id = int
				val rax = regNum AMD64Cells.rax
				val rdi = regNum AMD64Cells.rdi
				val rsi = regNum AMD64Cells.rsi
				val rdx = regNum AMD64Cells.rdx
				val rcx = regNum AMD64Cells.rcx
				val r8 = regNum AMD64Cells.r8
				val r9 = regNum AMD64Cells.r9
				val xmm0 = regNum AMD64Cells.xmm0
				val xmm1 = regNum AMD64Cells.xmm1
				val xmm2 = regNum AMD64Cells.xmm2
				val xmm3 = regNum AMD64Cells.xmm3
				val xmm4 = regNum AMD64Cells.xmm4
				val xmm5 = regNum AMD64Cells.xmm5
				val xmm6 = regNum AMD64Cells.xmm6
				val xmm7 = regNum AMD64Cells.xmm7
				structure SA = X86_64SA)
    in
    structure X86_64VarargCall = VarargCallFn (
			          val params = X86_64Convention.params
				  val returns = X86_64Convention.returns
				  val store0 = X86_64Convention.store0
				  val bitWidthOfPointer = 64
				  val alignBOfPointer = 8
				  val alignBOfInt = 8
				  val alignBOfDouble = 8
				  val kindOfInt = CLocKind.GPR
				  val kindOfPointer = CLocKind.GPR
				  val kindOfDouble = CLocKind.FPR
				  structure SA = X86_64SA
			      )
    end (* x86 64 *)

  (* sparc *)
    local
    structure SparcSA = StagedAllocationFn (
                         type reg_id = int
			 datatype loc_kind = datatype CLocKind.loc_kind
			 val memSize = 4)
    val GP = regNum o SparcCells.GPReg
    val FP = regNum o SparcCells.FPReg
    fun freg r = FP r
    fun oreg r = GP (r + 8)
    structure SparcConvention = SparcCConventionFn(
			        type reg_id = int
				val r8 = oreg 0
				val r9 = oreg 1
				val r10 = oreg 2
				val r11 = oreg 3
				val r12 = oreg 4
				val r13 = oreg 5
				val f0 = freg 0
				val f1 = freg 1
				structure SA = SparcSA)
    in
    structure SparcVarargCall = VarargCallFn (
			          val params = SparcConvention.params
				  val returns = SparcConvention.return
				  val store0 = SparcConvention.store0
				  val bitWidthOfPointer = 32
				  val alignBOfPointer = 4
				  val alignBOfInt = 4
				  val alignBOfDouble = 4
				  val kindOfInt = CLocKind.GPR
				  val kindOfPointer = CLocKind.GPR
				  val kindOfDouble = CLocKind.FPR
				  structure SA = SparcSA
			      )
    end (* sparc *)

  end
