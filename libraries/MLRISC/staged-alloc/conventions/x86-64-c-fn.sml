(* x86-64-c-fn.sml
 * 
 * C calling convention for the X86-64 using the System V ABI
 * 
 * Register conventions:
 *   %rax            return value             (caller save)
 *   %rbx            optional base pointer    (callee save)
 *   %rbp            optional frame pointer   (callee save)
 *   %rdi            arg 1                    (caller save)
 *   %rsi            arg 2                    (caller save)
 *   %rdx            arg 3                    (caller save)
 *   %rcx            arg 4                    (caller save)
 *   %r8             arg 5                    (caller save)
 *   %r9             arg 6                    (caller save)
 *   %r10            chain pointer            (caller save)
 *   %r11            scratch                  (caller save)
 *   %r12-r15        general purpose          (callee save)
 *   %xmm0-xmm1      pass/return fp           (caller save)
 *   %xmm2-xmm7      pass fp                  (caller save)
 *   %xmm8-xmm15     scratch                  (caller save)
 *
 * Calling conventions:
 * 
 *    Return result:
 *      + Integer and pointer results are returned in %rax.
 *      + 128-bit integers returned in %rax/%rdx.
 *      + Floating-point results returned in %xmm0/%xmm1.
 *      + Small structs returned in %rax/%rdx. Otherwise, returned
 *        in space provided by the caller.
 *
 *    Function arguments:
 *      + The first 6 integer arguments go in the argument registers.
 *      + The first 8 floating-point registers go in %xmm0-xmm8.
 *      + Otherwise, arguments are pushed on the stack right to left.
 *      + The stack is 16-byte aligned.
 *      + Struct arguments are padded out to word length.
 *)


functor X86_64CConventionFn (
    type reg_id

  (* relevant GPRs *)
    val rax : reg_id
    val rdi : reg_id
    val rsi : reg_id
    val rdx : reg_id
    val rcx : reg_id
    val r8 : reg_id
    val r9 : reg_id
  (* relevant FPRs*)
    val xmm0 : reg_id
    val xmm1 : reg_id
    val xmm2 : reg_id
    val xmm3 : reg_id
    val xmm4 : reg_id
    val xmm5 : reg_id
    val xmm6 : reg_id
    val xmm7 : reg_id

    structure SA : STAGED_ALLOCATION
          where type reg_id = reg_id
          where type loc_kind = CLocKind.loc_kind

  ) = struct

    datatype loc_kind = datatype CLocKind.loc_kind

    fun gpr r = (64, GPR, r)
    fun gprs rs = List.map gpr rs
    fun fpr r = (64, FPR, r)
    fun fprs rs = List.map fpr rs

  (* registers *)
    val gprRetRegs = gprs [rax, rdx]
    val fprRetRegs = fprs [xmm0, xmm1]

    val gprParamRegs = gprs [rdi, rsi, rdx, rcx, r8, r9]
    val fprParamRegs = fprs [xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6, xmm7]

  (* staged-allocation counters *)
    val cCallStk = SA.freshCounter ()
    val cCallGpr = SA.freshCounter ()
    val cCallFpr = SA.freshCounter ()
    val cRetGpr = SA.freshCounter ()
    val cRetFpr = SA.freshCounter ()

  (* max frame alignment in bytes *)
    val maxAlign = 16

  (* initial store *)
    val store0 = SA.init [cCallStk, cCallGpr, cCallFpr, cRetFpr, cRetGpr]

  (* rules for passing arguments *)
    val params = [ 
          SA.WIDEN (fn w => Int.max (64, w)),
	  SA.CHOICE [
	    (fn (w, k, store) => (k = GPR), SA.SEQ [
		SA.BITCOUNTER cCallGpr,
		SA.REGS_BY_BITS (cCallGpr, gprParamRegs) 
	    ]),
	    (fn (w, k, store) => (k = FPR), SA.SEQ [
		SA.BITCOUNTER cCallFpr,
		SA.REGS_BY_BITS (cCallFpr, fprParamRegs) 
	    ]),
	    (fn (w, k, store) => (k = STK orelse k = FSTK),
	     SA.OVERFLOW {counter=cCallStk, 
			  blockDirection=SA.UP, 
			  maxAlign=maxAlign}) 
	  ],
	  SA.OVERFLOW {counter=cCallStk, 
		       blockDirection=SA.UP, 
		       maxAlign=maxAlign}
    ]

  (* rules for returning values *)
    val returns = [
	  SA.WIDEN (fn w => Int.max (64, w)), 
	  SA.CHOICE [
	    (fn (w, k, store) => (k = GPR),
	     SA.SEQ [
		SA.BITCOUNTER cRetGpr,
		SA.REGS_BY_BITS (cRetGpr, gprRetRegs)]),
	    (fn (w, k, store) => (k = FPR),
	     SA.SEQ [
		SA.BITCOUNTER cRetFpr,
	        SA.REGS_BY_BITS (cRetGpr, fprRetRegs)])]
	  ]
  end
