(* x86-c-fn.sml
 *
 *
 * C function calls for the X86 using the System V ABI
 *
 * Register conventions:
 *
 *    %eax	return value		(caller save)
 *    %ebx	global offset for PIC	(callee save)
 *    %ecx	scratch			(caller save)
 *    %edx	extra return/scratch	(caller save)
 *    %ebp	optional frame pointer	(callee save)
 *    %esp	stack pointer		(callee save)
 *    %esi	locals			(callee save)
 *    %edi	locals			(callee save)
 *
 *    %st(0)	top of FP stack; FP return value
 *    %st(1..7)	FP stack; must be empty on entry and return
 *
 * Calling convention:
 *
 *    Return result:
 *	+ Integer and pointer results are returned in %eax.  Small
 *	  integer results are not promoted.
 *	+ 64-bit integers (long long) returned in %eax/%edx
 *	+ Floating point results are returned in %st(0) (all types).
 *	+ Struct results are returned in space provided by the caller.
 *	  The address of this space is passed to the callee as an
 *	  implicit 0th argument, and on return %eax contains this
 *	  address.  The called function is responsible for removing
 *	  this argument from the stack using a "ret $4" instruction.
 *	  NOTE: the MacOS X ABI returns small structs in %eax/%edx.
 *
 *    Function arguments:
 *	+ Arguments are pushed on the stack right to left.
 *	+ Integral and pointer arguments take one word on the stack.
 *	+ float arguments take one word on the stack.
 *	+ double arguments take two words on the stack.  The i386 ABI does
 *	  not require double word alignment for these arguments.
 *	+ long double arguments take three words on the stack.
 *	+ struct arguments are padded out to word length.
 *
 *)

functor X86CConventionFn (
    type reg_id
    val eax : reg_id
    val edx : reg_id
    val st0 : reg_id
    structure SA : STAGED_ALLOCATION
           where type reg_id = reg_id
           where type loc_kind = CLocKind.loc_kind
  ) = struct

    datatype loc_kind = datatype CLocKind.loc_kind

    fun gpr r = (32, GPR, r)
    fun gprs rs = List.map gpr rs
    fun fpr r = (80, FPR, r)
    fun fprs rs = List.map fpr rs

  (* conventions for calling a C function *)
    val maxAlign = 16
    val cStack = SA.freshCounter()
    val params = [
	SA.SEQ[
	SA.WIDEN (fn ty => Int.max(32, ty)),
	SA.OVERFLOW {counter=cStack, blockDirection=SA.UP, maxAlign=maxAlign}
	]
    ]

  (* conventions for returning from a C call *)
    val (cInt, retInGpr) = SA.useRegs (gprs [eax, edx])
    val (cFloat, retInFpr) = SA.useRegs (fprs [st0])
    val returns = [
  	  SA.CHOICE [
	  (* return in general-purpose register *)
	  (fn (ty, k, store) => k = GPR, 
	   SA.SEQ [SA.WIDEN (fn ty => Int.max(ty, 32)),
		   retInGpr]),
	  (* return in floating-point register *)
	  (fn (ty, k, store) => k = FPR,
	   SA.SEQ [SA.WIDEN (fn ty => 80), retInFpr])
	]
    ]
		       
  (* initial store *)
    val store0 = SA.init [cInt, cFloat, cStack]
	       
end (* X86CConventionsFn *)
