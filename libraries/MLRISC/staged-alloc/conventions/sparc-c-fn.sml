(* sparc-c-fn.sml
 *
 * Calling convention for the Sparc:
 *
 *    Return result:
 *	+ Integer and pointer results are returned in %o0
 *	+ 64-bit integers (long long) returned in %o1/%o1
 *	+ float results are returned in %f0; double in %f0/%f1
 *	+ Struct results are returned in space provided by the caller.
 *	  The address of this space is passed to the callee as a hidden
 *	  implicit argument on the stack (in the caller's frame).  It
 *        gets stored at [%sp+64] (from the caller's point of view).
 *        An UNIMP instruction must be placed after the call instruction,
 *        indicating how much space has been reserved for the return value.
 *      + long double results are returned like structs
 *
 *    Function arguments:
 *      + Arguments that are smaller than a word are promoted to word-size.
 *      + Up to six argument words (words 0-5) are passed in registers
 *        %o0...%o5.  This includes doubles and long longs.  Alignment for
 *        those types is NOT maintained, i.e., it is possible for an 8-byte
 *        quantity to end up in an odd-even register pair.
 *      * Arguments beyond 6 words are passed on the stack in the caller's
 *        frame.  For this, the caller must reserve space in its frame
 *        prior to the call.  Argument word 6 appears at [%sp+92], word 7
 *        at [%sp+96], ...
 *	+ struct arguments are passed as pointers to a copy of the struct.
 *        The copy itself is allocated by the caller in its stack frame.
 *      + long double arguments are passed like structs (i.e., via pointer
 *        to temp copy)
 *      + Space for argument words 0-5 is already allocated in the
 *        caller's frame.  This space might be used by the callee to
 *        save those arguments that must be addressable.  %o0 corresponds
 *        to [%sp+68], %o1 to [%sp+72], ...
 *)

functor SparcCConventionFn (
    type reg_id

  (* parameter GPRs*)
    val r8 : reg_id
    val r9 : reg_id
    val r10 : reg_id
    val r11 : reg_id
    val r12 : reg_id
    val r13 : reg_id
  (* parameter FPRs *)
    val f0 : reg_id
    val f1 : reg_id

    structure SA : STAGED_ALLOCATION
          where type reg_id = reg_id
          where type loc_kind = CLocKind.loc_kind

  ) = struct

    datatype loc_kind = datatype CLocKind.loc_kind

    fun gpr r = (32, GPR, r)
    fun gprs rs = List.map gpr rs
    fun fpr r = (64, FPR, r)
    fun fprs rs = List.map fpr rs

  (* convention for calling a C function *)
    val (cParam, paramRegs) = SA.useRegs (List.map gpr [r8, r9, r10, r12, r13])
    val cStack = SA.freshCounter()
    val params = [
	  SA.WIDEN (fn w => Int.max(32, w)),
	  paramRegs,
	  SA.OVERFLOW{counter=cStack, blockDirection=SA.UP, maxAlign=8}
	]

  (* convention for returning values *)
    val (cFRet, retFlt) = SA.useRegs (List.map fpr [f0, f1])
    val (cRet, retGpr) = SA.useRegs (List.map gpr [r8])
    val return = [
	  SA.WIDEN (fn w => Int.max(32, w)),
	  SA.CHOICE [
	    (fn (w, k, store) => k = FPR, retFlt),
	    (fn (w, k, store) => true, retGpr)
	  ]
	]

  (* initial store *)
    val store0 = SA.init[cStack, cParam, cFRet, cRet]

  end
