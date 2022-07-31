(* power-pc-fn.sml
 * 
 * C calling convention for the OS X Power PC.
 *
 *)

functor PowerPCCConventionFn (
    type reg_id
	
  (* parameter GPRs *)
    val r3 : reg_id
    val r4 : reg_id
    val r5 : reg_id
    val r6 : reg_id
    val r7 : reg_id
    val r8 : reg_id
    val r9 : reg_id
    val r10 : reg_id
  (* parameter FPRs *) 
    val f1 : reg_id
    val f2 : reg_id
    val f3 : reg_id
    val f4 : reg_id
    val f5 : reg_id
    val f6 : reg_id
    val f7 : reg_id
    val f8 : reg_id
    val f9 : reg_id
    val f10 : reg_id
    val f11 : reg_id
    val f12 : reg_id
    val f13 : reg_id

    structure SA : STAGED_ALLOCATION
          where type reg_id = reg_id
          where type loc_kind = CLocKind.loc_kind

  ) = struct

    datatype loc_kind = datatype CLocKind.loc_kind

    fun gpr r = (32, GPR, r)
    fun gprs rs = List.map gpr rs
    fun fpr r = (64, FPR, r)
    fun fprs rs = List.map fpr rs

    val useRegs = #2 o SA.useRegs

    val paramFprs = [f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13]
    val paramGprs = [r3, r4, r5, r6, r7, r8, r9, r10]

  (* conventions for calling a C function *)
    val cStack = SA.freshCounter()
    val cCallGpr = SA.freshCounter()
    val params = [
	  SA.WIDEN (fn w => Int.max(32, w)),
	  SA.BITCOUNTER cCallGpr,
	  SA.CHOICE [
	    (fn (w, k, store) => k = FPR,
	     SA.SEQ [SA.WIDEN(fn w => 64), SA.USEREGS_RESERVE (List.map fpr paramFprs)])
	    (fn (w, k, store) => true,
	     SA.USEREGS_RESERVE (List.map gpr paramGprs))
	  ]
        ]

  (* rules for returning values *)
    val returns = [
	  SA.CHOICE [
	    (fn (w, k, store) => k = FPR, SA.SEQ [
               SA.WIDEN(fn w => 64), useRegs [fpr f1]
	     ]),
	    (fn (w, k, store) => true, SA.SEQ [
               SA.WIDEN(fn w => 32), useRegs [gpr r3, gpr r4]
	     ])

	  ]
	]

  end
