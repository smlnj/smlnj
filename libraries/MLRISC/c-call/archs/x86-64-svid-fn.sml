(* x86-64-svid-fn.sml
 *
 * C calling-sequence generator for x86-64.
 *
 * Mike Rainey (mrainey@cs.uchicago.edu)
 *)

functor X86_64SVIDFn (
    structure T : MLTREE
  ) = struct

    structure T = T
    structure C = AMD64Cells
    structure CB = CellsBasis
    structure CTy = CType

    val wordTy = 64
    val mem = T.Region.memory
    val stack = T.Region.stack

    fun lit i = T.LI (T.I.fromInt (wordTy, i))
    fun gpr r = T.GPR (T.REG (wordTy, r))
    fun fpr (ty, f) = T.FPR (T.FREG (ty, f))
    fun sum ls = List.foldl (op +) 0 ls
    fun szBOfCTy cTy = #sz (CSizes.sizeOfTy cTy)
    fun alignBOfCTy cTy = #align (CSizes.sizeOfTy cTy)
    val spReg = T.REG (wordTy, C.rsp)
    fun offSp 0 = spReg
      | offSp offset = T.ADD (wordTy, spReg, T.LI offset)

    datatype loc_kind = datatype CLocKind.loc_kind

    structure SA = StagedAllocationFn (
                         type reg_id = T.reg
                         datatype loc_kind = datatype loc_kind
			 val memSize = 8 (* bytes *))

    structure Gen = CCallGenFn (
		        structure T = T
			structure C = C
			val wordTy = wordTy
			val offSp = offSp
			structure SA = SA
			fun lobits {e, nBits, width} = e
			fun sx {fromWidth, toWidth, e} = T.SX(toWidth, fromWidth, e)
			fun f2f {fromWidth, toWidth, e} = e
		      )

    datatype c_arg = datatype Gen.c_arg

    structure CCs = X86_64CConventionFn (
		      structure SA = SA
		      type reg_id = T.reg
		      val rax = C.rax
		      val rdi = C.rdi
		      val rsi = C.rsi
		      val rdx = C.rdx
		      val rcx = C.rcx
		      val r8 = C.r8
		      val r9 = C.r9
		      val xmm0 = C.xmm0
		      val xmm1 = C.xmm1
		      val xmm2 = C.xmm2
		      val xmm3 = C.xmm3
		      val xmm4 = C.xmm4
		      val xmm5 = C.xmm5
		      val xmm6 = C.xmm6
		      val xmm7 = C.xmm7)

    fun toGpr r = (wordTy, r)
    fun toGprs gprs = List.map toGpr gprs
    fun toFpr r = (64, r)
    fun toFprs fprs = List.map toFpr fprs 
		      
    val calleeSaveRegs = [C.rbx, C.r12, C.r13, C.r14, C.r15]
    val callerSaveRegs =  [C.rax, C.rcx, C.rdx, C.rsi, C.rdi, C.r8, C.r9, C.r10, C.r11]
    val callerSaveFRegs = (C.Regs CB.FP {from=0, to=15, step=1})
    val calleeSaveFRegs = []
			  
    val frameAlignB = 16

    val calleeSaveRegs'  = toGprs calleeSaveRegs
    val callerSaveRegs'  = toGprs callerSaveRegs
    val calleeSaveFRegs' = toFprs calleeSaveFRegs
    val callerSaveFRegs' = toFprs callerSaveFRegs

  (* convert a list of C types to a list of eight bytes *)
    fun eightBytesOfCTys ([], [], ebs) = List.rev (List.map List.rev ebs)
      | eightBytesOfCTys ([], eb, ebs) = List.rev (List.map List.rev (eb :: ebs))
      | eightBytesOfCTys (cTy :: cTys, eb, ebs) = let
	    val szTy = szBOfCTy cTy
	    val szEb = sum(List.map szBOfCTy eb)
	    in
	       if szTy + szEb = 8
		  then eightBytesOfCTys(cTys, [], (cTy :: eb) :: ebs)
	       else if szTy + szEb < 8
	          then eightBytesOfCTys(cTys, cTy :: eb, ebs)
	       else eightBytesOfCTys(cTys, [cTy], eb :: ebs)
	    end

  (* convert a C type into its eight bytes *)
    fun eightBytesOfCTy cTy = eightBytesOfCTys (CType.flattenCTy cTy, [], [])

  (* classify a C type into its location kind (assuming that aggregates cannot be passed in registers) *)
    fun kindOfCTy (CTy.C_float | CTy.C_double | CTy.C_long_double) = FPR
      | kindOfCTy (CTy.C_ARRAY _ | CTy.C_STRUCT _ | CTy.C_UNION _) = raise Fail "impossible"
      | kindOfCTy (CTy.C_unsigned _ | CTy.C_signed _ | CTy.C_PTR) = GPR

    fun combineKinds (k1, k2) = if (k1 = k2)
	then k1
	else (case (k1, k2)
	       of (STK, _) => STK
		| (_, STK) => STK
		| (GPR, _) => GPR
		| (_, GPR) => GPR
		| _ => FPR
 	      (* end case*))

  (* this part of the ABI is tricky. if the eightbyte contains all floats, we use fprs, but 
   * otherwise we use gprs. *)
    fun kindOfEightByte [] = raise Fail "impossible"
      | kindOfEightByte [cTy] = kindOfCTy cTy
      | kindOfEightByte (cTy1 :: cTy2 :: cTys) = let
	   val k1 = combineKinds (kindOfCTy cTy1, kindOfCTy cTy2)
	   val k2 = kindOfEightByte(cTy2 :: cTys)
           in
	       combineKinds(k1, k2)
	   end

    fun containsUnalignedFields cTy = (case cTy
        of (CTy.C_STRUCT cTys | CTy.C_UNION cTys) => List.exists containsUnalignedFields cTys
	 | cTy => Int.max(8, szBOfCTy cTy) mod 8 <> 0
        (* end case *))

    fun reqsOfCTy (cTy as (CTy.C_STRUCT _ | CTy.C_UNION _ | CTy.C_ARRAY _)) = 
	   if (szBOfCTy cTy > 2*8 orelse containsUnalignedFields cTy)
	      then List.tabulate (szBOfCTy cTy div 8, fn _ => (8*8, STK, 8))
	      else List.map (fn eb => (8*8, kindOfEightByte eb, 8)) (eightBytesOfCTy cTy)
      | reqsOfCTy cTy = [(8*szBOfCTy cTy, kindOfCTy cTy, alignBOfCTy cTy)]

    fun reqOfCTy cTy = (case reqsOfCTy cTy
			  of [req] => req
			   | _ => raise Fail "malformed C type"
			(* end case *))

  (* given a return type, return the locations for the return values *)
    fun layoutReturn retTy = (case retTy
 	    of CTy.C_void => ([], NONE, CCs.store0)
	     | retTy as CTy.C_STRUCT _ => raise Fail "todo"
	     | retTy => let
		   val (locs, store) = SA.allocateSeq CCs.returns (reqsOfCTy retTy, CCs.store0)
	           in
		      (locs, NONE, store)
	           end
            (* end case *))

  (* given a store and some parameters, return the C locations for those parameters *)
    fun layoutCall (store, paramTys) = let
	   val paramReqs = List.map reqsOfCTy paramTys
           in
	      SA.allocateSeqs CCs.params (paramReqs, store)
           end

    fun layout {conv, retTy, paramTys} = let
	   val (resLocs, structRetLoc, store) = layoutReturn retTy
	   val (paramLocss, store) = layoutCall(store, paramTys)
	 (* number of bytes allocated for the call *)
	   val frameSzB = SA.find(store, CCs.cCallStk)
	   val argMem = {szb=CSizes.alignAddr(frameSzB, frameAlignB), align=frameAlignB}
           in
	      {argLocs=paramLocss, argMem=argMem, structRetLoc=structRetLoc, resLocs=resLocs}
	   end

    fun genCall {name, proto, paramAlloc, structRet, saveRestoreDedicated, callComment, args} = let
	val {argLocs, argMem, resLocs, structRetLoc} = layout proto
	val argAlloc = if (#szb argMem = 0 orelse paramAlloc argMem)
			then []
			else [T.MV (wordTy, C.rsp, T.SUB (wordTy, spReg, 
			      T.LI (T.I.fromInt (wordTy, #szb argMem))))]
	val (copyArgs, gprUses, fprUses) = Gen.writeLocs(args, argLocs)
       (* the defined registers of the call depend on the calling convention *)
 	val defs = (case #conv proto
            of "ccall" => List.map (gpr o #2) callerSaveRegs' @ List.map fpr callerSaveFRegs'
	     | "ccall-bare" => []
	     | conv => raise Fail (concat [
			"unknown calling convention \"", String.toString conv, "\""
		      ])
            (* end case *))
	val uses = List.map gpr gprUses @ List.map fpr fprUses
	val callStm = T.CALL {funct=name, targets=[], defs=defs, uses=uses, region=mem, pops=0}
	val (resultRegs, copyResult) = Gen.readLocs resLocs
	val callSeq = argAlloc @ copyArgs @ [callStm] @ copyResult
        in
          {callseq=callSeq, result=resultRegs}
        end

  end (* X86_64SVIDFn *)
