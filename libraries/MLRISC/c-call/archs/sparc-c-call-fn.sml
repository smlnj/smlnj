(* sparc-c-call-fn.sml
 *
 * C function calls for the Sparc
 *
 *)

functor SparcCCallFn (
    structure T : MLTREE
    val ix : (T.stm, T.rexp, T.fexp, T.ccexp) SparcInstrExt.sext
	         -> T.sext
  ) : C_CALL = struct

    structure T = T
    structure Ty = CType
    structure C = SparcCells
    structure IX = SparcInstrExt
    structure CTy = CType

    val wordTy = 32

    val GP = C.GPReg
    val FP = C.FPReg

    fun greg r = GP r
    fun oreg r = GP (r + 8)
    fun ireg r = GP (r + 24)
    fun freg r = FP r
    fun reg32 r = T.REG (32, r)
    fun freg64 r = T.FREG (64, r)
    fun LI i = T.LI (T.I.fromInt (32, i))

    val sp = oreg 6
    val spReg = reg32 sp
    val mem = T.Region.memory
    val stack = T.Region.stack

    fun addli (x, 0) = x
      | addli (x, d) = let
	    val d' = T.I.fromInt (32, d)
	in
	    case x of
		T.ADD (_, r, T.LI d) =>
		T.ADD (32, r, T.LI (T.I.ADD (32, d, d')))
	      | _ => T.ADD (32, x, T.LI d')
	end

    datatype loc_kind = datatype CLocKind.loc_kind

    structure SA = StagedAllocationFn (
                    type reg_id = T.reg
		    datatype loc_kind = datatype loc_kind
		    val memSize = 4)

    structure Gen = CCallGenFn(
             structure T = T
	     structure C = C
	     val wordTy = wordTy
	     fun offSp 0 = spReg
	       | offSp offset = T.ADD (32, spReg, T.LI offset)
	     fun lobits {e, nBits, width} = e
	     fun sx {fromWidth, toWidth, e} = T.SX(toWidth, fromWidth, e)
	     fun f2f {fromWidth, toWidth, e} = e
	     structure SA = SA)

    datatype c_arg = datatype Gen.c_arg

    structure CCs = SparcCConventionFn (
		      type reg_id = T.reg
		      val r8 = oreg 0
		      val r9 = oreg 1
		      val r10 = oreg 2
		      val r11 = oreg 3
		      val r12 = oreg 4
		      val r13 = oreg 5
		      val f0 = freg 0
		      val f1 = freg 1
		      structure SA = SA)

  (* assign a C type to a kind of machine location *)
    fun kindOfCTy (CTy.C_float | CTy.C_double | CTy.C_long_double) = FPR
      | kindOfCTy (CTy.C_unsigned _ | CTy.C_signed _ | CTy.C_PTR | 
		   CTy.C_ARRAY _ | CTy.C_STRUCT _ | CTy.C_UNION _) = GPR

  (* takes a C type and a request for passing values of that type *)
    fun cTyToReq cTy = let
	  val {sz, align} = SparcCSizes.sizeOfTy cTy
          in
	    case cTy
	     of (CTy.C_STRUCT _ | CTy.C_UNION _) => (32, kindOfCTy cTy, align)
	      | (CTy.C_unsigned CTy.I_long_long |
		 CTy.C_signed CTy.I_long_long   ) => raise Fail "todo"
	      | _ => (sz * 8, kindOfCTy cTy, align)
	  end

    fun singleton x = [x]

    fun layout {conv, retTy, paramTys} = let
	  val (resLoc, store) = (
	        case retTy
		 of CTy.C_void => ([], CCs.store0)
		  | _ => let
			val (resLoc, store) = SA.allocate CCs.return (cTyToReq retTy, CCs.store0)
		        in
			  ([resLoc], store)
			end
                (* end case *))
	  val paramReqs = List.map cTyToReq paramTys
	  val (paramLocs, store) = SA.allocateSeq CCs.params (paramReqs, store)
	  val stackdelta = SA.find(store, CCs.cStack)
	  val argMem = {align=stackdelta, szb=stackdelta}
	  in
	     {argLocs=List.map singleton paramLocs, argMem=argMem, structRetLoc=NONE, resLocs=resLoc}
	  end

    local
    val g_regs = List.map greg [1, 2, 3, 4, 5, 6, 7]
    val a_regs = List.map oreg [0, 1, 2, 3, 4, 5]
    val l_reg = oreg 7
    val f_regs = List.map freg
		     [0, 2, 4, 6, 8, 10, 12, 14,
		      16, 18, 20, 22, 24, 26, 28, 30]
    in
    val callerSaveRegs = l_reg :: g_regs @ a_regs
    val calleeSaveRegs = []
    val callerSaveFRegs = f_regs
    val calleeSaveFRegs = []
    end

    fun genCall {name, proto as {retTy, ...}, paramAlloc, structRet, saveRestoreDedicated, callComment, args} = let
	  val {argLocs, argMem, resLocs, structRetLoc} = layout proto
	  val argAlloc = []
	  val (copyArgs, gprUses, fprUses) = Gen.writeLocs(args, argLocs)
	  val (resultRegs, copyResult) = Gen.readLocs resLocs

        (* bytes to allocate on the stack for returning structs *)
	  val res_szal =
	    case retTy of
		(Ty.C_long_double | Ty.C_STRUCT _ | Ty.C_UNION _) =>
		  SOME (SparcCSizes.sizeOfTy retTy)
	      | _ => NONE

	  val (sretsetup, srethandshake) =
	    case res_szal of
		NONE => ([], [])
	      | SOME {sz, align=al} => let
		    val addr = structRet { szb = sz, align = al }
		in
		    ([T.STORE (32, addli (spReg, 64), addr, stack)],
		     [T.EXT (ix (IX.UNIMP sz))])
		end

	  val stackdelta = #szb argMem
	  val (sp_sub, sp_add) =
	      if stackdelta = 0 then ([], []) else
	      if paramAlloc { szb = stackdelta, align = 4 } then ([], [])
	      else ([T.MV (32, sp, T.SUB (32, spReg, LI stackdelta))],
		    [T.MV (32, sp, addli (spReg, stackdelta))])

	  val (defs, uses) = let
	    val gp = T.GPR o reg32
	    val fp = T.FPR o freg64
	    val g_regs = List.map (gp o greg) [1, 2, 3, 4, 5, 6, 7]
	    val a_regs = List.map (gp o oreg) [0, 1, 2, 3, 4, 5]
	    val l_reg = gp (oreg 7)
	    val f_regs = List.map (fp o freg)
			     [0, 2, 4, 6, 8, 10, 12, 14,
			      16, 18, 20, 22, 24, 26, 28, 30]
	    (* a call instruction defines all caller-save registers:
	     *   - %g1 - %g7
	     *   - %o0 - %o5 (argument registers)
	     *   - %o7       (link register)
	     *   - all fp registers *)
	    
	    val defs = g_regs @ a_regs @ l_reg :: f_regs
	    (* A call instruction "uses" just the argument registers. *)
	    val uses = List.map gp gprUses @ List.map fp (List.map #2 fprUses)
            in
	       (defs, uses)
            end

	  val callStm = T.CALL {funct=name, targets=[], defs=defs, uses=uses, region=mem, pops=0}
	  val callseq = sp_sub @ argAlloc @ copyArgs @ [callStm] @ copyResult @ sp_add
          in
	    {callseq=callseq, result=resultRegs}
	  end
  end
