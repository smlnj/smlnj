(* x86-svid-fn.sml
 *
 * C calling-sequence generator for x86.
 *
 * Mike Rainey (mrainey@cs.uchicago.edu)
 *)


functor X86SVIDFn (
    structure T : MLTREE
    val abi : string
    val ix : (T.stm,T.rexp,T.fexp,T.ccexp) X86InstrExt.sext -> T.sext
  (* Note that the fast_loating_point flag must match the one passed
   * to the code generator module.
   *)
    val fast_floating_point : bool ref
  ) : C_CALL = 
  struct

    structure T = T
    structure C = X86Cells
    structure CB = CellsBasis
    structure CTy = CType
    structure IX = X86InstrExt

    val wordTy = 32
    fun gpr r = T.GPR(T.REG(32, r))
    fun fpr f = T.FPR(T.FREG(80, f))
    val spReg = T.REG (32, C.esp)

    (* the C calling convention requires that the FP stack be empty on function
     * entry.  We add the fpStk list to the defs when the fast_floating_point flag
     * is set.
     *)
    val st0 = C.ST 0

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

    structure CCs = X86CConventionFn (
		      type reg_id = T.reg
		      val eax = C.eax
		      val edx = C.edx
		      val st0 = st0
		      structure SA = SA
		    )

    val calleeSaveRegs = [C.ebx, C.esi, C.edi]
    val callerSaveRegs = [C.eax, C.ecx, C.edx]
    val calleeSaveFRegs = []
    val callerSaveFRegs = []

  (* assign a C type to a kind of machine location *)
    fun kindOfCTy (CTy.C_float | CTy.C_double | CTy.C_long_double) = FPR
      | kindOfCTy (CTy.C_unsigned _ | CTy.C_signed _ | CTy.C_PTR | CTy.C_ARRAY _) = GPR

    (* convert a C type to reqs for staged allocation *)
    fun cTyToReqs cTy = let
	val {sz, align} = X86CSizes.sizeOfTy cTy
	(* compute argument reqs for the flattened C type *)
	val reqs = List.map (fn cTy => (sz * 8, kindOfCTy cTy, align))
			    (CTy.flattenCTy cTy)
        in
	  case (cTy, abi)
           of (CTy.C_STRUCT _, "Darwin") => 
	      (* for Darwin, structs <= 8 bytes are returned in GPRs *)
	      if (sz <= 4)
                 then [(8, GPR, align)]
              else if (sz <= 8)
                 then [(8, GPR, align), (8, GPR, align)]
              else reqs
	    | ( (CTy.C_unsigned CTy.I_long_long |
		 CTy.C_signed CTy.I_long_long   ),
		_ ) => 
	      (* 64-bit integers are returned in GPRs *)
	      [(8, GPR, align), (8, GPR, align)]
	    | _ => reqs
        end

  (* compute the parameter passing and return for a given C call *)
    fun layout {conv, retTy, paramTys} = let
	(* lay out the return parameters *)
	val (resLocs, structRetLoc, store) = (case retTy
            of CTy.C_void => ([], NONE, CCs.store0)
	     | retTy as CTy.C_STRUCT _ => raise Fail ""
	     | retTy => let
	       val {sz, align} = X86CSizes.sizeOfTy retTy
	       val (locs, store) = SA.allocateSeq CCs.returns (cTyToReqs retTy, CCs.store0)
	       in
		   (locs, NONE, store)
               end
            (* end case *))

      (* lay out the parameters *)
	val paramReqss = List.map cTyToReqs paramTys
	val (paramLocss, store) = SA.allocateSeqs CCs.params (paramReqss, store)

	(* number of bytes allocated for the call *)
	val cStkSzB = let
             val n = SA.find(store, CCs.cStack)
             in
                if (abi = "Mac OS X")
		   then X86CSizes.alignAddr(n, 16)
                   else n
             end
	in
	   {argLocs=paramLocss, argMem={szb=cStkSzB, align=4}, structRetLoc=structRetLoc, resLocs=resLocs}
        end (* layout *)

    val callerSaveRegs' = List.map gpr calleeSaveRegs
    val calleeSaveRegs' = List.map gpr calleeSaveRegs
    val calleeSaveFRegs' = []
    val callerSaveFRegs' = []

    (* the C calling convention requires that the FP stack be empty on function
     * entry.  We add the fpStk list to the defs when the fast_floating_point flag
     * is set.
     *)
    val fpStk = List.tabulate(8, fn i => fpr (C.ST i))


  (* List of registers defined by a C Call with the given return type; this list
   * is the result registers plus the caller-save registers.
   *)
    fun definedRegs resTy = if !fast_floating_point
	  then let
	    val defs = callerSaveRegs' @ fpStk
	    in
	      case resTy
	       of (CTy.C_unsigned(CTy.I_long_long)) => gpr C.edx :: defs
		| (CTy.C_signed(CTy.I_long_long)) => gpr C.edx :: defs
		| _ => defs
	      (* end case *)
	    end
	  else (case resTy
	     of (CTy.C_float) => fpr st0 :: callerSaveRegs'
	      | (CTy.C_double) => fpr st0 :: callerSaveRegs'
	      | (CTy.C_long_double) => fpr st0 :: callerSaveRegs'
	      | (CTy.C_unsigned(CTy.I_long_long)) => gpr C.edx :: callerSaveRegs'
	      | (CTy.C_signed(CTy.I_long_long)) => gpr C.edx :: callerSaveRegs'
	      | _ => callerSaveRegs'
	    (* end case *))

    fun fstp (32, f) = T.EXT(ix(IX.FSTPS(f)))
      | fstp (64, f) = T.EXT(ix(IX.FSTPL(f)))
      | fstp (80, f) = T.EXT(ix(IX.FSTPT(f)))
      | fstp (sz, f) = raise Fail ("fstp(" ^ Int.toString sz ^ ",_)")

  (* This annotation is used to indicate that a call returns a fp value 
   * in %st(0) 
   *)
    val fpReturnValueInST0 = #create MLRiscAnnotations.RETURN_ARG C.ST0

    fun genCall {
	    name, proto, paramAlloc, structRet, saveRestoreDedicated, callComment, args
	  } = let
	  val {argLocs, argMem, structRetLoc, resLocs} = layout proto

	(* for functions that return a struct/union, pass the location as an
	 * implicit first argument.  Because the callee removes this implicit
	 * argument from the stack, we must also keep track of the size of the
	 * explicit arguments.
	 *)
	  val (args, argLocs, explicitArgSzB) = (case structRetLoc
		 of SOME pos => 
		      (ARG(structRet pos)::args, [SA.BLOCK_OFFSET(wordTy, GPR, 0)]::argLocs, #szb argMem)
		  | NONE => (args, argLocs, #szb argMem)
		(* end case *))

	(* instruction to allocate space for arguments *)
	  val argAlloc = if (#szb argMem = 0 orelse paramAlloc argMem)
	        then []
                else if abi = "Darwin"	
		      then let
		       (* align the frame on a 16-byte boundary *)
			val szb = X86CSizes.alignAddr(#szb argMem + 2*4, 16)-2*4
		        in
			  [T.MV(wordTy, C.esp, T.SUB(wordTy, spReg, T.LI(IntInf.fromInt szb)))]
			end
		else [T.MV(wordTy, C.esp, T.SUB(wordTy, spReg, T.LI(IntInf.fromInt(#szb argMem))))]
	  val (copyArgs, gprUses, fprUses) = Gen.writeLocs(args, argLocs)

	(* the SVID specifies that the caller pops arguments, but the callee
	 * pops the arguments in a stdcall on Windows.  I'm not sure what other
	 * differences there might be between the SVID and Windows ABIs. (JHR)
	 *)
	  val calleePops = (case #conv proto
		 of (""|"ccall") => false
		  | "stdcall" => true
		  | conv => raise Fail (concat [
			"unknown calling convention \"", String.toString conv, "\""
		      ])
		(* end case *))

	(* code to pop the arguments from the stack *)
	  val popArgs = if calleePops orelse (explicitArgSzB = 0)
		then []
		else [T.MV(wordTy, C.esp, T.ADD(wordTy, spReg, T.LI(IntInf.fromInt explicitArgSzB)))]

        (* FIXME: support fast floating point *)
	(* read return values *) 
	  val (resultRegs, copyResult) = Gen.readLocs resLocs

	  val defs = definedRegs(#retTy proto)
	  val { save, restore } = saveRestoreDedicated defs

	  val callStm = T.CALL{
		  funct=name, targets=[], defs=defs, uses=[], 
		  region = T.Region.memory,
		  pops = if calleePops
		      then Int32.fromInt(#szb argMem)
		      else Int32.fromInt(#szb argMem - explicitArgSzB)
		}
	  val callStm = (case callComment
		 of NONE => callStm
		  | SOME c => T.ANNOTATION (callStm, #create MLRiscAnnotations.COMMENT c)
		(* end case *))

	  val callStm = if !fast_floating_point
		andalso ((#retTy proto = CTy.C_float)
		  orelse (#retTy proto = CTy.C_double)
		  orelse (#retTy proto = CTy.C_long_double))
		then T.ANNOTATION(callStm, fpReturnValueInST0)
		else callStm

	(* assemble the call sequence *)
	  val callSeq = argAlloc @ copyArgs @ save @ [callStm] @ restore @ popArgs @ copyResult

          in
   	    {callseq=callSeq, result=resultRegs}
          end

  end (* X86SVIDFn *)

