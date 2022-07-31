(* ppc-macosx.sml
 *
 * COPYRIGHT (c) 2003 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * C function calls for the PowerPC using the MacOS X ABI.
 *
 * Register conventions:
 *
 *    Register	Callee-save	Purpose
 *    --------	-----------	-------
 *	GPR0	   no		Zero
 *	 1	   no		Stack pointer
 *	 2	   no		scratch (TOC on AIX)
 *	 3	   no		arg0 and return result
 *	4-10	   no		arg1-arg7
 *	 11	   no		scratch
 *	 12	   no		holds taget of indirect call
 *	13-31	   yes		callee-save registers
 *
 *	FPR0	   no		scratch
 *	1-13	   no		floating-point arguments
 *	14-31	   yes		floating-point callee-save registers
 *
 *	V0-V1	   no		scratch vector registers
 *	 2-13	   no		vector argument registers
 *	14-19	   no		scratch vector registers
 *	20-31	   yes		callee-save vector registers
 *
 *	 LR	   no		link register holds return address
 *
 *	CR0-CR1	   no		scratch condition registers
 *	 2-4	   yes		callee-save condition registers
 *	 5-7	   no		scratch condition registers
 *
 * Calling convention:
 *
 *    Return result:
 *	+ Integer and pointer results are returned in GPR3
 *	+ 64-bit integers (long long) returned in GPR3/GPR4
 *	+ float/double results are returned in FPR1
 *	+ Struct results are returned in space provided by the caller.
 *	  The address of this space is passed to the callee as an
 *	  implicit first argument in GPR3 and the first real argument is
 *	  passed in GPR4.
 *
 *    Function arguments:
 *	* arguments (except for floating-point values) are passed in
 *	  registers GPR3-GPR10
 *
 * Note also that stack frames are supposed to be 16-byte aligned.
 *)

(* we extend the interface to support generating the stubs needed for
 * dynamic linking (see "Inside MacOS X: Mach-O Runtime Architecture"
 * for details.
 *)
signature PPC_MACOSX_C_CALLS =
  sig
    include C_CALLS

(*
    val genStub : {
	    name  : T.rexp,
            proto : CTypes.c_proto,
	    paramAlloc : {szb : int, align : int} -> bool,
            structRet : {szb : int, align : int} -> T.rexp,
	    saveRestoreDedicated :
	      T.mlrisc list -> {save: T.stm list, restore: T.stm list},
	    callComment : string option,
            args : c_arg list
	  } -> {
	    callseq : T.stm list,
	    result: T.mlrisc list
	  }
*)

  end;

functor PPCMacOSX_CCalls (

    structure T : MLTREE

  ): C_CALLS = struct

    structure T  = T
    structure CTy = CTypes
    structure C = PPCCells

    fun error msg = MLRiscErrorMsg.error ("PPCCompCCalls", msg)

  (* the location of arguments/parameters; offsets are given with respect to the
   * low end of the parameter area.
   *)
    datatype arg_location
      = Reg of T.ty * T.reg * T.I.machine_int option
					(* integer/pointer argument in register *)
      | FReg of T.fty * T.reg * T.I.machine_int option
					(* floating-point argument in register *)
      | Stk of T.ty * T.I.machine_int	(* integer/pointer argument in parameter area *)
      | FStk of T.fty * T.I.machine_int	(* floating-point argument in parameter area *)
      | Args of arg_location list

    val wordTy = 32
    val fltTy = 32	(* MLRISC type of float *)
    val dblTy = 64	(* MLRISC type of double *)

  (* shorts and chars are promoted to 32-bits *)
    val naturalIntSz = wordTy

  (* stack pointer *)
    val spReg = T.REG(wordTy, C.GPReg 1)

  (* registers used for parameter passing *)
    val argGPRs = List.map C.GPReg [3, 4, 5, 6, 7, 8, 9, 10]
    val argFPRs = List.map C.FPReg [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13]
    val resGPR = C.GPReg 3
    val resGPR2 = C.GPReg 4
    val resRegLoc = Reg(wordTy, resGPR, NONE)
    val resRegLoc2 = Reg(wordTy, resGPR2, NONE)
    val resRegLocPair = Args[resRegLoc, resRegLoc2]
    val resFPR = C.FPReg 1

  (* C callee-save registers *)
    val calleeSaveRegs = List.map C.GPReg [
	    13, 14, 15, 16, 17, 18, 19, 20, 21, 22,
	    23, 24, 25, 26, 27, 28, 29, 30, 31
	  ]
    val calleeSaveFRegs = List.map C.FPReg [
	    14, 15, 16, 17, 18, 19, 20, 21, 22,
	    23, 24, 25, 26, 27, 28, 29, 30, 31
	  ]

  (* C caller-save registers (including argument registers) *)
    val callerSaveRegs =
	  T.FPR(T.FREG(dblTy, C.FPReg 0)) ::
	    (List.map (fn r => T.GPR(T.REG(wordTy, C.GPReg r))) [2, 11, 12])

    val linkReg = T.GPR(T.REG(wordTy, C.lr))

  (* the parameter area lies just above the linkage area in the caller's frame.
   * The linkage area is 24 bytes, so the first parameter is at 24(sp).
   *)
    val paramAreaOffset = 24

  (* size, padding, and natural alignment for integer types.  Note that the
   * padding is based on the parameter-passing description on p. 35 of the
   * documentation and the alignment is from p. 31.
   *)
    fun sizeOfInt CTy.I_char = {sz = 1, pad = 3, align = 1}
      | sizeOfInt CTy.I_short = {sz = 2, pad = 2, align = 2}
      | sizeOfInt CTy.I_int = {sz = 4, pad = 0, align = 4}
      | sizeOfInt CTy.I_long = {sz = 4, pad = 0, align = 4}
      | sizeOfInt CTy.I_long_long = {sz = 8, pad = 0, align = 8}

  (* sizes of other C types *)
    val sizeOfPtr = {sz = 4, pad = 0, align = 4}

  (* align the address to the given alignment, which must be a power of 2 *)
    fun alignAddr (addr, align) = let
	  val mask = Word.fromInt(align-1)
	  in
	    Word.toIntX(Word.andb(Word.fromInt addr + mask, Word.notb mask))
	  end

  (* compute the size and alignment information for a struct; tys is the list
   * of member types.  The alignment is what Apple calls the "embedding" alignment.
   * The total size is padded to agree with the struct's alignment.
   *)
    fun sizeOfStruct tys = let
	  fun ssz [] = {sz = 0, align = 1}
	    | ssz (first::rest) = let
		fun f ([], maxAlign, offset) =
		      {sz = alignAddr(offset, maxAlign), align = maxAlign}
		  | f (ty::tys, maxAlign, offset) = let
			val {sz, align} = sizeOfTy ty
			val align = Int.min(align, 4)
			val offset = alignAddr(offset, align)
			in
			  f (tys, Int.max(maxAlign, align), offset+sz)
			end
		val {sz, align} = sizeOfTy first
		in
		  f (rest, align, sz)
		end
	  in
	    ssz tys
	  end

  (* the size alignment of a union type is the maximum of the sizes and alignments of the
   * members.  The final size is padded to agree with the alignment.
   *)
    and sizeOfUnion tys = let
	  fun usz [] = {sz = 0, align = 1}
	    | usz (first::rest) = let
		fun f ([], maxAlign, maxSz) =
		      {sz = alignAddr(maxSz, maxAlign), align = maxAlign}
		  | f (ty::tys, maxAlign, maxSz) = let
			val {sz, align} = sizeOfTy ty
			in
			  f (tys, Int.max(maxAlign, align), Int.max(align, maxAlign))
			end
		val {sz, align} = sizeOfTy first
		in
		  f (rest, align, sz)
		end
	  in
	    usz tys
	  end

    and sizeOfTy CTy.C_void = error "unexpected void argument type"
      | sizeOfTy CTy.C_float = {sz = 4, align = 4}
      | sizeOfTy CTy.C_double = {sz = 8, align = 8}
      | sizeOfTy CTy.C_long_double = {sz = 8, align = 8}
      | sizeOfTy (CTy.C_unsigned isz) = let
	  val {sz, align, ...} = sizeOfInt isz
	  in
	    {sz = sz, align = align}
	  end
      | sizeOfTy (CTy.C_signed isz) = let
	  val {sz, align, ...} = sizeOfInt isz
	  in
	    {sz = sz, align = align}
	  end
      | sizeOfTy CTy.C_PTR = {sz = 4, align = 4}
      | sizeOfTy (CTy.C_ARRAY(ty, n)) = let
	  val {sz, align} = sizeOfTy ty
	  in
	    {sz = n*sz, align = align}
	  end
      | sizeOfTy (CTy.C_STRUCT tys) = sizeOfStruct tys
      | sizeOfTy (CTy.C_UNION tys) = sizeOfUnion tys

  (* compute the layout of a C call's arguments *)
    fun layout {conv, retTy, paramTys} = let
	  fun gprRes isz = (case #sz(sizeOfInt isz)
		 of 8 => SOME resRegLocPair
		  | _ => SOME resRegLoc
		(* end case *))
	  val (resLoc, argGPRs, structRet) = (case retTy
		 of CTy.C_void => (NONE, argGPRs, NONE)
		  | CTy.C_float => (SOME(FReg(fltTy, resFPR, NONE)), argGPRs, NONE)
		  | CTy.C_double => (SOME(FReg(dblTy, resFPR, NONE)), argGPRs, NONE)
		  | CTy.C_long_double => (SOME(FReg(dblTy, resFPR, NONE)), argGPRs, NONE)
		  | CTy.C_unsigned isz => (gprRes isz, argGPRs, NONE)
		  | CTy.C_signed isz => (gprRes isz, argGPRs, NONE)
		  | CTy.C_PTR => (SOME resRegLoc, argGPRs, NONE)
		  | CTy.C_ARRAY _ => error "array return type"
		  | CTy.C_STRUCT s => let
		      val sz = #sz(sizeOfStruct s)
		      in
		      (* Note that this is a place where the MacOS X and Linux ABIs differ.
		       * In Linux, GPR3/GPR4 are used to return composite values of 8 bytes.
		       *)
			(SOME resRegLoc, List.tl argGPRs, SOME{szb=sz, align=4})
		      end
		  | CTy.C_UNION u => let
		      val sz = #sz(sizeOfUnion u)
		      in
			(SOME resRegLoc, List.tl argGPRs, SOME{szb=sz, align=4})
		      end
		(* end case *))
	  fun assign ([], offset, _, _, layout) = (offset, List.rev layout)
	    | assign (ty::tys, offset, availGPRs, availFPRs, layout) = (
		case ty
		 of CTy.C_void => error "unexpected void argument type"
		  | CTy.C_float => (case (availGPRs, availFPRs)
		       of (_::gprs, fpr::fprs) =>
			    assign (tys, offset+4, gprs, fprs, FReg(fltTy, fpr, SOME offset)::layout)
			| ([], fpr::fprs) =>
			    assign (tys, offset+4, [], fprs, FReg(fltTy, fpr, SOME offset)::layout)
			| ([], []) =>
			    assign (tys, offset+4, [], [], FStk(fltTy, offset)::layout)
			| _ => error "FPRs exhausted before GPRs"
		      (* end case *))
		  | CTy.C_double =>
		      assignFPR (tys, offset, availGPRs, availFPRs, layout)
		  | CTy.C_long_double =>
		      assignFPR (tys, offset, availGPRs, availFPRs, layout)
		  | (CTy.C_unsigned isz | CTy.C_signed isz) =>
		      assignGPR([sizeOfInt isz], tys, offset, availGPRs, availFPRs, layout)
		  | CTy.C_PTR =>
		      assignGPR([sizeOfPtr], tys, offset, availGPRs, availFPRs, layout)
		  | CTy.C_ARRAY _ =>
		      assignGPR([sizeOfPtr], tys, offset, availGPRs, availFPRs, layout)
		  | CTy.C_STRUCT tys' =>
		      assignMem(sizeOfStruct tys', tys, offset, availGPRs, availFPRs, layout)
		  | CTy.C_UNION tys' => 
		      assignMem(sizeOfUnion tys', tys, offset, availGPRs, availFPRs, layout)
		(* end case *))
	(* assign a GP register and memory for an integer/pointer argument. *)
	  and assignGPR ([], args, offset, availGPRs, availFPRs, layout) =
	        assign (args, offset, availGPRs, availFPRs, layout)
	    | assignGPR ({ sz = 8, ... } :: szs,
			 args, offset, availGPRs, availFPRs, layout) =
	        (* The C compiler seems to treat "long long" arguments
		 * as two individual 4-byte arguments.  There seems to be
		 * no 8-byte alignment requirement, as far as I can tell.
		 *   - Matthias *)
	        assignGPR ({ sz = 4, pad = 0, align = 4 } ::
			   { sz = 4, pad = 0, align = 4 } :: szs,
			   args, offset, availGPRs, availFPRs, layout)
	    | assignGPR ({ sz, pad, ... } :: szs,
			 args, offset, availGPRs, availFPRs, layout) =
	        let val (loc, availGPRs) =
			case availGPRs
			 of [] => (Stk(wordTy, offset), [])
			  | r1 :: rs => (Reg(wordTy, r1, SOME offset), rs)
		    val offset = offset + IntInf.fromInt (sz + pad)
		in assignGPR (szs, args, offset, availGPRs, availFPRs, loc :: layout)
		end
								       
	(* assign a FP register and memory/GPRs for double-precision argument. *)
	  and assignFPR (args, offset, availGPRs, availFPRs, layout) = let
		fun continue (availGPRs, availFPRs, loc) =
		      assign (args, offset+8, availGPRs, availFPRs, loc::layout)
		fun freg fpr = FReg(dblTy, fpr, SOME offset)
		in
		  case (availGPRs, availFPRs)
		   of (_::_::gprs, fpr::fprs) => continue (gprs, fprs, freg fpr)
		    | (_, fpr::fprs) => continue ([], fprs, freg fpr)
		    | ([], []) => continue ([], [], FStk(dblTy, offset))
		    | _ => error "FPRs exhausted before GPRs"
		  (* end case *)
		end
	(* assign a argument locations to pass a composite argument (struct or union) *)
	  and assignMem ({sz, ...}, args, offset, availGPRs, availFPRs, layout) = let
		val sz = IntInf.fromInt sz
		fun assignMem (relOffset, availGPRs, fields) =
		      if (relOffset < sz)
			then let
			  val (loc, availGPRs) = (case availGPRs
				 of [] => (Stk(wordTy, offset+relOffset), [])
				  | r1::rs => (Reg(wordTy, r1, SOME(offset+relOffset)), rs)
				(* end case *))
			  in
			    assignMem (relOffset+4, availGPRs, loc::fields)
			  end
			else assign (args, offset+relOffset, availGPRs, availFPRs,
			    Args(List.rev fields) :: layout)
		in
		  assignMem (0, availGPRs, [])
		end
	  val (sz, argLocs) = assign (paramTys, 0, argGPRs, argFPRs, [])
	  in {
	    argLocs = argLocs,
	    argMem = {szb = IntInf.toInt sz, align = 4},
	    resLoc = resLoc,
	    structRetLoc = structRet
	  } end

    datatype c_arg
      = ARG of T.rexp	    
      | FARG of T.fexp
      | ARGS of c_arg list

    val memRg = T.Region.memory
    val stkRg = T.Region.memory

  (* SP-based address of parameter at given offset *)
    fun paramAddr off =
	  T.ADD(wordTy, spReg, T.LI(off + IntInf.fromInt paramAreaOffset))

    fun genCall {
	  name, proto, paramAlloc, structRet, saveRestoreDedicated,
	  callComment, args
	} = let
	  val {conv, retTy, paramTys} = proto
	  val {argLocs, argMem, resLoc, structRetLoc} = layout proto
	(* inform the client of the size of the parameter area *)
	  val _ = if not(paramAlloc argMem)
		then raise Fail "parameter memory allocation not implemented yet"
		else ()
	(* generate code to assign the arguments to their locations *)
	  fun assignArgs ([], [], stms) = stms
	    | assignArgs (Reg(ty, r, _) :: locs, ARG exp :: args, stms) =
		assignArgs (locs, args, T.MV(ty, r, exp) :: stms)
	    | assignArgs (Stk(ty, off) :: locs, ARG exp :: args, stms) =
		assignArgs (locs, args, T.STORE(ty, paramAddr off, exp, stkRg) :: stms)
	    | assignArgs (FReg(ty, r, _) :: locs, FARG fexp :: args, stms) =
		assignArgs (locs, args, T.FMV(ty, r, fexp) :: stms)
	    | assignArgs (FStk(ty, off) :: locs, FARG fexp :: args, stms) =
		assignArgs (locs, args, T.FSTORE(ty, paramAddr off, fexp, stkRg) :: stms)
	    | assignArgs ((Args locs') :: locs, (ARGS args') :: args, stms) =
		raise Fail "ARGS constructor is obsolete"
	    | assignArgs ((Args locs') :: locs, ARG exp :: args, stms) = let
	      (* MLRISC expression for address inside the source struct *)
		fun addr 0 = T.LOAD(wordTy, exp, memRg)
		  | addr offset = T.LOAD(wordTy, T.ADD(wordTy, exp, T.LI offset), memRg)
		fun copy ([], _, stms) = assignArgs(locs, args, stms)
		  | copy (Reg(ty, r, _) :: locs, offset, stms) =
		      copy (locs, offset+4, T.MV(ty, r, addr offset)::stms)
		  | copy (Stk(ty, off) :: locs, offset, stms) = let
		      val r = C.newReg()
		      in
			copy (locs, offset+4,
			  T.STORE(ty, paramAddr off, T.REG(wordTy, r), stkRg)
			  :: T.MV(ty, r, addr offset) :: stms)
		      end
		  | copy _ = raise Fail "unexpected FReg/FStk/Args in location list"
		in
		(* copy data from memory specified by exp to locs' *)
		  copy (locs', 0, stms)
		end
	    | assignArgs _ = error "argument/formal mismatch"
	  val argSetupCode = List.rev(assignArgs(argLocs, args, []))
	(* convert the result location to an MLRISC expression list *)
	  val result = (case resLoc
		 of NONE => []
		  | SOME(Reg(ty, r, _)) => [T.GPR(T.REG(ty, r))]
		  | SOME(FReg(ty, r, _)) => [T.FPR(T.FREG(ty, r))]
		  | SOME(Args[Reg(ty1,r1,_),Reg(ty2,r2,_)]) =>
		      [T.GPR(T.REG(ty1,r1)), T.GPR(T.REG(ty2,r2))]
		  | SOME _ => raise Fail "bogus result location"
		(* end case *))
	(* make struct return-area setup (if necessary) *)
	  val setupStructRet = (case structRetLoc
		 of NONE => []
		  | SOME loc => let
		      val structAddr = structRet loc
		      in
			[T.MV(wordTy, resGPR, structAddr)]
		      end
		(* end case *))
	(* determine the registers used and defined by this call *)
	  val (uses, defs) = let
		val locs = (case resLoc
		       of NONE => argLocs
			| SOME loc => loc::argLocs
		      (* end case *))
	      (* get the list of registers used to pass arguments and results *)
		fun addArgReg (Reg(ty, r, _)::locs, argRegs) =
		      addArgReg (locs, T.GPR(T.REG(ty, r))::argRegs)
		  | addArgReg (FReg(ty, r, _)::locs, argRegs) =
		      addArgReg (locs, T.FPR(T.FREG(ty, r))::argRegs)
		  | addArgReg ((Args locs')::locs, argRegs) =
		      addArgReg (locs, addArgReg(locs', argRegs))
		  | addArgReg (_::locs, argRegs) = addArgReg(locs, argRegs)
		  | addArgReg ([], argRegs) = argRegs
		val argRegs = addArgReg (locs, [])
		in
		  (argRegs, linkReg :: callerSaveRegs)
		end
	(* the actual call instruction *)
	  val callStm = T.CALL {
		  funct = name, targets = [],
		  defs = defs, uses = uses,
		  region = memRg, pops = 0
		}
	(* annotate, if necessary *)
	  val callStm = (case callComment
		 of NONE => callStm
		  | SOME c => T.ANNOTATION(callStm, #create MLRiscAnnotations.COMMENT c)
		(* end case *))
	(* take care of dedicated client registers *)
	  val {save, restore} = saveRestoreDedicated defs
	  val callseq = List.concat [
		  setupStructRet,
		  argSetupCode,
		  save,
		  [callStm],
		  restore
		]
	  in
	  (* check calling convention *)
	    case conv
	     of ("" | "ccall") => ()
	      | _ => error (concat [
		    "unknown calling convention \"",
		    String.toString conv, "\""
		  ])
	    (* end case *);
	    {callseq = callseq, result = result}
	  end

  end
