(* amd64-svid.sml
 *
 * COPYRIGHT (c) 2000 Bell Labs, Lucent Technologies
 *
 * C function calls for the AMD64 using the System V ABI
 *
 * Register conventions:
 *
 *    %rdi	1st argument
 *    %rsi	2nd
 *    %rdx	3rd
 *    %rcx	4th
 *    %r8	5th
 *    %r9	6th
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
 * Questions:
 *    - what about stack frame alignment?
 *)

functor AMD64SVID_CCalls (
    structure T : MLTREE
  ) : C_CALLS = struct
           structure MLTreeUtils = MLTreeUtils
               (structure T = T
                fun hashSext  _ _ = 0w0 
                fun hashRext  _ _ = 0w0
                fun hashFext  _ _ = 0w0 
                fun hashCCext _ _ = 0w0
             
                (* Equality extensions *)
                fun eqSext  _ _ = false
                fun eqRext  _ _ = false
                fun eqFext  _ _ = false
                fun eqCCext _ _ = false
             
                (* Pretty printing extensions *)
                fun showSext  _ _ = ""
                fun showRext  _ _ = ""
                fun showFext  _ _ = ""
                fun showCCext _ _ = ""
               )

    val frameAlign = 16

    structure T  = T
    structure Ty = CTypes
    structure C = AMD64Cells

    fun error msg = MLRiscErrorMsg.error ("AMD64SVID_CCalls", msg)

    datatype  c_arg 
      = ARG of T.rexp	    
      | FARG of T.fexp
      | ARGS of c_arg list

    val mem = T.Region.memory
    val stack = T.Region.stack

  (* MLRISC types *)
    val wordTy = 64
    val fltTy = 32
    val dblTy = 64

  (* shorts and chars are promoted to 32-bits *)
    val naturalIntSz = wordTy

    val paramAreaOffset = 0 (* stack offset to param area *)

  (* This annotation is used to indicate that a call returns a fp value 
   * in %st(0) 
   *)
    val fpReturnValueInXmm0 = #create MLRiscAnnotations.RETURN_ARG C.xmm0

    val sp = C.rsp
    val spR = T.REG(wordTy, sp)

    fun fpr(sz,f) = T.FPR(T.FREG(sz, f))
    fun gpr(sz,r) = T.GPR(T.REG(sz, r))
    val rax = C.rax
    val xmm0 = C.xmm0

  (* the C calling convention requires that the FP stack be empty on function
   * entry.  We add the fpStk list to the defs when the fast_floating_point flag
   * is set.
   *)
    val fpStk =
	[fpr (dblTy, C.xmm0), fpr (dblTy, C.xmm1),
	 fpr (dblTy, C.xmm2), fpr (dblTy, C.xmm3),
	 fpr (dblTy, C.xmm4), fpr (dblTy, C.xmm5),
	 fpr (dblTy, C.xmm6), fpr (dblTy, C.xmm7),
	 fpr (dblTy, C.xmm8), fpr (dblTy, C.xmm9),
	 fpr (dblTy, C.xmm10), fpr (dblTy, C.xmm11),
	 fpr (dblTy, C.xmm12), fpr (dblTy, C.xmm13),
	 fpr (dblTy, C.xmm14), fpr (dblTy, C.xmm15)]

  (* note that the caller saves includes the result register (%eax) *)
    val callerSaves =
	[gpr(wordTy, rax), gpr(wordTy, C.rcx), gpr(wordTy, C.rdx),
	 gpr(wordTy, C.rsi), gpr(wordTy, C.rdi),
	 gpr(wordTy, C.r8), gpr(wordTy, C.r9),
	 gpr(wordTy, C.r10), gpr(wordTy, C.r11)]

  (* C callee-save registers *)
    val calleeSaveRegs =
	[C.rbx, C.rbp,
	 C.r12, C.r13, C.r14, C.r15]		(* C callee-save registers *)
    val calleeSaveFRegs = []			(* C callee-save floating-point registers *)

  (* align the address to the given alignment, which must be a power of 2 *)
    fun alignAddr (addr, align) = let
	  val mask = Word.fromInt(align-1)
	  in
	    Word.toIntX(Word.andb(Word.fromInt addr + mask, Word.notb mask))
	  end

    fun align4 addr = Word.toIntX(Word.andb(Word.fromInt addr + 0w3, Word.notb 0w3))
    fun align8 addr = Word.toIntX(Word.andb(Word.fromInt addr + 0w7, Word.notb 0w7))

  (* size and natural alignment for integer types. *)
  (* arguments are always aling 8 structs may align differently *)
    fun sizeOfInt Ty.I_char = {ty = 8, sz = 1, align = 8 (* 1 *) }
      | sizeOfInt Ty.I_short = {ty = 16, sz = 2, align = 8 (* 2 *) }
      | sizeOfInt Ty.I_int = {ty = 32, sz = 4, align = 8 (* 4 *) }
      | sizeOfInt Ty.I_long = {ty = 32, sz = 4, align = 8 (* 4 *) }
      | sizeOfInt Ty.I_long_long = {ty = 64, sz = 8, align = 8 (* 4 *) }

  (* sizes of other C types *)
    val sizeOfPtr = {ty = 64, sz = 8, align = 8}

  (* compute the size and alignment information for a struct; tys is the list
   * of member types.
   * The total size is padded to agree with the struct's alignment.
   *)
    fun sizeOfStruct tys = let
	  fun ssz ([], maxAlign, offset) =
		{sz = alignAddr(offset, maxAlign), align = maxAlign}
	    | ssz (ty::tys, maxAlign, offset) = let
		  val {sz, align} = sizeOfTy ty
		  val offset = alignAddr(offset, align)
		  in
		    ssz (tys, Int.max(maxAlign, align), offset+sz)
		  end
	  in
	    ssz (tys, 1, 0)
	  end

  (* the size alignment of a union type is the maximum of the sizes and alignments of the
   * members.  The final size is padded to agree with the alignment.
   *)
    and sizeOfUnion tys = let
	  fun usz ([], maxAlign, maxSz) =
		{sz = alignAddr(maxSz, maxAlign), align = maxAlign}
	    | usz (ty::tys, maxAlign, maxSz) = let
		  val {sz, align} = sizeOfTy ty
		  in
		    usz (tys, Int.max(maxAlign, align), Int.max(sz, maxSz))
		  end
	  in
	    usz (tys, 1, 0)
	  end

    and sizeOfTy Ty.C_void = error "unexpected void argument type"
      | sizeOfTy Ty.C_float = {sz = 4, align = 4}
      | sizeOfTy Ty.C_double = {sz = 8, align = 4}
      | sizeOfTy Ty.C_long_double = {sz = 12, align = 4}
      | sizeOfTy (Ty.C_unsigned isz) = let
	  val {sz, align, ...} = sizeOfInt isz
	  in
	    {sz = sz, align = align}
	  end
      | sizeOfTy (Ty.C_signed isz) = let
	  val {sz, align, ...} = sizeOfInt isz
	  in
	    {sz = sz, align = align}
	  end
      | sizeOfTy Ty.C_PTR = {sz = 8, align = 8}
      | sizeOfTy (Ty.C_ARRAY(ty, n)) = let
	  val {sz, align} = sizeOfTy ty
	  in
	    {sz = n*sz, align = align}
	  end
      | sizeOfTy (Ty.C_STRUCT tys) = sizeOfStruct tys
      | sizeOfTy (Ty.C_UNION tys) = sizeOfUnion tys

  (* the location of arguments/parameters; offsets are given with respect to the
   * low end of the parameter area (see paramAreaOffset above).
   *)
    datatype arg_location
      = Reg of T.ty * T.reg * T.I.machine_int option
					(* integer/pointer argument in register *)
      | FReg of T.fty * T.reg * T.I.machine_int option
					(* floating-point argument in register *)
      | Stk of T.ty * T.I.machine_int	(* integer/pointer argument in parameter area *)
      | FStk of T.fty * T.I.machine_int	(* floating-point argument in parameter area *)
      | Args of arg_location list

    fun intResult iTy = (case #ty(sizeOfInt iTy)
	   of 64 => raise Fail "register pair result"
	    | ty => (SOME(Reg(ty, rax, NONE)), NONE, 0)
	  (* end case *))

    fun layout {conv, retTy, paramTys} = let
	(* get the location of the result (resLoc) and the offset of the first
	 * parameter/argument.  If the result is a struct or union, then we also
	 * compute the size and alignment of the result type (structRetLoc).
	 *)
	  val (resLoc, structRetLoc, argOffset) = (case retTy
		 of Ty.C_void => (NONE, NONE, 0)
		  | Ty.C_float => (SOME(FReg(fltTy, xmm0, NONE)), NONE, 0)
		  | Ty.C_double => (SOME(FReg(dblTy, xmm0, NONE)), NONE, 0)
(*
		  | Ty.C_long_double => (SOME(FReg(xdblTy, st0, NONE)), NONE, 0)
*)
		  | Ty.C_unsigned iTy => intResult iTy
		  | Ty.C_signed iTy => intResult iTy
		  | Ty.C_PTR => (SOME(Reg(wordTy, rax, NONE)), NONE, 0)
		  | Ty.C_ARRAY _ => error "array return type"
(*
		  | Ty.C_STRUCT tys => let
		      val {sz, align} = sizeOfStruct tys
		      in
		        if (sz > 8) orelse (not returnSmallStructsInRegs)
		          then (SOME(Reg(wordTy, eax, NONE)), SOME{szb=sz, align=align}, 4)
		          else raise Fail "small struct return not implemented yet"
		      end
		  | Ty.C_UNION tys => let
		      val {sz, align} = sizeOfUnion tys
		      in
		        if (sz > 8) orelse (not returnSmallStructsInRegs)
		          then (SOME(Reg(wordTy, rax, NONE)), SOME{szb=sz, align=align}, 4)
		          else raise Fail "small union return not implemented yet"
		      end
*)
		(* end case *))
	  fun assign ([], frees, offset, locs) = (List.rev locs, align8 offset)
	    | assign (paramTy::params, frees, offset, locs) = let
		fun next {ty, align, sz} =
		    if null frees
		    then
		      let
		      val offset = alignAddr (offset, align)
		      in
			assign (params, [], offset+sz, Stk(ty, IntInf.fromInt offset)::locs)
		      end
		    else
			assign
			(params, tl frees, offset, Reg(ty, hd frees, NONE)::locs)
		fun nextFlt (ty, szb) = let
		      val offset = alignAddr (offset, 4)
		      in
			assign (params, [], offset+szb, FStk(ty, IntInf.fromInt offset)::locs)
		      end
		fun assignMem {sz, align} = let
		      fun f (nb, offset, locs') =
			    if (nb >= 4)
			      then f(nb-4, offset+4, Stk(wordTy, IntInf.fromInt offset)::locs')
			    else if (nb >= 2)
			      then f(nb-2, offset+2, Stk(16, IntInf.fromInt offset)::locs')
			    else if (nb = 1)
			      then f(nb, offset+1, Stk(8, IntInf.fromInt offset)::locs')
			      else assign(params, [], align4 offset, Args(List.rev locs')::locs)
		      in
			f (sz, offset, [])
		      end
		in
		  case paramTy
		   of Ty.C_void => error "void argument type"
		    | Ty.C_float => nextFlt (fltTy, 4)
		    | Ty.C_double => nextFlt (dblTy, 8)
(*
		    | Ty.C_long_double => nextFlt (xdblTy, 12)
*)
		    | Ty.C_unsigned iTy => next (sizeOfInt iTy)
		    | Ty.C_signed iTy => next (sizeOfInt iTy)
		    | Ty.C_PTR => next sizeOfPtr
		    | Ty.C_ARRAY _ => next sizeOfPtr
		    | Ty.C_STRUCT tys => assignMem(sizeOfStruct tys)
		    | Ty.C_UNION tys => assignMem(sizeOfUnion tys)
		  (* end case *)
		end
	  val (argLocs, argSz) =
	      assign
	      (paramTys,
	       [C.rdi, C.rsi, C.rdx, C.rcx, C.r8, C.r9],
	       argOffset, [])
	  val argMem = {szb = alignAddr (argSz, frameAlign), align = frameAlign}
	  in {
	    argLocs = argLocs, argMem = argMem,
	    resLoc = resLoc, structRetLoc = structRetLoc
	  } end

  (* List of registers defined by a C Call with the given return type; this list
   * is the result registers plus the caller-save registers.
   *)
    fun definedRegs resTy = let
	val defs = callerSaves @ fpStk
	in
	    case resTy
	     of (Ty.C_unsigned(Ty.I_long_long)) => gpr(wordTy, C.rdx) :: defs
	      | (Ty.C_signed(Ty.I_long_long)) => gpr(wordTy, C.rdx) :: defs
	      | _ => defs
	      (* end case *)
	end

(*
    fun fstp (32, f) = T.EXT(ix(IX.FSTPS(f)))
      | fstp (64, f) = T.EXT(ix(IX.FSTPL(f)))
      | fstp (80, f) = T.EXT(ix(IX.FSTPT(f)))
      | fstp (sz, f) = error ("fstp(" ^ Int.toString sz ^ ",_)")
*)

    fun genCall {
	    name, proto, paramAlloc, structRet, saveRestoreDedicated, callComment, args
	  } = let
	  val {argLocs, argMem, resLoc, structRetLoc} = layout proto
	  fun finduses (Reg(ty, r, _)::t) = gpr (ty, r)::finduses t
	    | finduses (_::t) = finduses t
	    | finduses [] = []
	  val uses = finduses argLocs
	(* instruction to save frame pointer in rbp *)
(*
	  val rbpFramePointer =
		[T.MV(wordTy, C.rbp, spR)]
*)
	(* instruction to allocate space for arguments *)
	  val argAlloc = if ((#szb argMem = 0) orelse paramAlloc argMem)
		then []
		else [T.MV(wordTy, sp, T.SUB(wordTy, spR, T.LI(IntInf.fromInt(#szb argMem))))]
	(* for functions that return a struct/union, pass the location as an
	 * implicit first argument.  Because the callee removes this implicit
	 * argument from the stack, we must also keep track of the size of the
	 * explicit arguments.
	 *)
	  val (args, argLocs, explicitArgSzB) = (case structRetLoc
		 of SOME pos =>
		      (ARG(structRet pos)::args, Stk(wordTy, 0)::argLocs, #szb argMem - 4)
		  | NONE => (args, argLocs, #szb argMem)
		(* end case *))
	(* generate instructions to copy arguments into argument area
	 * using %rsp to address the argument area.
         * stmsDelay can delay copying of the arguments.
	 *)
	  val (copyArgs, copyArgsDelay) = let
		fun offSP 0 = spR
		  | offSP offset = T.ADD(wordTy, spR, T.LI offset)
		fun f ([], [], stms, stmsDelay) = (List.rev stms, List.rev stmsDelay)
		  | f (arg::args, loc::locs, stms, stmsDelay) = let
			val stms = (case (arg, loc)
			       of (ARG(rexp as T.REG _), Stk(mty, offset)) =>
					(
				    T.STORE(mty, offSP offset, rexp, stack)
				      :: stms
					)
				| (ARG rexp, Stk(mty, offset)) => let
				    val tmp = C.newReg()
				    in
				      T.STORE(wordTy, offSP offset, T.REG(wordTy, tmp), stack)
					:: T.MV(wordTy, tmp, rexp)
					:: stms
				    end
				| (ARG rexp, Args memLocs) => let
				  (* addrR is used to address the source of the memory object
				   * being passed to the memLocs.  loadAddr is the code to
				   * initialize addrR.
				   *)
				    val (loadAddr, addrR) = (case rexp
					   of T.REG _ => ([], rexp)
					    | _ => let
						val r = C.newReg()
						in
						  ([T.MV(wordTy, r, rexp)], T.REG(wordTy, r))
						end
					  (* end case *))
				    fun addr 0 = addrR
				      | addr offset = T.ADD(wordTy, addrR, T.LI offset)
				  (* stack offset of first destination word *)
				    val baseOffset = (case memLocs
					   of Stk(ty, offset)::_ => offset
					    | _ => error "bogus Args"
					  (* end case *))
				    fun copy ([], stms) = stms
				      | copy (Stk(ty, offset)::locs, stms) = let
					  val tmp = C.newReg()
					  val stms =
						T.STORE(ty, offSP offset, T.REG(ty, tmp), stack)
						  :: T.MV(ty, tmp, T.LOAD(ty, addr(offset - baseOffset), mem))
						  :: stms
					  in
					    copy (locs, stms)
					  end
				      | copy _ = error "bogus memory location"
				    in
				      copy (memLocs, loadAddr @ stms)
				    end
				| (FARG(fexp as T.FREG _), FStk(ty, offset)) =>
					(
				    T.FSTORE(ty, offSP offset, fexp, stack) :: stms
					)
				| (FARG fexp, FStk(ty, offset)) => let
				    val tmp = C.newFreg()
				    in
				      T.FSTORE(ty, offSP offset, T.FREG(ty, tmp), stack)
					:: T.FMV(ty, tmp, fexp)
					:: stms
				    end
				| (ARGS _, _) => raise Fail "ARGS obsolete"
				| _ => stms
			      (* end case *))
			val stmsDelay = (case (arg, loc)
			       of (ARG rexp, Reg(mty, mreg, NONE)) => let
					val CellsBasis.CELL{id,...} = mreg
				    in
					T.MV(wordTy, mreg, rexp) :: stmsDelay
				    end
				| _ => stmsDelay
			      (* end case *))

			in
			  f (args, locs, stms, stmsDelay)
			end
		  | f _ = error "argument arity error"
		in
		  f (args, argLocs, [], [])
		end
	(* the SVID specifies that the caller pops arguments, but the callee
	 * pops the arguments in a stdcall on Windows.  I'm not sure what other
	 * differences there might be between the SVID and Windows ABIs. (JHR)
	 *)
	  val calleePops = (case #conv proto
		 of (""|"ccall") => false
		  | "stdcall" => true
		  | conv => error (concat [
			"unknown calling convention \"", String.toString conv, "\""
		      ])
		(* end case *))
	  val defs = definedRegs(#retTy proto)
	  val { save, restore } = saveRestoreDedicated defs
	  val callStm = T.CALL{
		  funct=name, targets=[], defs=defs, uses=uses, 
		  region = mem,
		  pops = if calleePops
		      then Int32.fromInt(#szb argMem)
		      else Int32.fromInt(#szb argMem - explicitArgSzB)
		}
	  val callStm = (case callComment
		 of NONE => callStm
		  | SOME c => T.ANNOTATION (callStm, #create MLRiscAnnotations.COMMENT c)
		(* end case *))
	(* If return type is floating point then add an annotation RETURN_ARG 
	 * This is currently a hack.  Eventually MLTREE *should* support
	 * return arguments for CALLs.
	 * --- Allen
	 *)
	  val callStm = if
		        ((#retTy proto = Ty.C_float)
		  orelse (#retTy proto = Ty.C_double)
		  orelse (#retTy proto = Ty.C_long_double))
		then T.ANNOTATION(callStm, fpReturnValueInXmm0)
		else callStm
	(* code to pop the arguments from the stack *)
	  val popArgs = if calleePops orelse (explicitArgSzB = 0)
		then []
		else [T.MV(wordTy, sp, T.ADD(wordTy, spR, T.LI(IntInf.fromInt explicitArgSzB)))]
	(* code to copy the result into fresh pseudo registers *)
	  val (resultRegs, copyResult) = (case resLoc
		 of NONE => ([], [])
		  | SOME(Reg(ty, r, _)) => let
		      val resReg = C.newReg()
		      in
			([T.GPR(T.REG(ty, resReg))], [T.COPY(ty, [resReg], [r])])
		      end
		  | SOME(FReg(ty, r, _)) => let
		      val resReg = C.newFreg()
		      val res = [T.FPR(T.FREG(ty, resReg))]
		      in
        	      (* If we are using fast floating point mode then do NOT 
        	       * generate FSTP.
        	       * --- Allen 
        	       *)
			  (res, [T.FCOPY(ty, [resReg], [r])])
		      end
		  | _ => error "bogus result location"
		(* end case *))
	(* assemble the call sequence *)
	  val callSeq = argAlloc @ (* rbpFramePointer @ *) copyArgs @ save @ copyArgsDelay @ [callStm] @ restore @ popArgs @ copyResult
	  in
	    {callseq=callSeq, result=resultRegs}
	  end

  end
