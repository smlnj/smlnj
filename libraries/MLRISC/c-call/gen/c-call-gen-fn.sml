(* c-call-gen-fn.sml
 *
 * Generate MLRISC code for moving arguments to and from machine locations.
 *)

functor CCallGenFn (
    structure T : MLTREE
    structure C : CELLS
  (* given an offset constant, return an expression that gives an offset from
   * the run-time stack pointer 
   *)
    val offSp : T.I.machine_int -> T.rexp
  (* we assume that the address width is the same *)
    val wordTy : int
  (* extract least significant bits *)
    val lobits : {nBits : int, width : int, e : T.rexp} -> T.rexp
  (* sign extend the expression *)
    val sx : {fromWidth : int, toWidth : int, e : T.rexp} -> T.rexp
  (* float to float conversion (change width) *)
    val f2f : {fromWidth : int, toWidth : int, e : T.fexp} -> T.fexp
		 
    structure SA : STAGED_ALLOCATION
          where type reg_id = T.reg
          where type loc_kind = CLocKind.loc_kind

  ) : C_CALL_GEN = struct

    structure K = CLocKind
    structure T = T
    structure C = C
    structure SA = SA

    fun concatMap f ls = List.concat(List.map f ls)

    datatype c_arg 
      = ARG of T.rexp	
	  (* rexp specifies integer or pointer; if the 
           * corresponding parameter is a C struct, then 
	   * this argument is the address of the struct. 
	   *)
      | FARG of T.fexp
	  (* fexp specifies floating-point argument *)

    fun copyToReg (mty, r, e) = let
	val tmp = C.newReg ()
        in
	    [T.COPY (mty, [r], [tmp]), T.MV (mty, tmp, e)]
        end

    fun copyToFReg (mty, r, e) = let
	val tmp = C.newFreg ()
        in
	    [T.FCOPY (mty, [r], [tmp]), T.FMV (mty, tmp, e)] 
        end

    val stack = T.Region.stack

    fun litInt i = T.I.fromInt(wordTy, i)
    val lit = T.LI o litInt
    val offSp = offSp o litInt

  (* returns any general-purpose register IDs used in a machine location *)
    fun gprsOfLoc (SA.REG (_, K.GPR, r)) = [r]
      | gprsOfLoc (SA.COMBINE (l1, l2)) = gprsOfLoc l1 @ gprsOfLoc l2
      | gprsOfLoc (SA.NARROW (l, _, K.GPR)) = gprsOfLoc l
      | gprsOfLoc _ = []

  (* returns any floating-point register IDs used in a machine location *)
    fun fprsOfLoc (SA.REG (w, K.FPR, r)) = [(w, r)]
      | fprsOfLoc (SA.COMBINE (l1, l2)) = fprsOfLoc l1 @ fprsOfLoc l2
      | fprsOfLoc (SA.NARROW (l, _, K.FPR)) = fprsOfLoc l
      | fprsOfLoc _ = []

  (* eliminate redundant narrows, i.e., narrow.32(r1.32) == r1.32 *)
    fun elimNarrow (loc as SA.NARROW(SA.REG(wr, kr, r), wn, kn)) =
	  if kr = kn andalso wr = wn
	     then SA.REG(wr, kr, r)
	  else loc
      | elimNarrow (loc as SA.NARROW(SA.BLOCK_OFFSET(wb, kb, offset), wn, kn)) =
	  if kb = kn andalso wb = wn
	     then SA.BLOCK_OFFSET(wb, kb, offset)
	  else loc
      | elimNarrow (SA.COMBINE(l1, l2)) = SA.COMBINE(elimNarrow l1, elimNarrow l2)
      | elimNarrow loc = loc

    (* write a C argument (non aggregate) to a machine location
     *   - arg is the argument data
     *   - off is an offset into the argument data
     *   - loc is the machine location
     *   - stms is the accumulator of machine instructions
     *)
    fun writeLoc arg (off, loc, stms) = (
	  case (arg, loc)
	   of (ARG (e as T.REG _), SA.BLOCK_OFFSET(w, (K.GPR | K.STK), offset)) =>
	      (* register to stack (gpr) *)
	      T.STORE(wordTy, offSp offset, e, stack) :: stms
	    | (ARG (e as T.REG _), SA.NARROW(SA.BLOCK_OFFSET(w, (K.GPR | K.STK), offset), w', (K.GPR | K.STK))) =>
	      (* register to stack with width conversion (gpr) *)
	      T.STORE(w, offSp offset, sx{fromWidth=w', toWidth=w, e=e}, stack) :: stms
	    | (ARG (T.LOAD (ty, e, rgn)), SA.REG (w, K.GPR, r)) =>
	      (* memory to register (gpr) *)
	      copyToReg(w, r, T.LOAD (ty, T.ADD(wordTy, e, off), rgn)) @ stms
	    | (ARG (T.LOAD (ty, e, rgn)), SA.NARROW(SA.REG (w, K.GPR, r), w', K.GPR)) =>
	      (* memory to register with conversion (gpr) *)
	      copyToReg(w, r, sx{fromWidth=w', toWidth=w, e=T.LOAD (w', T.ADD(wordTy, e, off), rgn)}) @ stms
	    | (ARG e, SA.REG (w, K.GPR, r)) =>
	      (* expression to register *)
	      copyToReg(w, r, e) @ stms
	    | (ARG e, SA.NARROW (SA.REG(w, K.GPR, r), w', K.GPR)) => 
	      (* expression to register with conversion *)
	      copyToReg(w, r, sx{fromWidth=w', toWidth=w, e=e}) @ stms
	    | (ARG (T.LOAD (ty, e, rgn)), SA.BLOCK_OFFSET(w, (K.GPR | K.STK), offset)) => let
	      (* memory to stack (gpr) *)
		val tmp = C.newReg ()
	        in
		  T.STORE (ty, offSp offset, T.REG (ty, tmp), stack) :: 
		  T.MV (ty, tmp, T.LOAD (ty, T.ADD(wordTy, e, off), rgn)) :: stms
	        end
	    | (ARG (T.LOAD (ty, e, rgn)), SA.NARROW(SA.BLOCK_OFFSET(w, (K.GPR | K.STK), offset), w', K.GPR)) => let
	      (* memory to stack with conversion (gpr) *)
		val tmp = C.newReg ()
	        in
		  T.STORE (w, offSp offset, T.REG (w, tmp), stack) :: 
		  T.MV (w, tmp, sx{fromWidth=w', toWidth=w, e=T.LOAD (w', T.ADD(wordTy, e, off), rgn)}) :: stms
	        end
	    | (ARG e, SA.BLOCK_OFFSET(w, (K.GPR | K.STK), offset)) => let
	      (* expression to stack (gpr) *)
		val tmp = C.newReg ()
	        in
		  T.STORE (w, offSp offset, T.REG (w, tmp), stack) :: T.MV (w, tmp, e) :: stms
	        end
	    | (ARG e, SA.NARROW(SA.BLOCK_OFFSET(w, (K.GPR | K.STK), offset), w', K.GPR)) => let
	      (* expression to stack with conversion (gpr) *)
		val tmp = C.newReg ()
	        in
		  T.STORE (w, offSp offset, T.REG (w, tmp), stack) :: T.MV (w, tmp, sx{fromWidth=w', toWidth=w, e=e}) :: stms
	        end
	    | (FARG (e as T.FREG _), SA.BLOCK_OFFSET(w, (K.FPR | K.FSTK), offset)) =>
	      (* register to stack (fpr) *)
	      T.FSTORE (w, offSp offset, e, stack) :: stms
	    | (FARG e, SA.REG(w, K.FPR, r)) => 
	      (* expression to register (fpr) *)
	      copyToFReg(w, r, e) @ stms
	    | (FARG e, SA.NARROW(SA.REG(w, K.FPR, r), w', K.FPR)) => 
	      (* expression to register with conversion (fpr) *)
	      copyToFReg(w', r, f2f{fromWidth=w, toWidth=w', e=e}) @ stms
	    | (ARG (T.LOAD (ty, e, rgn)), SA.REG(w, K.FPR, r)) =>
	      (* memory to register (fpr) *)
	      copyToFReg(w, r, T.FLOAD (ty, T.ADD(wordTy, e, off), rgn)) @ stms
	    | (ARG (T.LOAD (ty, e, rgn)), SA.BLOCK_OFFSET(w, (K.FPR | K.FSTK), offset)) => let
              (* memory to stack (fpr) *)
		val tmp = C.newFreg ()
	        in
		  T.FSTORE (w, offSp offset, T.FREG (w, tmp), stack) :: 
		  T.FMV (w, tmp, T.FLOAD (ty, T.ADD(wordTy, e, off), rgn)) :: stms
	        end
	    | (ARG (T.LOAD (ty, e, rgn)), SA.NARROW(SA.BLOCK_OFFSET(w, (K.FPR | K.FSTK), offset), w', K.FPR)) => let
              (* memory to stack with conversion (fpr) *)
		val tmp = C.newFreg ()
	        in
		  T.FSTORE (w, offSp offset, T.FREG (w, tmp), stack) :: 
		  T.FMV (w', tmp, f2f{fromWidth=w, toWidth=w', e=T.FLOAD (w', T.ADD(wordTy, e, off), rgn)}) :: stms
	        end
	    | (FARG (T.FLOAD (ty, e, rgn)), SA.BLOCK_OFFSET(w, (K.FPR | K.FSTK), offset)) => let
              (* memory to stack (fpr) *)
		val tmp = C.newFreg ()
	        in
		  T.FSTORE (w, offSp offset, T.FREG (w, tmp), stack) :: 
		  T.FMV (w, tmp, T.FLOAD (w, T.ADD(wordTy, e, off), rgn)) :: stms
	        end
	    | (FARG (T.FLOAD (ty, e, rgn)), SA.NARROW(SA.BLOCK_OFFSET(w, (K.FPR | K.FSTK), offset), w', K.FPR)) => let
              (* memory to stack with conversion (fpr) *)
		val tmp = C.newFreg ()
	        in
		  T.FSTORE (w, offSp offset, T.FREG (w, tmp), stack) :: 
		  T.FMV (w', tmp, f2f{fromWidth=w, toWidth=w', e=T.FLOAD (w, T.ADD(wordTy, e, off), rgn)}) :: stms
	        end
	    | (FARG e, SA.BLOCK_OFFSET(w, (K.FPR | K.FSTK), offset)) => let
              (* expression to stack (fpr) *)
		val tmp = C.newFreg ()
	        in
		  T.FSTORE (w, offSp offset, T.FREG (w, tmp), stack) :: T.FMV (w, tmp, e) :: stms
	        end
	    | (FARG e, SA.NARROW(SA.BLOCK_OFFSET(w, (K.FPR | K.FSTK), offset), w', K.FPR)) => let
              (* expression to stack (fpr) *)
		val tmp = C.newFreg ()
	        in
		  T.FSTORE (w', offSp offset, f2f{fromWidth=w, toWidth=w', e=T.FREG (w, tmp)}, stack) :: T.FMV (w, tmp, e) :: stms
	        end
	    | (FARG _, SA.COMBINE _) => 
	      raise Fail ""
	    | _ => raise Fail "invalid arg / loc pair"
          (* end case *))

  (* write a C argument (possibly an aggregate) to some parameter locations *)
    fun writeLocs' (arg, locs, stms) = let
	  val locs = List.map elimNarrow locs
        (* offsets of the members of the struct *)
	  val membOffs = List.tabulate(List.length locs, fn i => lit(i*8))
          in
	     ListPair.foldl (writeLoc arg) stms (membOffs, locs)
          end

  (* write C arguments to parameter locations; also return any used GPRs and FPRs *)
    fun writeLocs (args, argLocs) = let
	  val gprs = concatMap gprsOfLoc (List.concat argLocs)
	  val fprs = concatMap fprsOfLoc (List.concat argLocs)
	  val instrs = ListPair.foldl writeLocs' [] (args, argLocs)
          in
	     (List.rev instrs, gprs, fprs)
          end

  (* read from a machine location *)
    fun readLoc (loc, (resultRegs, copyResult)) = (
	  case elimNarrow loc
	   of SA.REG(w, K.GPR, r) => let
                (* register (gpr) *)
		val tmpR = C.newReg()
	        in
		  (T.GPR(T.REG(w, tmpR)) :: resultRegs, T.COPY(w, [tmpR], [r]) :: copyResult)
	        end
	    | SA.NARROW(loc, w', K.GPR) => let
                (* conversion (gpr) *)
		val ([resultReg as T.GPR(T.REG(_, tmp))], copyResult') = readLoc(loc, ([], []))
		val w = SA.width loc
	        in
		  (T.GPR(T.REG(w', tmp)) :: resultRegs, 
		   T.MV(w, tmp, lobits{nBits=w', width=w, e=T.REG (w, tmp)}) :: copyResult' @ copyResult)
	        end
	    | SA.REG(w, K.FPR, r) => let
		val resReg = C.newFreg()
	        in
		   (T.FPR(T.FREG(w, resReg)) :: resultRegs, T.FCOPY(w, [resReg], [r]) :: copyResult)
	        end
	    | SA.NARROW(loc, w', K.FPR) => let
                (* conversion (fpr) *)
		val ([resultReg as T.FPR(T.FREG(_, tmp))], copyResult') = readLoc(loc, ([], []))
		val w = SA.width loc
	        in
		   (T.FPR(T.FREG(w', tmp)) :: resultRegs, 
		    T.FMV(w', tmp, f2f{fromWidth=w, toWidth=w', e=T.FREG(w', tmp)}) :: copyResult' @ copyResult)
	        end
	    | SA.COMBINE (l1, l2) => (
	        case (readLoc (l1, ([], [])), readLoc (l2, ([], [])))
		 of ( ([T.GPR e1], instrs1), ([T.GPR e2], instrs2) ) => let
			val w = SA.width loc
			val w' = SA.width l2
			val tmp = C.newReg()
		        in
			   (T.GPR(T.REG(w, tmp)) :: resultRegs, 
			    T.MV(w, tmp, T.ADD(w, T.SLL(w, lit w', e1), e2)) :: 
			    instrs1 @ instrs2 @ copyResult)
			end
 	        (* end case *))
	    | _ => raise Fail "bogus read location"
         (* end case *))

  (* read from some machine locations *)
    fun readLocs locs = let
	  val (resultRegs, copyResult) = List.foldl readLoc ([], []) locs
          in
	      (List.rev resultRegs, List.rev copyResult)
	  end

  end (* CCallGenFn *)
