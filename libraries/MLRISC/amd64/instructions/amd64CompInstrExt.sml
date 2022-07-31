(* amd64CompInstrExt.sml
 *
 * COPYRIGHT (c) 2007 The Fellowship of SML/NJ (http://smlnj.org)
 *
 * emit code for extensions to the amd64 instruction set.
 *)

signature AMD64COMP_INSTR_EXT = sig
  structure I : AMD64INSTR
  structure TS : MLTREE_STREAM
		 where T = I.T
  structure CFG : CONTROL_FLOW_GRAPH 
 	         where I = I
                   and P = TS.S.P

  type reducer = 
    (I.instruction, I.C.cellset, I.operand, I.addressing_mode, CFG.cfg) TS.reducer

  val compileSext : 
     reducer 
      -> {stm: (I.T.stm, I.T.rexp, I.T.fexp, I.T.ccexp) AMD64InstrExt.sext, 
	  an: I.T.an list} 
        -> unit
end


functor AMD64CompInstrExt
  ( structure I : AMD64INSTR
    structure TS  : MLTREE_STREAM
		   where T = I.T
    structure CFG : CONTROL_FLOW_GRAPH 
		   where P = TS.S.P
		     and I = I
   ) : AMD64COMP_INSTR_EXT = 
struct
  structure CFG = CFG
  structure T = TS.T
  structure I = I
  structure C = I.C
  structure X = AMD64InstrExt
  structure TS = TS

  type stm = (T.stm, T.rexp, T.fexp, T.ccexp) X.sext

  type reducer = 
    (I.instruction, I.C.cellset, I.operand, I.addressing_mode, CFG.cfg) TS.reducer

  val rsp = C.rsp
  val rspOpnd = I.Direct(64,rsp)

  fun error msg = MLRiscErrorMsg.error("AMD64CompInstrExt", msg)

  val stackArea = I.Region.stack

  fun compileSext reducer {stm: stm, an:T.an list} = let
    val TS.REDUCER{operand, emit, reduceFexp, instrStream, reduceOperand,
                  ...} = reducer
    val TS.S.STREAM{emit=emitI, ...} = instrStream
    fun fstp(sz, fstpInstr, fexp) = 
      (case fexp
        of T.FREG(sz', f) =>
	    if sz <> sz' then error "fstp: sz"
	    else emitI(I.INSTR(fstpInstr(I.FDirect f)))
         | _ => error "fstp: fexp"
      (*esac*))
  in
    case stm
     of X.PUSHQ(rexp) => emit(I.push(operand rexp), an)
      | X.POP(rexp)   => emit(I.pop(operand rexp), an)
      | X.LEAVE	     => emit(I.leave, an)
      | X.RET(rexp)   => emit(I.ret(SOME(operand rexp)), an)
      | X.LOCK_XADDL (src, dst) => 
	   emit (I.xadd{
                 (* src must be in a register *)
                 lock=true,sz=I.I32,
                 src=I.Direct(32,reduceOperand(operand src)),
                 dst=operand dst},
	       an)
      | X.LOCK_XADDQ (src, dst) => 
	    emit (I.xadd{
		  (* src must be in a register *)
                  lock=true,sz=I.I64,
                  src=I.Direct(64,reduceOperand(operand src)),
                  dst=operand dst},
		  an)
      | X.LOCK_CMPXCHGL(src, dst) =>
	(* src must be in a register *)
	  emit(I.cmpxchg{
	      lock=true,sz=I.I32, 
	      src=I.Direct(32,reduceOperand(operand src)), 
	      dst=operand dst
	    }, an)
      | X.LOCK_CMPXCHGQ(src, dst) =>
	(* src must be in a register *)
	  emit(I.cmpxchg{
	      lock=true, sz=I.I64, 
	      src=I.Direct(64,reduceOperand(operand src)), 
	      dst=operand dst
	    }, an)
      | X.LOCK_XCHGL(src, dst) =>
	  emit(I.xchg{
	      lock=true,sz=I.I32, 
	      src=operand src,
	      dst=operand dst
	    }, an)
      | X.LOCK_XCHGQ(src, dst) =>
	  emit(I.xchg{
	      lock=true, sz=I.I64, 
	      src=operand src,
	      dst=operand dst
	    }, an)
      | X.PAUSE => emit(I.pause, an)
      | X.MFENCE => emit(I.mfence, an)
      | X.LFENCE => emit(I.lfence, an)
      | X.SFENCE => emit(I.sfence, an)
      | X.RDTSC => emit(I.rdtsc, an)
      | X.RDTSCP => emit(I.rdtscp, an)
    (* end case *)
  end
end
