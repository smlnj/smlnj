(* x86comp-instr-ext.sml
 *
 * COPYRIGHT (c) 2000 Bell Labs, Lucent Technologies
 *
 * emit code for extensions to the x86 instruction set.
 *)
signature X86COMP_INSTR_EXT = sig
  structure I : X86INSTR
  structure TS : MLTREE_STREAM
		 where T = I.T
  structure CFG : CONTROL_FLOW_GRAPH 
 	         where I = I
                   and P = TS.S.P

  type reducer = 
    (I.instruction, I.C.cellset, I.operand, I.addressing_mode, CFG.cfg) TS.reducer

  val compileSext : 
     reducer 
      -> {stm: (I.T.stm, I.T.rexp, I.T.fexp, I.T.ccexp) X86InstrExt.sext, 
	  an: I.T.an list} 
        -> unit
end




functor X86CompInstrExt
  ( structure I : X86INSTR
    structure TS  : MLTREE_STREAM
		   where T = I.T
    structure CFG : CONTROL_FLOW_GRAPH 
		   where P = TS.S.P
		     and I = I
   ) : X86COMP_INSTR_EXT = 
struct
  structure CFG = CFG
  structure T = TS.T
  structure I = I
  structure C = I.C
  structure X = X86InstrExt
  structure TS = TS

  type stm = (T.stm, T.rexp, T.fexp, T.ccexp) X.sext

  type reducer = 
    (I.instruction, I.C.cellset, I.operand, I.addressing_mode, CFG.cfg) TS.reducer

  val esp = C.esp
  val espOpnd = I.Direct(esp)

  fun error msg = MLRiscErrorMsg.error("X86CompInstrExt", msg)

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
    of X.PUSHL(rexp) => emit(I.pushl(operand rexp), an)
     | X.POP(rexp)   => emit(I.pop(operand rexp), an)

     | X.FSTPS(fexp) => fstp(32, I.FSTPS, fexp)
     | X.FSTPL(fexp) => fstp(64, I.FSTPL, fexp)
     | X.FSTPT(fexp) => fstp(80, I.FSTPT, fexp)

     | X.LEAVE	     => emit(I.leave, an)
     | X.RET(rexp)   => emit(I.ret(SOME(operand rexp)), an)
     | X.LOCK_CMPXCHGL(src, dst) =>
       (* src must in a register *)
       emit(I.cmpxchg{lock=true,sz=I.I32, 
                      src=I.Direct(reduceOperand(operand src)), 
                      dst=operand dst},an)
     | X.PAUSE => emit(I.pause, an)
     | X.MFENCE => emit(I.mfence, an)
     | X.LFENCE => emit(I.lfence, an)
     | X.SFENCE => emit(I.sfence, an)

  end
end
