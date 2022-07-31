(* sparccomp-instr-ext.sml
 *
 * COPYRIGHT (c) 2001 Bell Labs, Lucent Technologies
 *
 * compiling a trivial extensions to the Sparc instruction set
 * (UNIMP instruction)
 *)
signature SPARCCOMP_INSTR_EXT = sig
    structure T : MLTREE
    structure I : SPARCINSTR
    		where T = T
    structure TS : MLTREE_STREAM
		where T = I.T
    structure CFG : CONTROL_FLOW_GRAPH 
    		where I = I 


    type reducer =
	 (I.instruction, I.C.cellset, I.operand, I.addressing_mode, CFG.cfg) TS.reducer

    val compileSext :
	reducer
	-> { stm: (T.stm, T.rexp, T.fexp, T.ccexp) SparcInstrExt.sext,
	     an: T.an list }
	-> unit
end

functor SparcCompInstrExt 
  (structure I   : SPARCINSTR
   structure TS  : MLTREE_STREAM
		where T = I.T
   structure CFG : CONTROL_FLOW_GRAPH 
   		where I = I
                  and P = TS.S.P
  ) : SPARCCOMP_INSTR_EXT = 
struct
    structure CFG = CFG
    structure T = TS.T
    structure TS = TS
    structure I = I
    structure C = I.C
    structure X = SparcInstrExt

    type stm = (T.stm, T.rexp, T.fexp, T.ccexp) X.sext

    type reducer =
	 (I.instruction, I.C.cellset, I.operand, I.addressing_mode, CFG.cfg) TS.reducer

    fun compileSext reducer { stm: stm, an: T.an list } = let
	val TS.REDUCER { emit, operand, reduceOperand, ... } = reducer
    in
	case stm 
	 of X.UNIMP i => emit (I.unimp {const22 = i}, an)
	  | X.SAVE (r, i, d) => emit(I.save{r=reduceOperand(operand r), i=operand i, d=reduceOperand(operand d)}, an)
	  | X.RESTORE (r, i, d) => emit(I.restore{r=reduceOperand(operand r), i=operand i, d=reduceOperand(operand d)}, an)
    end
end
