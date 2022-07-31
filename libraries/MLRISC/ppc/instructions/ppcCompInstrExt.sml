(* ppcCompInstrExt.sml
 *
 * COPYRIGHT (c) 2004 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * emit code for extensions to the ppc instruction set.
 *)

signature PPCCOMP_INSTR_EXT =
  sig
    structure I : PPCINSTR
    structure TS : MLTREE_STREAM
      where T = I.T
    structure CFG : CONTROL_FLOW_GRAPH 
      where I = I
	and P = TS.S.P

    type reducer = 
      (I.instruction, I.C.cellset, I.operand, I.addressing_mode, CFG.cfg) TS.reducer

    val compileSext : 
       reducer 
	-> {stm: (I.T.stm, I.T.rexp, I.T.fexp, I.T.ccexp) PPCInstrExt.sext, 
	    an: I.T.an list} 
          -> unit
  end

functor PPCCompInstrExt (

    structure I : PPCINSTR
    structure TS  : MLTREE_STREAM
      where T = I.T
    structure CFG : CONTROL_FLOW_GRAPH 
      where P = TS.S.P
	and I = I

  ) : PPCCOMP_INSTR_EXT =  struct

    structure CFG = CFG
    structure T = TS.T
    structure I = I
    structure C = I.C
    structure X = PPCInstrExt
    structure TS = TS

    type stm = (T.stm, T.rexp, T.fexp, T.ccexp) X.sext

    type reducer = 
      (I.instruction, I.C.cellset, I.operand, I.addressing_mode, CFG.cfg) TS.reducer

    fun error msg = MLRiscErrorMsg.error("PPCCompInstrExt", msg)

    fun compileSext (reducer : reducer) {stm : stm, an : T.an list} = let
	  val TS.REDUCER{
		  reduceRexp, operand, emit, instrStream, addressOf, ...
		} = reducer
	  val TS.S.STREAM{emit=emitI, ...} = instrStream
	  fun emit' inst = emit(I.INSTR inst, an)
	  in
	    case stm
	     of X.STWU{src, ea} => let
		  val (base, disp) = addressOf ea
		  in
		    emit' (I.ST{
			st = I.STWU,
			rs = reduceRexp src,
			ra = base,
			d = disp,
			mem = T.Region.memory
		      })
		  end
	    (* end case *)
	  end

  end
