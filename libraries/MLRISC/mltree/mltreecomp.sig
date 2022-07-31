(* mltreeComp.sig --- translate mltrees to a flowgraph of target machine code.
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *)

(*
 * This signature describes how MLTree extensions are compiled.
 *)
signature MLTREE_EXTENSION_COMP =
sig
   structure T : MLTREE
   structure TS : MLTREE_STREAM where T = T
   structure I : INSTRUCTIONS
   structure CFG : CONTROL_FLOW_GRAPH
		where I = I
	          and P = TS.S.P
   (* 
    * The reducer is given to the client during the compilation of
    * the user extensions.
    *)
   type reducer = 
     (I.instruction,I.C.cellset,I.operand,I.addressing_mode,CFG.cfg) TS.reducer

   val compileSext : reducer -> {stm:T.sext, an:T.an list} -> unit
   val compileRext : reducer -> {e:T.ty * T.rext, rd:CellsBasis.cell, an:T.an list} -> unit
   val compileFext : reducer -> {e:T.ty * T.fext, fd:CellsBasis.cell, an:T.an list} -> unit
   val compileCCext : reducer -> {e:T.ty * T.ccext, ccd:CellsBasis.cell, an:T.an list} -> unit
end




signature MLTREECOMP = 
sig
   structure TS : MLTREE_STREAM
   structure I : INSTRUCTIONS 
   structure CFG : CONTROL_FLOW_GRAPH 
      		where I = I
		  and P = TS.S.P
   structure Gen : MLTREEGEN 
   		where T = TS.T

   type instrStream = (I.instruction, I.C.cellset, CFG.cfg) TS.stream  
   type mltreeStream = (TS.T.stm, TS.T.mlrisc list, CFG.cfg) TS.stream 

    (* 
     * The instruction selection phase converts an instruction stream
     * into a mltree stream.  Please see the file "instructions/stream.sig"
     * for description of the stream interface.
     *
     * Note: the mltree stream does NOT support direct instruction emission.
     * Fo equivalent functionality, you can use the emit method 
     * of the instruction stream instead.
     *)
   val selectInstructions : instrStream -> mltreeStream
end




