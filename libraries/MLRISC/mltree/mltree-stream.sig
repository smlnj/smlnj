(* mltree-stream.sig 
 *
 * COPYRIGHT (c) 2001 Lucent Technologies, Bell Laboratories.
 *
 *)

signature MLTREE_STREAM = sig
  structure T : MLTREE
  structure S : INSTRUCTION_STREAM

  (*
   * Instruction streams
   *)
  type ('i,'cellset, 'cfg) stream = ('i, T.an list, 'cellset, 'cfg) S.stream 

  (* 
   * Extension mechanism
   *)
  datatype ('instr, 'cellset, 'operand, 'addressing_mode, 'cfg) reducer =
    REDUCER of
      {reduceRexp    : T.rexp -> T.reg,
       reduceFexp    : T.fexp -> T.reg,
       reduceCCexp   : T.ccexp -> T.reg,
       reduceStm     : T.stm * T.an list -> unit,
       operand       : T.rexp -> 'operand,
       reduceOperand : 'operand -> T.reg,
       addressOf     : T.rexp -> 'addressing_mode,
       emit          : 'instr * T.an list -> unit,
       instrStream   : ('instr,'cellset, 'cfg) stream,
       mltreeStream  : (T.stm,T.mlrisc list, 'cfg) stream
      }
end




