(*
 * Signature of a MLRISC IR-based optimization phase
 *)
signature MLRISC_IR_OPTIMIZATION =
sig
   structure IR : MLRISC_IR
   include MLRISC_OPTIMIZATION
      where type flowgraph = IR.IR
end
