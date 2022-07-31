(*
 * Module for inserting preheaders and splitting critical edges.
 *
 * -- Allen
 *)

signature CFG_STRUCTURING =
sig

   structure IR : MLRISC_IR
 
   val reshape : IR.IR ->
                 { add_preheader        : bool,
                   split_critical_edges : bool
                 } -> unit

end

