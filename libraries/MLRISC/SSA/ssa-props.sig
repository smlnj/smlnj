(*
 * Instruction properties for utilizing the SSA form
 *
 * -- Allen (leunga@cs.nyu.edu)
 *)
signature SSA_PROPERTIES =
sig

   structure I        : INSTRUCTIONS
   structure C        : CELLS
   structure RTL      : MLTREE_RTL
   structure RTLProps : RTL_PROPERTIES
   structure OT       : OPERAND_TABLE
      sharing RTLProps.I   = OT.I = I
      sharing I.C          = C
      sharing RTLProps.RTL = RTL

   (* Create special nodes *)
   val source : I.instruction
   val sink   : I.instruction
   val phi    : I.instruction

   (* Physical registers whose value are volatile *)
   val volatile : C.cell list

   (* Instructions with definitions (or use) of these registers will not
    * be moved
    *)
   val pinnedDef : C.cell list
   val pinnedUse : C.cell list

   (* Definitions (or use) of these registers will not be renamed *)
   val fixedDef : C.cell list
   val fixedUse : C.cell list

   (* Extract the naming constraints on the operands *)
   val namingConstraints :   
        { instr : I.instruction, dst : C.cell list, src : C.cell list } -> 
        { dst    : (RTL.T.var * C.cell) list,  (* destination is fixed *)
          src    : (RTL.T.var * C.cell) list,  (* source is fixed *)
          dstsrc : (RTL.T.var * RTL.T.var) list    (* dst = src *)
        }

   (* Rewrite the operands of an instruction *)
   val rewriteOperands :   
        { const : int -> OT.const } ->
        { instr : I.instruction, dst : C.cell list, src : C.cell list } -> 
        I.instruction

   (* Make copies *)
   val copies : {kind: C.cellkind, dst:C.cell, src:C.cell} list -> 
                 I.instruction list
end

