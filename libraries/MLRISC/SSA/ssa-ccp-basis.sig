(*
 * This module performs a conditional constant propagation analysis
 * No update to the SSA graph is performed.
 *
 * -- Allen (leunga@cs.nyu.edu)
 *)
signature SSA_CONDITIONAL_CONSTANT_PROPAGATION =
sig
   structure SSA : SSA
   structure CF  : SSA_CONSTANT_FOLDING
      sharing CF.SSA = SSA

   type valueMap = CF.valnum Array.array (* SSA.value -> CF.valnum *)

   val condConstProp : SSA.ssa -> valueMap 
end
