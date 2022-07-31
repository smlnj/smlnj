(*
 * This module performs global value numbering.
 * GVN also eliminates unnecessary branches.
 * 
 * -- Allen (leunga@cs.nyu.edu)
 *)
signature SSA_GLOBAL_VALUE_NUMBERING =
sig

   structure SSA : SSA
   structure CF  : SSA_CONSTANT_FOLDING
     sharing CF.SSA = SSA

   val computeValueNumbers : SSA.ssa -> int Array.array

end
