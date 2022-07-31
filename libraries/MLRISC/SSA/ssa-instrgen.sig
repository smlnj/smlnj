(*
 * This module is responsible for generating new instructions from 
 * MLTREE and inserting them into the SSA graph.  This is useful for
 * patching in new instructions as the SSA graph is being transformed.
 *
 * Special MLRISC Magic(tm) for invoking the instruction selection 
 * module and ssa-ifying the output code are all hidden here.
 * 
 * -- Allen (leunga@cs.nyu.edu)
 * 
 *)
signature SSA_INSTRGEN =
sig

   structure SSA : SSA
   structure RTL : MLTREE_RTL
   structure MLTreeComp : MLTREECOMP
      sharing SSA.MLTreeComp = MLTreeComp
      sharing SSA.RTL = RTL

    exception Illegal

   (* Convert internal RTL into a mltree term *)
   val translate : SSA.ssa ->
                   {rtl:RTL.rtl, defs:SSA.value list, uses:SSA.value list} ->
                      MLTreeComp.T.stm
   (* Convert an mltree term into a list of instructions *)
   val instrGen  : SSA.ssa -> MLTreeComp.T.stm -> SSA.I.instruction list

   (* Insert instructions into the SSA graph *)

   (* Replace the instruction at id *)
   val replace : SSA.ssa -> {id:SSA.ssa_id, mltree:MLTreeComp.T.stm} -> bool

end
