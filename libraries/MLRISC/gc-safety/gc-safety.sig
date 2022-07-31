(*
 * This module is responsible for performing GC-safety repair in SSA form.
 *)
signature GC_SAFETY =
sig

   structure SSA : SSA

   val gcSafety : SSA.ssa -> SSA.ssa

end
