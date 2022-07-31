(*
 * This module provides few helper functions for annotating virtual registers
 * with gc type information. 
 *)

signature GC_CELLS =
sig

   structure C  : CELLS
   structure GC : GC_TYPE
   structure CB : CELLS_BASIS = CellsBasis

   (* Generate a virtual register and update the gc info at the same time. *)
   val newCell   : CB.cellkind -> GC.gctype -> CB.cell
   val setGCType : CB.cell * GC.gctype -> unit
   val getGCType : CB.cell -> GC.gctype

   (* Prettty print gc type *)
   val printType : CB.cell -> string

   val GCLIVEOUT : (CB.cell * GC.gctype) list Annotations.property

end
