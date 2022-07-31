signature GC_MAP =
sig
   structure GC : GC_TYPE
   structure C  : CELLS_BASIS = CellsBasis 

   exception GCMap

   type gcmap = GC.gctype C.HashTable.hash_table

   val GCMAP       : gcmap Annotations.property

   val GCLIVEIN    : (C.cell * GC.gctype) list Annotations.property 

   val GCLIVEOUT   : (C.cell * GC.gctype) list Annotations.property 

   val toString    : gcmap -> (C.cell -> string)

end
