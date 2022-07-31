(*
 * This makes a new cell function that automatically propagate gc type info.
 *)
functor GCCells(structure C : CELLS
                structure GC : GC_TYPE
               ) : GC_CELLS =
struct

   structure C  = C
   structure GC = GC
   structure CB = CellsBasis
  
   (*
    * Generate a new virtual register and update the gc information 
    * at the same time.
    *)
   fun newCell k = 
   let val new = C.newCell k
       val set = #set GC.GC_TYPE
       fun genVar gc =
       let val r as CB.CELL{an, ...} = new()
       in  an := set(gc,!an); r end
   in  genVar
   end

   fun getGCType(CB.CELL{an, ...}) = #lookup GC.GC_TYPE (!an)
   fun setGCType(CB.CELL{an, ...}, gc) = an := #set GC.GC_TYPE (gc, !an)

   fun printType(CB.CELL{an, ...}) = 
       case #get GC.GC_TYPE (!an) of
         SOME ty => ":"^GC.toString ty
       | NONE    => ":?"

   val GCLIVEOUT = Annotations.new(SOME(fn _ => "GCLIVEOUT")) 
                     : (CB.cell * GC.gctype) list Annotations.property
end
