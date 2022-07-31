(*
 * This module implements the MLRISC annotations for describing
 * memory aliasing and control dependence.
 *
 * -- Allen
 *)

structure MLRiscRegion :> MLRISC_REGION =
struct

   datatype mutability = READONLY | IMMUTABLE | MUTABLE
   datatype region = ROOT
                   | REGION of int * mutability * string * region
                   | UNION of region list

   val counter = ref 0

   val memory = ROOT
   fun new(name,mut,parent) = 
   let val id = !counter
   in  counter := id + 1;
       REGION(!counter,mut,name,parent)
   end

   val union    = UNION
   val stack    = new("stack",MUTABLE,memory)
   val heap     = new("heap",MUTABLE,memory)
   val data     = new("data",MUTABLE,memory)
   val readonly = new("readonly",READONLY,data)

   fun toString ROOT = "root"
     | toString(REGION(_,_,name,ROOT)) = name
     | toString(REGION(_,_,name,parent)) = toString parent^"."^name
     | toString(UNION rs) = 
         String.concat(foldr (fn (r,[]) => [toString r]
                               | (r,s)  => toString r::"+"::s) [] rs)

end
