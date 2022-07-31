(*
 * This signature specifies the MLRISC annotations for describing
 * memory aliasing and control dependence.
 *
 * -- Allen
 *)

signature MLRISC_REGION =
sig

   type region 

   datatype mutability = 
     READONLY    (* readonly regions are never written to *)
   | IMMUTABLE   (* immutable region are never updated once it is initialized *)
   | MUTABLE     (* mutable regions can be updated *)

   val memory : region  (* root of the memory hierarchy *)

   val heap     : region  (* heap region *)
   val stack    : region  (* stack region *)
   val data     : region  (* global data region *)
   val readonly : region  (* read only data region *)

   val new      : string * mutability * region -> region
   val union    : region list -> region 

   val toString : region -> string

end
