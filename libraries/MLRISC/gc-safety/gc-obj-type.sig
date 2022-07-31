(*
 * Interface for client defined object representation types
 *)

signature OBJ_TYPE =
sig

   type objtype

   val toString : objtype -> string

end
