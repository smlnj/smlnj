(*
 * Register set datatype
 *
 * -- Allen
 *)

signature REGISTER_SET =
sig

   type regset 
   type reg = int

   val empty          : regset
   val fromList       : reg list -> regset
   val sort           : reg list -> reg list
   val fromSortedList : reg list -> regset
   val insert         : regset * reg -> regset 
   val remove         : regset * reg -> regset 
   val insertChanged  : regset * reg -> regset * bool
   val removeChanged  : regset * reg -> regset * bool
   val ==             : regset * regset -> bool
   val app            : (reg -> unit) -> regset -> unit
   val contains       : regset * reg -> bool
   val exists         : regset * reg list -> bool
   val isEmpty        : regset -> bool
   val toList         : regset -> reg list
   val toString       : regset -> string
   val union          : regset list -> regset
   val intersects     : regset list -> regset
   val +              : regset * regset -> regset
   val -              : regset * regset -> regset
   val *              : regset * regset -> regset

end

