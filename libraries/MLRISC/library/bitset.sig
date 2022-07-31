(*
 * Dense set in bitvector format.
 * 
 * -- Allen
 *)

signature BITSET =
sig

   type bitset 

   val create        : int -> bitset
   val size          : bitset -> int
   val contains      : bitset * int -> bool
   val set           : bitset * int -> unit
   val reset         : bitset * int -> unit
   val clear         : bitset -> unit
   val markAndTest   : bitset * int -> bool
   val unmarkAndTest : bitset * int -> bool
   val toString      : bitset -> string

end

