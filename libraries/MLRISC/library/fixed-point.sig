(* 
 * A simple fixed point datatype
 * 
 * -- Allen
 *)

signature FIXED_POINT =
sig
   type fixed_point = Word31.word

   val fixed_point  : int * int -> fixed_point

   val zero     : fixed_point
   val one      : fixed_point

   val compare  : fixed_point * fixed_point -> order

   val *        : fixed_point * fixed_point -> fixed_point
   val /        : fixed_point * fixed_point -> fixed_point
   val scale    : fixed_point * int -> fixed_point
   val div      : fixed_point * int -> fixed_point
   val min      : fixed_point * fixed_point -> fixed_point
   val max      : fixed_point * fixed_point -> fixed_point

   val toString : fixed_point -> string
   val toReal   : fixed_point -> real
   val toWord   : fixed_point -> word
   val fromReal : real -> fixed_point
   val fromInt  : int -> fixed_point
end

