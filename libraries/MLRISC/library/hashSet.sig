(*
 * A set datatype that uses hashing
 *
 * -- Allen
 *)

signature HASH_SET =
sig

   type 'a set

   val create : { order : 'a * 'a -> order,
                  hash  : 'a -> int
                } -> int -> 'a set 

   val size       : 'a set -> int
   val bucketSize : 'a set -> int
   val isEmpty    : 'a set -> bool
   val insert     : 'a set -> 'a -> unit
   val remove     : 'a set -> 'a -> unit
   val toList     : 'a set -> 'a list
   val clear      : 'a set -> unit
   val contains   : 'a set -> 'a -> bool
   val app        : ('a -> unit) -> 'a set -> unit
   val fold       : ('a * 'b -> 'b) -> 'b -> 'a set -> 'b
   val toString   : ('a -> string) -> 'a set -> string

end

