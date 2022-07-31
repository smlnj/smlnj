(*
 * Bag datatype that uses hashing
 * 
 * -- Allen
 *)

signature HASH_BAG =
sig

   type 'a bag

   val create : { order : 'a * 'a -> order,
                  hash  : 'a -> int,
                  exn   : exn
                } -> int -> 'a bag 

   val size       : 'a bag -> int
   val bucketSize : 'a bag -> int
   val isEmpty    : 'a bag -> bool
   val insert     : 'a bag -> 'a -> unit
   val insertN    : 'a bag -> 'a * int -> unit
   val remove     : 'a bag -> 'a -> unit
   val removeN    : 'a bag -> 'a * int -> unit
   val removeAll  : 'a bag -> 'a -> unit
   val toList     : 'a bag -> ('a * int) list
   val toDupList  : 'a bag -> 'a list
   val clear      : 'a bag -> unit
   val contains   : 'a bag -> 'a -> bool
   val count      : 'a bag -> 'a -> int
   val app        : ('a * int -> unit) -> 'a bag -> unit
   val dupApp     : ('a -> unit) -> 'a bag -> unit
   val fold       : (('a * int) * 'b -> 'b) -> 'b -> 'a bag -> 'b
   val dupFold    : ('a * 'b -> 'b) -> 'b -> 'a bag -> 'b
   val toString   : ('a -> string) -> 'a bag -> string

end

