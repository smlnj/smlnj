(*
 * Multimap datatype that uses hashing.
 *
 * -- allen
 *)

signature HASH_MULTIMAP =
sig

   type ('a,'b) multimap

   val create : { order : 'a * 'a -> order,
                  hash  : 'a -> int,
                  exn   : exn
                } -> int -> ('a,'b) multimap 

   val size       : ('a,'b) multimap -> int
   val bucketSize : ('a,'b) multimap -> int
   val isEmpty    : ('a,'b) multimap -> bool
   val insert     : ('a,'b) multimap -> ('a * 'b) -> unit
   val update     : ('a,'b) multimap -> ('a * 'b list) -> unit
   val removeAll  : ('a,'b) multimap -> 'a -> unit
   val lookup     : ('a,'b) multimap -> 'a -> 'b list
   val toList     : ('a,'b) multimap -> ('a * 'b list) list
   val toDupList  : ('a,'b) multimap -> ('a * 'b) list
   val clear      : ('a,'b) multimap -> unit
   val contains   : ('a,'b) multimap -> 'a -> bool
   val count      : ('a,'b) multimap -> 'a -> int
   val app        : ('a * 'b list -> unit) -> ('a,'b) multimap -> unit
   val dupApp     : ('a * 'b -> unit) -> ('a,'b) multimap -> unit
   val fold       : (('a * 'b list) * 'c -> 'c) -> 'c 
                        -> ('a,'b) multimap -> 'c
   val dupFold    : (('a * 'b) * 'c -> 'c) -> 'c -> ('a,'b) multimap -> 'c
   val toString   : (('a -> string) * ('b -> string)) -> ('a,'b) multimap
                        -> string

end

