(*
 * This is a simple cache datatype.
 *
 * -- Allen
 *) 

signature CACHE_REF =
sig

   type 'a cache 

   val cache : ('a -> 'b) -> 'a -> 'b cache
   val flush : 'a cache -> unit
   val !     : 'a cache -> 'a
   val :=    : 'a cache * 'a -> unit

end

structure CacheRef :> CACHE_REF =
struct

   type 'a cache = 'a option ref * (unit -> 'a)

   fun cache f x = (ref NONE, fn _ => f x)

   fun flush (x,_) = x := NONE

   fun ! (r as ref NONE,f)    = let val x = f() in r := SOME x; x end
     | ! (r as ref(SOME x),f) = x

   val op := = fn((r, _),x) => r := SOME x

end 

