(*
 * Signature of the hash table datatype
 * 
 * -- Allen
 *)

signature HASHTABLE =
sig

   type ('a,'b) table

   val create : { hash : 'a -> word, 
                  ==   : 'a * 'a -> bool,
                  exn  : exn,
                  size : int 
                } -> ('a,'b) table 

   val size         : ('a,'b) table -> int
   val clear        : ('a,'b) table -> unit
   val insert       : ('a,'b) table -> 'a * 'b -> unit
   val remove       : ('a,'b) table -> 'a -> unit
   val lookup       : ('a,'b) table -> 'a -> 'b 
   val copy         : ('a,'b) table -> ('a,'b) table
   val app          : ('a * 'b -> unit) -> ('a,'b) table -> unit
   val map          : ('a * 'b -> 'c) -> ('a,'b) table -> 'c list
   val fold         : ('a * 'b * 'c -> 'c) -> 'c -> ('a,'b) table -> 'c

end

