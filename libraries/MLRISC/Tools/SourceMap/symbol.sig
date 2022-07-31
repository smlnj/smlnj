(* 
 * Maps strings to unique symbols. 
 * This saves space and makes equality testing and hashing quicker
 *)
signature UNIQUE_SYMBOL =
sig

   eqtype symbol 

   val equal      : symbol * symbol -> bool
   val compare    : symbol * symbol -> order
   val hash       : symbol -> word
   val fromString : string -> symbol  
   val toString   : symbol -> string

end
