(*
 * Start a transaction
 * 
 * -- Allen
 *)

signature TRANSACTION =
sig

   exception Abort

   val transaction : 'a -> (unit -> 'a) -> 'a

end

