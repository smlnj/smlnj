(* 
 * This implements a transaction log.
 *
 * -- Allen
 *)

signature TRANSACTION_LOG =
sig

   exception TransactionLog

   type version = int
   val version      : version ref
   val add_object   : { rollback : version -> unit, 
			commit   : version -> unit } -> unit   
   val begin        : unit -> unit
   val commit       : unit -> unit
   val abort        : unit -> unit
   val init         : unit -> unit
end

