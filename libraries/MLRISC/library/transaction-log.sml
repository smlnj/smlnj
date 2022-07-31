(*
 * This implements a transaction log.  This is used
 * for undoable data structures.
 *
 * -- Allen
 *)

structure TransactionLog : TRANSACTION_LOG =
struct
   exception TransactionLog

   type version    = int
   val version     = ref 0
   val log         = ref [] : (version * { rollback : version -> unit,
					   commit   : version -> unit
					 } list ref) list ref
   fun add_object f =
       case !log of
	  (ver,trail)::_ => trail := f :: !trail 
       |  []             => raise TransactionLog

   fun init() = (version := 0; log := [])

   fun begin () =
   let val new_ver = !version+1
   in  version := new_ver;
       log := (new_ver,ref []) :: !log
   end

   fun abort () =
   let val old_ver = !version - 1
   in  case !log of
	  (_,ref trail)::rest => 
	     (app (fn {rollback,...} => rollback old_ver) trail;
              version := old_ver;
	      log := rest) 
       |  []                  => raise TransactionLog
   end

   fun commit () =
   let val old_ver = !version - 1
   in  case !log of
	  (_,ref trail)::rest => 
	     (app (fn {commit,...} => commit old_ver) trail;
              version := old_ver;
	      log := rest) 
       |  []      => raise TransactionLog
   end
end

