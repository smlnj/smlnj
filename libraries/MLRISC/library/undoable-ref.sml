(*
 *  A reference that allows undo.
 *
 *  -- Allen
 *)

signature UNDOABLE_REF =
sig
   eqtype 'a uref 
   val uref : 'a -> 'a uref
   val !   : 'a uref -> 'a
   val :=  : 'a uref * 'a -> unit
end

functor UndoableRef (Log : TRANSACTION_LOG) : UNDOABLE_REF =
struct

   type 'a uref = 'a ref * Log.version ref 

   fun uref a = (ref a, ref(!Log.version))

   fun !! (r,_) = !r

   fun commit (x,v) = fn ver => v := ver

   fun rollback (x,v) = 
   let val x' = !x
   in  fn ver => (x := x'; v := ver)
   end

   fun ::= (r as (x,v),y) = 
   let val ver = !Log.version
   in  if !v <> ver then (Log.add_object{rollback = rollback r,
					 commit   = commit r
					}; 
			  v := ver)
       else ();
       x := y
   end

   val !  = !!
   val op := = ::=
end

