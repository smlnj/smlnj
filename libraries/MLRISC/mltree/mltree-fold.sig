(* A fold function for MLTree datatypes
 * Useful for performing transformation on MLTree
 *)
signature MLTREE_FOLD =
sig
   structure T : MLTREE

   val fold : 'b T.folder -> 'b T.folder
end
