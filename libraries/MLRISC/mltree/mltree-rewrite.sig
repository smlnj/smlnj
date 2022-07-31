(* A rewrite function for MLTree datatypes
 * Useful for performing transformation on MLTree.
 * The signature is a bit hairy since we have to deal with extensions.
 *)
signature MLTREE_REWRITE =
sig
   structure T : MLTREE

   val rewrite : 
       (* User supplied transformations *)
       { rexp  : (T.rexp -> T.rexp) -> (T.rexp -> T.rexp), 
         fexp  : (T.fexp -> T.fexp) -> (T.fexp -> T.fexp),
         ccexp : (T.ccexp -> T.ccexp) -> (T.ccexp -> T.ccexp),
         stm   : (T.stm -> T.stm) -> (T.stm -> T.stm)
       } -> T.rewriter
end
