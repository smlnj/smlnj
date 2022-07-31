(*
 * Performs simple local optimizations.
 * Constant folding, algebraic simplication and some dead code elimination.
 *)
signature MLTREE_SIMPLIFIER =
sig

   structure T : MLTREE

   type simplifier = T.rewriter
   val simplify : 
       { addressWidth : int,  (* width of address in bits *)
         signedAddress : bool (* is the address computation signed? *)
       } -> simplifier
   
end
