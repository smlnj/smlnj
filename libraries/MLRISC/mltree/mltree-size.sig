(*
 * This module provides functions for computing the size MLTREE transformations.
 * Basically, we want to support various non built-in datatype widths.
 * This module handles the translation. 
 *
 * -- Allen
 *)
signature MLTREE_SIZE =
sig

   structure T : MLTREE

   val intTy : int (* natural width of integers *)

   (*
    * Return the size of an expression
    *)
   val size  : T.rexp -> T.ty
   val fsize : T.fexp -> T.ty

end
