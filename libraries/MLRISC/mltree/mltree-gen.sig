(*
 * This module provides various generic MLTREE transformations.
 * Basically, we want to support various non built-in datatype widths.
 * This module handles the translation. 
 *
 * -- Allen
 *)
signature MLTREEGEN =
sig

   structure T : MLTREE
   structure Size : MLTREE_SIZE
   	where T = T

   val condOf : T.ccexp -> T.Basis.cond
   val fcondOf : T.ccexp -> T.Basis.fcond

   (* 
    * Perform simplification
    *)
   val compileRexp : T.rexp -> T.rexp
   val compileFexp : T.fexp -> T.fexp
   val compileStm  : T.stm  -> T.stm list
  
   (*
    * Simulate conditional expression. 
    *)
   val compileCond : 
       {exp : T.ty * T.ccexp * T.rexp * T.rexp,
        an  : Annotations.annotations,
        rd  : CellsBasis.cell
       } -> T.stm list

   val compileFcond : 
       {exp : T.fty * T.ccexp * T.fexp * T.fexp,
        an  : Annotations.annotations,
        fd  : CellsBasis.cell
       } -> T.stm list


end
