(*
 * Let's generate good multiplication/division code!
 *
 * -- Allen 
 *)
signature MLTREE_MULT_DIV =
sig

   structure T : MLTREE
   structure C : CELLS 
   structure I : INSTRUCTIONS where C=C

   exception TooComplex

   val multiply : {r:CellsBasis.cell,i:int,d:CellsBasis.cell} -> I.instruction list 

   val divide   : { mode:T.Basis.rounding_mode,
                    stm :T.stm -> unit
                  } -> {r:CellsBasis.cell,i:int,d:CellsBasis.cell} -> I.instruction list

end
