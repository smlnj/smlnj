(* 
 *  Common operations on MLTREE
 *
 * -- Allen 
 *)
signature MLTREE_UTILS =
sig

   structure T : MLTREE 

   (*
    * Hashing
    *)
   val hashStm   : T.stm -> word
   val hashRexp  : T.rexp -> word
   val hashFexp  : T.fexp -> word
   val hashCCexp : T.ccexp -> word

   (*
    * Equality
    *)
   val eqStm     : T.stm * T.stm -> bool
   val eqRexp    : T.rexp * T.rexp -> bool
   val eqFexp    : T.fexp * T.fexp -> bool
   val eqCCexp   : T.ccexp * T.ccexp -> bool
   val eqMlriscs : T.mlrisc list * T.mlrisc list -> bool

   (*
    * Pretty printing 
    *)
   val show : {def       : int -> string, 
               use       : int -> string,
               regionDef : T.Region.region -> string,
               regionUse : T.Region.region -> string
              } -> T.printer  

   val stmToString   : T.stm -> string
   val rexpToString  : T.rexp -> string
   val fexpToString  : T.fexp -> string
   val ccexpToString : T.ccexp -> string

end
