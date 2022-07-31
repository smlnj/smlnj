(* labelExp.sml -- expressions involving labels
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)
signature LABELEXP = 
sig
  structure T  : MLTREE
  val valueOf  : T.labexp -> int
  val toString : T.labexp -> string
  val hash     : T.labexp -> word
  val ==       : T.labexp * T.labexp -> bool

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
    * Value 
    *)
   exception NonConst
   val eval : {const:T.Constant.const -> IntInf.int,
               label:Label.label -> int} -> 
              {rexp : T.rexp -> IntInf.int,
               ccexp : T.ccexp -> bool
              }
end

