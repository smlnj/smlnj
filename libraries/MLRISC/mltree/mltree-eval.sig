(* mltree-eval.sig
 *
 * COPYRIGHT (c) 2001 Lucent Technologies, Bell Laboratories.
 *
 * Utilites to evaluate and compare mltree expressions.
 *)

signature MLTREE_EVAL = sig
  structure T : MLTREE

   (*
    * Equality
    *)
  val eqStm     : T.stm * T.stm -> bool
  val eqRexp    : T.rexp * T.rexp -> bool
  val eqFexp    : T.fexp * T.fexp -> bool
  val eqCCexp   : T.ccexp * T.ccexp -> bool
  val eqMlriscs : T.mlrisc list * T.mlrisc list -> bool
  val ==       : T.labexp * T.labexp -> bool


  (* 
   * Value
   *)
  exception NonConst
  val eval : 
      {const:T.Constant.const -> IntInf.int,
       label:Label.label -> int} 
     -> 
      {rexp : T.rexp -> IntInf.int,
       ccexp : T.ccexp -> bool}

  val valueOf : T.labexp -> int
end
