(* Copyright (c) 1997 YALE FLINT PROJECT *)
(* pflatten.sml *)

signature PFLATTEN =
sig

    (* the following functions are used in flintnm.sml *)

    val t_pflatten   : Lty.lty -> bool * Lty.lty list * bool

    val v_punflatten : Lty.lty ->
		       (bool * Lty.lty list * bool) *
                       ((LambdaVar.lvar * FLINT.lexp) -> (LambdaVar.lvar list * FLINT.lexp))

    val v_pflatten   : Lty.lty ->
		       (bool * Lty.lty list * bool) *
                       (FLINT.value -> (FLINT.value list * (FLINT.lexp -> FLINT.lexp)))

    (* the following functions are used during type specialization in FLINT *)
    val t_flatten    : (Lty.lty list * bool) -> bool * Lty.lty list * bool

    val v_unflatten  : (Lty.lty list * bool) -> 
                       (bool * Lty.lty list * bool) *
                       ((LambdaVar.lvar list * FLINT.lexp) -> (LambdaVar.lvar list * FLINT.lexp))

    val v_flatten    : (Lty.lty list * bool) -> 
                       (bool * Lty.lty list * bool) *
                       (FLINT.value list -> (FLINT.value list * (FLINT.lexp -> FLINT.lexp)))

    (* the following function is used by representation analysis in FLINT *)
    val v_coerce     : bool * Lty.tyc list * Lty.tyc list -> 
                       Lty.tyc list *
		       (FLINT.value list -> (FLINT.value list * (FLINT.lexp -> FLINT.lexp))) option

end (* signature PFLATTEN *)




