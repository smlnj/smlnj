(* COPYRIGHT (c) 1997 YALE FLINT PROJECT *)
(* ppflint.sig -- Pretty printer for Flint IL. *)

signature PRINT_FLINT =
sig
    val printFKind : FunRecMeta.fkind -> unit
    val printRKind : FunRecMeta.rkind -> unit
    val printCon   : PLambda.con -> unit
    val printValue : FLINT.value -> unit
    val printLexp  : FLINT.lexp -> unit
    val printFundec: FLINT.fundec -> unit
    val printProg  : FLINT.prog -> unit

    val printTyc : Lty.tyc -> unit
    val printLty : Lty.lty -> unit
    val printTycList : Lty.tyc list -> unit
    val printLtyList : Lty.lty list -> unit

    (* defaults to LV.lvarName *)
    val lvarToStringRef  : (LambdaVar.lvar -> string) ref

    val valueToString : FLINT.value -> string

end (* signature PRINT_FLINT *)
