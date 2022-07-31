(* ppflint.sig
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Signature of new pretty printer (PPFlint) for FLINT IR.
 *)

signature PPFLINT =
  sig

    (* pretty printing functions *)
    val ppFKind : PrettyPrint.stream -> FunRecMeta.fkind  -> unit
    val ppRKind : PrettyPrint.stream -> FunRecMeta.rkind  -> unit
    val ppCon   : PrettyPrint.stream -> PLambda.con    -> unit
    val ppValue : PrettyPrint.stream -> FLINT.value  -> unit  (* was printSval *)
    val ppFundec: int -> PrettyPrint.stream -> FLINT.fundec -> unit  (* also takes FLINT.prog *)
    val ppLexp : PrettyPrint.stream -> (FLINT.lexp * int) -> unit

    (* "top-level" printing functions *)
    val printLexp : FLINT.lexp -> unit
      (* controlled by Control.FLINT.printDepth and Control.FLINT.lineWidth *)
    val printLexpLimited : FLINT.lexp * int -> unit
      (* controlled by Control.FLINT.lineWidth *)
    val printProg : FLINT.prog -> unit
      (* controlled by Control.FLINT.printDepth and Control.FLINT.lineWidth *)
    val printProgLimited : FLINT.prog * int -> unit
      (* controlled by Control.FLINT.lineWidth *)

    val valueToString : FLINT.value -> string

  end (* signature PPFLINT *)
