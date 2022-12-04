(* FLINT/flint/ppflint-new.sig
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Signature PPFLINT of pretty printer for FLINT IR, using the NewPrettyPrint library.
 *)

(* This PPFLINT replaces th old PRINT_FLINT and the previous PPFLINT.
 * The implementation in PPFlint (FLINT/flint/ppflint.sml) uses the NewPP prettyprinter *)

signature PPFLINT =
sig

  (* formatter functions for FLINT types *)
  val fmtFKind : FunRecMeta.fkind -> NewPrettyPrint.format
  val fmtRKind : FunRecMeta.rkind -> NewPrettyPrint.format
  val fmtCon   : PLambda.con -> NewPrettyPrint.format
  val fmtValue : FLINT.value -> NewPrettyPrint.format         (* was printSval *)
  val fmtFundec: int -> FLINT.fundec -> NewPrettyPrint.format  (* also applies to FLINT.prog *)
  val fmtLexp  : int -> FLINT.lexp -> NewPrettyPrint.format

  (* "top-level" printing functions *)
  val ppLexp : FLINT.lexp -> unit
    (* controlled by Control.FLINT.printDepth and Control.FLINT.lineWidth *)
  val ppLexpLimited : int -> FLINT.lexp -> unit
    (* controlled by Control.FLINT.lineWidth *)
  val ppProg : FLINT.prog -> unit
    (* controlled by Control.FLINT.printDepth and Control.FLINT.lineWidth *)
  val ppProgLimited : int -> FLINT.prog -> unit
    (* controlled by Control.FLINT.lineWidth *)

  val valueToString : FLINT.value -> string

end (* signature PPFLINT *)
