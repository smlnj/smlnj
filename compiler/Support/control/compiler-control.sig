(* Admin/control/codegen-control.sig
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature COMPILER_CONTROL =
sig

  val debugging : bool ref   (* a debugging flag for the whole compiler !? *)

  val saveLambda : bool ref
  val preserveLvarNames : bool ref
  val trackExn : bool ref
  val polyEqWarn : bool ref

  val saveit : bool ref
  val saveAbsyn : bool ref
  val saveConvert : bool ref
  val saveCPSopt : bool ref
  val saveClosure : bool ref

(* val tdp_instrument : bool ref -- investigate! Who would miss it? *)

end (* signature COMPILER_CONTROL *)
