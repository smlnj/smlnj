(* Admin/control/compiler-control.sml
 *
 * (C) 2023 The Fellowship of SML/NJ (www.smlnj.org)
 *)

signature COMPILER_CONTROL =
sig
  val printWarnings : bool ref
  (* if false, suppress all warning messages *)
end

structure CompilerControl : COMPILER_CONTROL =
struct

  val printWarnings = ref true

  val debugging = new ("debugging", "?", false)

  val trackExn = ref true (* "whether to generate code that tracks exceptions" *)

  (* warning message when call of polyEqual compiled: *)
  val polyEqWarn = ref true (* "whether to warn about calls of polyEqual" *)

  val preserveLvarNames = ref false (* "preserve-names?" *)

  (* these are really all the same flag (ref cell): *)
  val saveit : bool ref = ElabDataControl.saveLvarNames
  val saveAbsyn : bool ref = saveit
  val saveLambda : bool ref = saveit
  val saveConvert : bool ref = saveit
  val saveCPSopt : bool ref = saveit
  val saveClosure : bool ref = saveit

(*  val tdp_instrument = TDPInstrument.enabled ??? -- needs investigation! *)

end (* structure CompilerControl *)
