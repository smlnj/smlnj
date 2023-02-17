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

end
