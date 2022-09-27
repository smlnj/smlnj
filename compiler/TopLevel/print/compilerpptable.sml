(* pptable.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)

(* NOT USED! *)

structure CompilerPPTable =
struct

    val install_formatter : string list -> ('a -> NewPP.format) -> unit =
        Unsafe.cast PPTable.install_pp

end (* CompilerPPTable *)
