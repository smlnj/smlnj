(* pptable.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)

(* NOT USED! *)

structure CompilerPPTable =
struct

    val installFormatter : string list -> ('a -> NewPP.format) -> unit =
        Unsafe.cast PPTable.installFormatter

end (* CompilerPP
Table *)
