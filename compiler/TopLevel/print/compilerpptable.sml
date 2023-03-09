(* pptable.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)

(* NOT USED! *)

structure CompilerPPTable =
struct

    val installFormatter : string list -> ('a -> Formatting.format) -> unit =
        Unsafe.cast PPTable.installFormatter

end (* structure CompilerPPTable *)
