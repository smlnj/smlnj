(* fastsymbol.sig
 *
 * (C) 2001 Lucent Technologies, Bell labs
 *)
signature FASTSYMBOL = sig
    type raw_symbol
    type symbol
    val rawSymbol: word * string -> raw_symbol
    val sameSpaceSymbol : symbol -> raw_symbol -> symbol
    val varSymbol: raw_symbol -> symbol
    val tycSymbol: raw_symbol -> symbol
    val sigSymbol: raw_symbol -> symbol
    val strSymbol: raw_symbol -> symbol
    val fctSymbol: raw_symbol -> symbol
    val fixSymbol: raw_symbol -> symbol
    val labSymbol: raw_symbol -> symbol
    val tyvSymbol: raw_symbol -> symbol
    val fsigSymbol: raw_symbol -> symbol
    val var'n'fix : raw_symbol -> symbol * symbol
end
