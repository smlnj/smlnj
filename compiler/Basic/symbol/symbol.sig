(* Copyright 1989 by AT&T Bell Laboratories *)
signature SYMBOL = sig
    type symbol
    datatype namespace =
       VALspace | TYCspace | SIGspace | STRspace | FCTspace | FIXspace |
       LABspace | TYVspace | FSIGspace
    val eq: symbol * symbol -> bool
    and symbolGt : symbol * symbol -> bool
    and symbolCMLt : symbol * symbol -> bool
    and varSymbol: string -> symbol
    and tycSymbol: string -> symbol
    and sigSymbol: string -> symbol
    and strSymbol: string -> symbol
    and fctSymbol: string -> symbol
    and fsigSymbol: string -> symbol
    and fixSymbol: string -> symbol
    and labSymbol: string -> symbol
    and tyvSymbol: string -> symbol
    and var'n'fix : string -> symbol * symbol
    and name: symbol -> string
    and number: symbol -> word
    val nameSpace : symbol -> namespace
    val nameSpaceToString : namespace -> string
    val describe : symbol -> string
    val symbolToString : symbol -> string
    val compare : symbol * symbol -> order
(* Probably should merge STRspace and FCTspace into one namespace.
   Similarly for SIGspace and FSIGspace. *)
end
