(*
 * Argument for SetFn and MapFn for the case of symbols.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure SymbolOrdKey = struct
    type ord_key = Symbol.symbol
    fun compare (s1, s2) =
	if Symbol.symbolCMLt (s1, s2) then LESS
	else if Symbol.eq (s1, s2) then EQUAL
	else GREATER
end
