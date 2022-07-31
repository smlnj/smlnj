(*
 * The internal symbol that is used to find "core" bindings.  This symbol
 * is "structure _Core", which is something that cannot legally occur in
 * normal SML code, so there is no danger of accidentially overriding
 * this binding.
 * 
 * (C) 2000 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure CoreSym = struct
    val coreSym = Symbol.strSymbol "_Core"
end
