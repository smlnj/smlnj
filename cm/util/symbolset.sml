(*
 * Sets of symbols.
 *   Hooks into compiler and uses SML/NJ library implementation of sets.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure SymbolSet = SetFn (SymbolOrdKey)
