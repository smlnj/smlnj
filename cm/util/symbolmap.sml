(*
 * Symbol dictionaries.
 *   Hooks into compiler and uses SML/NJ library implementation of binary maps.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure SymbolMap = MapFn (SymbolOrdKey)
