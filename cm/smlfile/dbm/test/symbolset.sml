(* smlfile/dbm/test/symbolset.sml 
 *
 * Sets of symbols.
 *   Hooks into compiler and uses SML/NJ library implementation of sets.
 *)

structure SymbolSet =
RedBlackSetFn
  (struct
     type ord_key = Symbol.symbol
     val compare = Symbol.compare
   end)
