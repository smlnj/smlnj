(* fastsymbol.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)
structure FastSymbol = struct
  local open Symbol
  in

  type symbol = symbol

  (* Another version of symbols but hash numbers have no increments
   * according to their nameSpace *)
  datatype raw_symbol = RAWSYM of word * string

  (* builds a raw symbol from a pair name, hash number *)
  fun rawSymbol hash_name = RAWSYM hash_name

  (* builds a symbol from a raw symbol belonging to the same space as
   * a reference symbol *)
  fun sameSpaceSymbol (SYMBOL(i,s)) (RAWSYM(i',s')) =
        SYMBOL(i' + (i - HashString.hashString s), s')

  (* build symbols in various name space from raw symbols *)
  fun varSymbol (RAWSYM (hash,name)) =
        SYMBOL(hash + varInt,name)
  fun tycSymbol (RAWSYM (hash,name)) =
        SYMBOL(hash + tycInt, name)
  fun fixSymbol (RAWSYM (hash,name)) =
        SYMBOL(hash + fixInt, name)
  fun labSymbol (RAWSYM (hash,name)) =
        SYMBOL(hash + labInt, name)
  fun tyvSymbol (RAWSYM (hash,name)) =
        SYMBOL(hash + tyvInt, name)
  fun sigSymbol (RAWSYM (hash,name)) =
        SYMBOL(hash + sigInt, name)
  fun strSymbol (RAWSYM (hash,name)) =
        SYMBOL(hash + strInt, name)
  fun fctSymbol (RAWSYM (hash,name)) =
        SYMBOL(hash + fctInt, name)
  fun fsigSymbol (RAWSYM (hash,name)) =
        SYMBOL(hash + fsigInt, name)
  fun var'n'fix (RAWSYM (h,name)) =
        (SYMBOL(h+varInt,name),SYMBOL(h+fixInt,name))

  end (* local FastSymbol *)
end (* structure FastSymbol *)
