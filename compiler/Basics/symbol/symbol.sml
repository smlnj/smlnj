(* symbol.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)
structure Symbol = struct
  val varInt = 0w0 and sigInt = 0w1 and strInt = 0w2 and fsigInt = 0w3 and
      fctInt = 0w4 and tycInt = 0w5 and labInt = 0w6 and tyvInt = 0w7 and
      fixInt = 0w8

  datatype symbol = SYMBOL of word * string
  datatype namespace =
     VALspace | TYCspace | SIGspace | STRspace | FCTspace | FIXspace |
     LABspace | TYVspace | FSIGspace

  fun eq(SYMBOL(a1,b1),SYMBOL(a2,b2)) = a1=a2 andalso b1=b2
  fun symbolGt(SYMBOL(_,s1), SYMBOL(_,s2)) = s1 > s2
  fun symbolCMLt (SYMBOL (a1, s1), SYMBOL (a2, s2)) =
        a1 < a2 orelse a1 = a2 andalso s1 < s2
  fun compare(SYMBOL(a1,s1),SYMBOL(a2,s2)) =
      case Word.compare(a1,a2)
       of EQUAL => String.compare(s1,s2)
	| order => order

  fun varSymbol (name: string) =
        SYMBOL(HashString.hashString name + varInt,name)
  fun tycSymbol (name: string) =
	SYMBOL(HashString.hashString name + tycInt, name)
  fun fixSymbol (name: string) =
	SYMBOL(HashString.hashString name + fixInt, name)
  fun labSymbol (name: string) =
	SYMBOL(HashString.hashString name + labInt, name)
  fun tyvSymbol (name: string) =
	SYMBOL(HashString.hashString name + tyvInt, name)
  fun sigSymbol (name: string) =
        SYMBOL(HashString.hashString name + sigInt, name)
  fun strSymbol (name: string) =
	SYMBOL(HashString.hashString name + strInt, name)
  fun fctSymbol (name: string) =
	SYMBOL(HashString.hashString name + fctInt, name)
  fun fsigSymbol (name: string) =
	SYMBOL(HashString.hashString name + fsigInt, name)

  fun var'n'fix name =
        let val h = HashString.hashString name
	 in (SYMBOL(h+varInt,name),SYMBOL(h+fixInt,name))
	end

  fun name (SYMBOL(_,name)) = name
  fun number (SYMBOL(number,_)) = number
  fun nameSpace (SYMBOL(number,name)) : namespace =
        case number - HashString.hashString name
	 of 0w0 => VALspace
          | 0w5 => TYCspace
          | 0w1 => SIGspace
          | 0w2 => STRspace
          | 0w4 => FCTspace
          | 0w8 => FIXspace
          | 0w6 => LABspace
          | 0w7 => TYVspace
	  | 0w3 => FSIGspace
	  | _ => ErrorMsg.impossible "Symbol.nameSpace"

  fun nameSpaceToString (n : namespace) : string =
        case n
         of VALspace => "variable or constructor"
          | TYCspace => "type constructor"
          | SIGspace => "signature"
          | STRspace => "structure"
          | FCTspace => "functor"
          | FIXspace => "fixity"
          | LABspace => "label"
	  | TYVspace => "type variable"
	  | FSIGspace => "functor signature"

  fun describe s = concat [nameSpaceToString (nameSpace s), " ", name s]

  fun symbolToString(SYMBOL(number,name)) : string =
        case number - HashString.hashString name
         of 0w0 => "VAL$"^name
          | 0w1 => "SIG$"^name
          | 0w2 => "STR$"^name
          | 0w3 => "FSIG$"^name
          | 0w4 => "FCT$"^name
          | 0w5 => "TYC$"^name
          | 0w6 => "LAB$"^name
          | 0w7 => "TYV$"^name
          | 0w8 => "FIX$"^name
          | _ => ErrorMsg.impossible "Symbol.toString"
end (* structure Symbol *)
