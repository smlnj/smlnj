(* ElabData/basics/pathname.sml *)

structure PathName =
struct

local
  structure S = Symbol
  structure SP = SymPath
  structure IP = InvPath
		       
in

    val defaultVar : S.symbol = S.varSymbol "<var.???>"
    val defaultTyc : S.symbol = S.tycSymbol "<tyc.???>"
    val defaultStr : S.symbol = S.strSymbol "<str.???>"
    val defaultSig : S.symbol = S.sigSymbol "<sig.???>"
    val defaultFct : S.symbol = S.fctSymbol "<fct.???>"

    (* getNameSP : S.symbol -> SP.path -> S.symbol *)
    (* get the name of a binding, checking for empty path *)
    fun getNameSP (default : S.symbol) (SP.SPATH syms: SP.path) =
	(case syms
	   of nil => default
	    | _ => List.last syms) (* the actual last symbol in the SP.path *)

    (* getNameIP : S.symbol -> IP.path -> S.symbol *)
    (* get the name of a binding, checking for empty rpath *)
    fun getNameIP (default : S.symbol) (IP.IPATH syms: IP.path) =
	(case syms
	  of nil => default
	   | sym :: _ => sym) (* "last", i.e. first symbol in the IP.path *)

    val getVarNameSP = getNameSP defaultVar
    val getTycNameSP = getNameSP defaultTyc
    val getStrNameSP = getNameSP defaultStr
    val getSIGNameSP = getNameSP defaultSig
    val getFctNameSP = getNameSP defaultFct

    val getVarNameIP = getNameIP defaultVar
    val getTycNameIP = getNameIP defaultTyc
    val getStrNameIP = getNameIP defaultStr
    val getSIGNameIP = getNameIP defaultSig
    val getFctNameIP = getNameIP defaultFct

end (* top local *)
end (* structure PathName *)
