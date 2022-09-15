(* Basics/print/newpputil.sig *)

structure NEW_PP_UTIL =
struct

val fmtSym: Symbol.symbol -> NewPP.format

val fmtVerticalList : {header1: string, header2: string, formatter: 'a -> NewPP.format}
		      -> 'a list
		      -> NewPP.format

val fmtVerticalFormats : {header1: string, header2: string}
			    -> NewPP.format list
			    -> NewPP.format

end (* structure NewPPUtil *)
