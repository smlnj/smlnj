(* Basics/print/newpputil.sig *)

signature NEW_PP_UTIL =
sig

val vHeaders : {header1: string, header2: string, formatter: 'a -> NewPP.format}
	       -> 'a list
	       -> NewPP.format

val vHeaderFormats : {header1: string, header2: string}
		     -> NewPP.format list
		     -> NewPP.format

end (* signature NEW_PP_UTIL *)
