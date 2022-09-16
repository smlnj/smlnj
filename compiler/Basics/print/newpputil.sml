(* Basics/print/newpputil.sml *)

structure NewPPUtil =
struct

local
  structure S = Symbol
  structure PP = NewPP
in    

(* fmtSym: S.symbol -> PP.format *)
fun fmtSym sym = PP.text (S.name sym)

(* could also vertically align the element formats, taking account of the possibly
 * different sizes of the header strings *)

fun fmtVerticalList {header1: string, header2: string, formatter: 'a -> PP.format}
		    (elems: 'a list) =
      case elems
	of nil => PP.empty
	 | elem :: rest =>
	     PP.vblock
	       (PP.hcat (PP.text header1, formatter elem) ::
	        map (fn e => PP.hcat (PP.text header2), formatter e)
		    rest)

fun fmtVerticalFormats {header1: string, header2: string} (elems: PP.format list) =
      case elems
	of nil => PP.empty
	 | elem :: rest =>
	     PP.vblock
	       (PP.hcat (PP.text header1, elem) ::
	        map (fn fmt => PP.hcat (PP.text header2), fmt)
		    rest)

end (* top local *)
end (* structure NewPPUtil *)
