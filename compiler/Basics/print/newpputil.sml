(* Basics/print/newpputil.sml *)

structure NewPPUtil : NEW_PP_UTIL =
struct

local
  structure PP = NewPP
in    

(* TODO: Vertically align the element formats, taking account of the possibly
 * that the two header strings have different sizes by left-padding the 
 * shorter header. *)

fun vHeaders {header1: string, header2: string, formatter: 'a -> PP.format}
		    (elems: 'a list) =
      case elems
	of nil => PP.empty
	 | elem :: rest =>
	     PP.vblock
	       (PP.hcat (PP.text header1, formatter elem) ::
	        map (fn e => PP.hcat (PP.text header2, formatter e)) rest)

fun vHeaderFormats {header1: string, header2: string} (elems: PP.format list) =
      case elems
	of nil => PP.empty
	 | elem :: rest =>
	     PP.vblock
	       (PP.hcat (PP.text header1, elem) ::
	        map (fn fmt => PP.hcat (PP.text header2, fmt)) rest)

end (* top local *)
end (* structure NewPPUtil *)
