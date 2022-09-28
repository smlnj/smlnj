(* Basics/print/newpputil.sml *)

structure NewPPUtil =
struct

local
  structure S = Symbol
  structure PP = NewPP
in    

(* fmtSym: S.symbol -> PP.format *)
fun fmtSym sym = PP.text (S.name sym)

(* fmtSymList : S.symbol list -> PP.format *)
fun fmtSymList (syms: S.symbol list) = PP.formatList PPU.fmtSym syms

(* fmtSymPath : SymPath.path -> PP.format *)			 
fun fmtSymPath (path: SymPath.path) = PP.text (SymPath.toString path)

(* fmtInvPath : InvPath.path -> PP.format *)			 
fun fmtInvPath (path: InvPath.path) = PP.text (InvPath.toString path)

fun fmtRegion ((l,u): SourceMap.region) : PP.format =
    PP.cblock [PP.integer l, PP.text "-" PP.integer u]

(* TODO: Vertically align the element formats, taking account of the possibly
 * that the two header strings have different sizes by left-padding the 
 * shorter header. *)

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
