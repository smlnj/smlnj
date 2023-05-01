(* Basics/symbol/ppsymbols.sml *)

structure PPSymbols =
struct

local
  structure S = Symbol
  structure PP = Formatting
in

(* fmtSym: S.symbol -> PP.format *)
fun fmtSym sym = PP.text (S.name sym)

(* fmtSymList : S.symbol list -> PP.format *)
fun fmtSymList (syms: S.symbol list) = PP.list (map fmtSym syms)

end (* top local *)
end (* structure PPSymbols *)
