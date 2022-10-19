(* ElabData/basics/ppsymbols.sml *)

structure PPSymPaths =  (* abbreviated PPP *)
struct

local
  structure S = Symbol
  structure SP = SymPath
  structure IP = InvPath
  structure PP = NewPP
  structure PPS = PPSymbols
in

(* fmtSymPath : SP.path -> PP.format *)			 
fun fmtSymPath (path: SP.path) = PP.text (SP.toString path)

(* fmtInvPath : IP.path -> PP.format *)			 
fun fmtInvPath (path: IP.path) = PP.text (IP.toString path)

(* fmtTycName : IP.path -> PP.format *)
(* The path should be a tycon path (e.g. name from GENtyc{name,...});
 * returns a tycon symbol *)
val anonTycName = S.tycSymbol "<anonymous tycon>"
fun fmtTycName (IP.IPATH syms: IP.path) =
    PPS.fmtSym (case syms of nil => anonTycName | sym :: _ => sym)

end (* top local *)
end (* structure PPSymPaths *)
