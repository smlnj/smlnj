(* ElabData/basics/ppsymbols.sml *)

structure PPSymPaths =
struct

local
  structure SP = SymPath
  structure IP = InvPath
  structure PP = NewPP
in

(* fmtSymPath : SP.path -> PP.format *)			 
fun fmtSymPath (path: SP.path) = PP.text (SP.toString path)

(* fmtInvPath : IP.path -> PP.format *)			 
fun fmtInvPath (path: IP.path) = PP.text (IP.toString path)

end (* top local *)
end (* structure PPSymPaths *)
