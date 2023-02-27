(* Basics/errormsg/pperrormsg.sml *)

structure PPErrorMsg =
struct

local (* top local *)
  structure SR = Source
  structure SM = SourceMap
  structure PP = PrettyPrint
in

fun fmtRegion (REGION (lo,hi): SM.region) : PP.format =
    PP.cat [PP.integer lo, PP.text "-", PP.integer hi]

(* add fmtSourceloc, etc. *)

end (* top local *)
end (* structure PPErrorMsg *)
