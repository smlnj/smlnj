(* Basics/errormsg/pperrormsg.sml *)

structure PPErrorMsg =
struct

local (* top local *)
  structure SM = SourceMap
  structure PP = NewPP
in

fun fmtRegion ((lo,hi): SM.region) : PP.format =
    PP.cblock [PP.integer lo, PP.text "-", PP.integer hi]

(* add fmtSourceloc, etc. *)

end (* top local *)
end (* structure PPErrorMsg *)
