(* Basics/source/ppsourcemap.sml *)

structure PPSourceMap =
struct

local (* top local *)
  structure SR = Source
  structure SM = SourceMap
  structure PP = Formatting
in

(* fmtRegion : SM.region -> PP.format *)
fun fmtRegion (SM.REGION (lo,hi): SM.region) : PP.format =
    PP.cblock [PP.integer lo, PP.text "-", PP.integer hi]
  | fmtRegion NULLregion = PP.text "<<>>"

(* fmtLocation : SM.location -> PP.format *)
fun fmtLocation ({line, column}: SM.location) = 
    PP.cblock [PP.integer line, PP.period, PP.integer column]

fun fmtSourceRegion ({fileOpened, sourceMap,...}: SR.source, region: SM.region) =
    (case SM.regionToLocations (!sourceMap, region)
      of SOME (start, finish) =>
           PP.cblock [PP.text fileOpened, PP.colon, fmtLocation start, PP.text "-", fmtLocation finish]
      | NONE => PP.cblock [PP.text fileOpened, PP.colon, PP.text "<<>>"])

end (* top local *)
end (* structure PPSourceMap *)
