(* ~/sml/Dev/pp/new/new7/measure.sig *)

signature MEASURE =
sig

  val measure : Format.format -> int
				
  val measureElements : Format.element list -> int

  val measureFormats : Format.format list -> int

end  (* signature MEASURE *)
