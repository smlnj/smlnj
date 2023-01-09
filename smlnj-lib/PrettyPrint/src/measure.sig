(* smlnj-lib/PrettyPrint/src/measure.sig *)

signature MEASURE =
sig

  val measure : Format.format -> int

  val measureElements : Format.element list -> int

  val measureFormats : int * Format.format list -> int

end  (* signature MEASURE *)
