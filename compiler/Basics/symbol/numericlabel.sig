(* Basics/symbol/numericlabel.sig *)
(* Copyright 2022 The Fellowship of SML/NJ *)

signature NUMERIC_LABEL =
sig

  type numericLabel = Symbol.symbol

  val numericLabel : int -> numericLabel

  val numericLabelIsInt : numericLabel * int -> bool				

  val checkTupleLabels : Symbol.symbol list -> bool

end (* signature NUMERIC_LABEL *)
