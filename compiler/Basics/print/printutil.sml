(* Basics/print/printutil.sml *)
(* Copyright 2003, the Fellowship of SML/NJ (https://www.smlnj.org) *)

structure PrintUtil : PRINTUTIL =
struct

  (* string "formatting" functions -- translate to "formatted" string *)

  fun trimmed (s, maxsz) =
      if size s <= maxsz then s
      else String.substring (s, 0, maxsz) ^ "#"

  fun quoteString s = concat ["\"", String.toString s, "\""]
  fun formatString s = quoteString (trimmed (s, !Control_Print.stringDepth))
  fun formatIntInf i = trimmed (IntInf.toString i, !Control_Print.intinfDepth)

end (* structure PrintUtil *)

