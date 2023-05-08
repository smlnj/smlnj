(* Basics/print/stringformats.sml *)
(* Copyright 2003, the Fellowship of SML/NJ (https://www.smlnj.org) *)

structure StringFormats : STRING_FORMATS =
struct

  (* functions for "formatting" a string and an IntInf.int *)

  fun trimmed (s, maxsz) =
      if size s <= maxsz then s
      else String.substring (s, 0, maxsz) ^ "#"

  fun quoteString s = concat ["\"", String.toString s, "\""]

  fun formatString s = quoteString (trimmed (s, !PrintControl.stringDepth))

  fun formatIntInf i = trimmed (IntInf.toString i, !PrintControl.intinfDepth)

end (* structure StringFormats *)

