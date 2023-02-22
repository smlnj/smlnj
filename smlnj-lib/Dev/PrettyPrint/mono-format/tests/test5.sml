(* test5.sml *)

(* producing a succession of lines with varying PP.indentation *)

local
  structure PP = PrettyPrint
in

val body = [PP.indent 2 (PP.text "aaaa"), PP.indent 4 (PP.text "bb"), PP.indent 3 (PP.text "ccc")]

val fmtv = PP.vBlock body
val fmth = PP.hBlock body
val fmtp = PP.pBlock body

val body3 = [PP.text "aaaa", PP.text "bb", PP.text "ccc"]
val body4 = [PP.text "aaaa", PP.text "bb", PP.text "ccc", PP.text "dddd"]
val body5 = [PP.text "aaaa", PP.text "bb", PP.text "ccc", PP.text "dddd", PP.text "eeeee"]

val fmtp3 = PP.pBlock body3
val fmtp4 = PP.pBlock body4
val fmtp5 = PP.pBlock body5

val fmth3 = PP.hBlock body3
val fmth4 = PP.hBlock body4
val fmth5 = PP.hBlock body5

val fmtv3 = PP.vBlock body3
val fmtv4 = PP.vBlock body4
val fmtv5 = PP.vBlock body5

val fmthv5 = PP.hvBlock body5

fun test fmt n = printFormatLW n fmt

val p3 = test fmtp3
val p4 = test fmtp4
val p5 = test fmtp5

val h3 = test fmth3
val h4 = test fmth4
val h5 = test fmth5

val v3 = test fmtv3
val v4 = test fmtv4
val v5 = test fmtv5

val hv5 = test fmthv5

end; (* local *)

(* outputs:

- test fmtv 80;  (* all lines of fmtv PP.indented - at top level! *)
  aaaa
    bb
   ccc

- test (ccat (PP.text "xxx", fmtv)) 80;
xxxaaaa
       bb
      ccc

(* 1st line looses its PP.indentatioin and is shifted left 2 spaced to abut xxx,
 * but 2nd and 3rd line are not shifted, because blm of fmtv is still column 3,
 * the column in which the first character of "aaaa" is printed. *)
*)
