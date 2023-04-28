(* test5.sml *)

(* producing a succession of lines with varying indentation *)

local
  open PrettyPrint
in

val body = [indent 2 (text "aaaa"), indent 4 (text "bb"), indent 3 (text "ccc")]

val fmtv = vblock body
val fmth = hblock body
val fmtp = pblock body

val body3 = [text "aaaa", text "bb", text "ccc"]
val body4 = [text "aaaa", text "bb", text "ccc", text "dddd"]
val body5 = [text "aaaa", text "bb", text "ccc", text "dddd", text "eeeee"]

val fmtp3 = pblock body3
val fmtp4 = pblock body4
val fmtp5 = pblock body5

val fmth3 = hblock body3
val fmth4 = hblock body4
val fmth5 = hblock body5

val fmtv3 = vblock body3
val fmtv4 = vblock body4
val fmtv5 = vblock body5

val fmthv5 = hvblock body5

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

- test fmtv 80;  (* all lines of fmtv indented - at top level! *)
  aaaa
    bb
   ccc

- test (ccat (text "xxx", fmtv)) 80;
xxxaaaa
       bb
      ccc

(* 1st line looses its indentatioin and is shifted left 2 spaced to abut xxx,
 * but 2nd and 3rd line are not shifted, because blm of fmtv is still column 3,
 * the column in which the first character of "aaaa" is printed. *)
*)
