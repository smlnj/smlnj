local open PrettyPrint
in

val fx = text "xxxx"
val fy = text "yyy"

val fh = hblock [fx, fy]  (* = hcat (fx, fy) *)
val fv = vblock [fx, fy]
val fp = pblock [fx, fy]

end (* local *)
