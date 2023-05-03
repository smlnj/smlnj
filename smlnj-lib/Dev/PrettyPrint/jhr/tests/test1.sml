local
  structure PP = PrettyPrint
in

val fx = PP.text "xxxx"
val fy = PP.text "yyy"

val fh = PP.hBlock [fx, fy]  (* = hcat (fx, fy) *)
val fv = PP.vBlock [fx, fy]
val fp = PP.pBlock [fx, fy]

end (* local *)
