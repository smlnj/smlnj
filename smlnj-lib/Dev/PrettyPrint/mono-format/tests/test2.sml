local
  structure PP = PrettyPrint
in

val fx = PP.text "xxxx"
val fy = PP.text "yyy"
val fz = PP.text "zzzzz"

val fvxy = PP.vBlock [fx, fy]
val f0 = PP.vBlock [PP.text "begin", PP.indent 2 fvxy, PP.text "end"]

end (* local *)

(* Rendering f0 should produce:

begin
  xxxx
  yyy
end

*)
