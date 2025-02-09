local open PrettyPrint
in

val fx = text "xxxx"
val fy = text "yyy"
val fz = text "zzzzz"

val fvxy = vblock [fx, fy]
val f0 = vblock [text "begin", indent 2 fvxy, text "end"]

end (* local *)

(* Rendering f0 should produce:

begin
  xxxx
  yyy
end

*)
