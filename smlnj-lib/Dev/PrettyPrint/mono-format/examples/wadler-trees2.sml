(* smlnj-lib/PrettyPrint/examples/wadler-trees2.sml *)

local
structure PP = PrettyPrint
in

fun formatTree' (Node (s, trees)) =
    case trees
      of nil => PP.text s
       | _ => PP.vBlock [
            PP.cBlock [PP.text s, PP.text "["],
            PP.indent 2 (
              PP.sequenceWithMap {
                  align = PP.V, sep = PP.text ",", fmt = formatTree'
                } trees),
            PP.text "]"
          ]

end
