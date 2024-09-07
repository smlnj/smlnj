(* smlnj-lib/PrettyPrint/examples/wadler-trees2.sml *)

local
  structure F = Formatting
in

fun formatTree' (Node (s, trees)) =
    case trees
      of nil => F.text s
       | _ => F.vBlock [
            F.cBlock [F.text s, F.text "["],
            F.indent 2 (
              F.sequenceWithMap {
                  align = F.V, sep = F.text ",", fmt = formatTree'
                } trees),
            F.text "]"
          ]

end (* local *)
