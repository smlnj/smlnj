(* smlnj-lib/PrettyPrint/examples/wadler-trees2.sml *)

fun formatTree' (Node (s, trees)) = 
    case trees
      of nil => text s
       | _ =>
	  vblock [ccat (text s, lbracket),
		  indent 2 (vsequence comma (map formatTree' trees)),
		  rbracket];

