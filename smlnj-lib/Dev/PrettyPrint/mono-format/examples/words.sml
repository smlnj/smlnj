(* smlnj-lib/PrettyPrint/examples/words.sml *)

local

  structure PP = PrettyPrint

in

fun formatPara (s: string) : format =
    let val tokens = String.tokens Char.isSpace s
     in PP.pBlock (List.map PP.text tokens)
    end

end; (* local *)
