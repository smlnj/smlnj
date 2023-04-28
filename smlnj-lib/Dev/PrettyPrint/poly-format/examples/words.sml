(* smlnj-lib/PrettyPrint/examples/words.sml *)

local

  open PrettyPrint

in

fun formatPara (s: string) : format =
    let val tokens = String.tokens Char.isSpace s
     in pblock (map text tokens)
    end

end; (* local *)
