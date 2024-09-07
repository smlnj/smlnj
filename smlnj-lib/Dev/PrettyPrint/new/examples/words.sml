(* smlnj-lib/PrettyPrint/examples/words.sml *)

local
  structure F = Formatting
in

fun formatPara (s: string) : F.format =
    let val tokens = String.tokens Char.isSpace s
     in F.pBlock (List.map F.text tokens)
    end

end; (* local *)
