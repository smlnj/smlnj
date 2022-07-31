signature REWRITE_GEN = 
sig
   structure Ast   : MDL_AST (* abstract syntax *)

       (* translate rewriting and pattern matching code *)
   val gen : string -> string
   val main : string * string list -> int

end
