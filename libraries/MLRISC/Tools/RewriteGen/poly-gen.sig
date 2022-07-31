(*
 * This module abstracts out the process of generating a polytypic function,
 * from a set of datatype definitions and transformation rules.
 *)
signature POLY_GEN =
sig
   structure Ast : MDL_AST

   datatype hook =
      HOOK of 
      { name  : string,      (* name of function *)
        factor: bool,        (* factor rules by constructor? *)
        args  : string list, (* function arguments *)
        ret   : string,      (* return argument name *)
                 (* what is the unit transform of this function *)
        unit  : Ast.exp -> Ast.exp, 
                 (* how to generate code for a constructor *)
        gen   : (Ast.ty * Ast.exp -> Ast.exp) * Ast.consbind -> Ast.exp
      }  

   (* translate a set of rules into an declaration *)
   val gen : hook ->              (* hook *)
             (Ast.id -> bool) ->  (* is it a non-terminal type id? *)
                                  (* rules for each datatype *)
             (Ast.datatypebind * Ast.clause list) list ->
             Ast.decl
end
