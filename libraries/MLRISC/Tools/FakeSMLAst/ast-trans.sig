(*
 * Translation from one sort to another
 *)
signature MDL_AST_TRANSLATION =
sig

   structure Ast : MDL_AST

   type 'a map = {origName : Ast.id,  (* original name *)
                  newName  : Ast.id,  (* new name (for duplicates) *)
                  ty       : Ast.ty   (* the type associated with it *)
                 } -> 'a

   type 'a folder = {origName : Ast.id,
                     newName  : Ast.id,
                     ty       : Ast.ty} * 'a -> 'a


   (* Simplify an expression, declaration etc. *)
   val simplifyExp  : Ast.exp  -> Ast.exp
   val simplifyDecl : Ast.decl -> Ast.decl
   val simplifyPat  : Ast.pat -> Ast.pat
   val simplifyTy   : Ast.ty -> Ast.ty
   val simplifySexp : Ast.structexp -> Ast.structexp

   (* Strip away all location markings *)
   val stripMarks   : Ast.decl -> Ast.decl

   (* Translate a type to a pattern.  Apply the map function on each binding
    *)
   val mapTyToPat   : Ast.pat map -> Ast.ty -> Ast.pat

   (* Translate a type to an expression.  Apply the map function on 
    * each identifier.
    *)
   val mapTyToExp   : Ast.exp map -> Ast.ty -> Ast.exp

   (* Fold functions that does similar things as the ones above, i.e.,
    * it enumerates all the bindings and their types.
    *)
   val foldTy   : 'a folder -> 'a -> Ast.ty -> 'a
   val foldCons : 'a folder -> 'a -> Ast.consbind -> 'a

   (* Translate a constructor to a pattern *)
   val mapConsToPat : {prefix : Ast.id list,  (* path prefix *)
                       id     : Ast.pat map   (* how to map identifiers *)
                      } -> Ast.consbind -> Ast.pat

   (* Translate a constructor to an expression representing its arguments *)
   val mapConsArgToExp : Ast.exp map -> (* how to map identifiers *)
                         Ast.consbind -> Ast.exp

   (* Translate a constructor to a constructor expression *)
   val mapConsToExp : {prefix : Ast.id list,  (* path prefix *)
                       id     : Ast.exp map   (* how to map identifiers *)
                      } -> Ast.consbind -> Ast.exp

   (* Translate a constructor to a clause *)
   val mapConsToClause : {prefix : Ast.id list,        (* path prefix *)
                          pat    : Ast.pat -> Ast.pat,
                          exp    : Ast.exp  
                         } -> Ast.consbind -> Ast.clause

   (* Given a constructor, return a function that looks up the pattern
    * variables and their types
    *)  
   val consBindings : Ast.consbind -> (Ast.id -> Ast.exp * Ast.ty)

end
