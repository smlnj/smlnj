(*
 * Some simple utilities on transforming RTLs
 *)
signature MDL_RTL_TOOLS =
sig
   structure RTL : MLTREE_RTL 
   structure Ast : MDL_AST

   (* Simplify an RTL *)
   val simplify : RTL.rtl -> RTL.rtl
     
   (* Translate an rtl into an expression *)
   val rtlToExp : RTL.rtl -> Ast.exp

   (* Translate an rtl into a pattern *)
   val rtlToPat : RTL.rtl -> Ast.pat

   (* Translate an rtl into an rtl construction function  *)
   val rtlToFun : Ast.id * Ast.id list * RTL.rtl -> Ast.decl

   (* create code to generate a new operator *)
   val createNewOp : RTL.T.Basis.misc_op -> Ast.decl

end
