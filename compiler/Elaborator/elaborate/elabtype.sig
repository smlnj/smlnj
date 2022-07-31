(* COPYRIGHT (c) 1998 Bell Laboratories *)
(* elabtype.sig *)

signature ELABTYPE =
sig

  val elabType :
        Ast.ty * StaticEnv.staticEnv * ErrorMsg.errorFn * SourceMap.region
        -> Types.ty * TyvarSet.tyvarset

  val elabTyvList :
        Ast.tyvar list * ErrorMsg.errorFn * SourceMap.region
        -> Types.tyvar list

  val elabTYPEdec :
        Ast.tb list * StaticEnv.staticEnv * InvPath.path
        * SourceMap.region * ElabUtil.compInfo
        -> Absyn.dec * StaticEnv.staticEnv

  val elabDATATYPEdec :
        {datatycs: Ast.db list, withtycs: Ast.tb list} * StaticEnv.staticEnv
        * ExpandTycon.sigContext * EntityEnv.entityEnv
        * (Types.tycon -> bool) * InvPath.path
        * SourceMap.region * ElabUtil.compInfo
        -> Types.tycon list * Types.tycon list * Types.datacon list
           * StaticEnv.staticEnv

  val debugging : bool ref

end (* signature ELABTYPE *)
