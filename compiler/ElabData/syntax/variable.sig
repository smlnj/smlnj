(* variable.sig
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature VARIABLE =
  sig

    datatype var
      = VALvar of	                (* ordinary variables *)
	  {path : SymPath.path,
	   typ : Types.ty ref,
	   btvs : Types.tyvar list ref,
	   access : Access.access,
	   prim   : PrimopId.prim_id}
      | OVLDvar of       	        (* overloaded identifier *)
	{name : Symbol.symbol,          (* name of the overloaded operator *)
	 variants : var list}           (* variant variables (VALvars) *)
      | ERRORvar

    val varPath : var -> SymPath.path
    val varName : var -> Symbol.symbol
    val varType : var -> Types.ty
    val varAccess : var -> Access.access
    val varBtvs : var -> Types.tyvar list   (* generalized (polymorphically bound) tyvars *)
    val hasLvarAccess : var -> bool
    val varToLvar : var -> LambdaVar.lvar
    val lvarToVar : LambdaVar.lvar -> var

    val mkVALvar : Symbol.symbol * Access.access -> var
    val newVALvar : Symbol.symbol * Types.ty -> var
    val replaceLvar : var -> var * LambdaVar.lvar

    val eqVar : var * var -> bool

    val wildVar : var

    val isWildVar : var -> bool

    val toString : var -> string

  end (* signature VARCON *)
