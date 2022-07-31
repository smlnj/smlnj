(* Copyright (c) 1998 by Lucent Technologies *)

local 
  type 'a type_util      = Tables.tidtab -> Ast.ctype               -> 'a 
  type 'a type_mem_util  = Tables.tidtab -> Ast.ctype * Ast.member  -> 'a 
  type 'a type_type_util = Tables.tidtab -> Ast.ctype * Ast.ctype -> 'a 
in

signature TYPE_UTIL =
sig

  exception TypeError of Ast.ctype

  val hasKnownStorageSize : bool type_util
  (* check if type has known storage size *)

(*
  val fixArrayType : Ast.tidtab -> {n:Int32.int, ty:Ast.ctype} -> {err:bool, ty:Ast.ctype}
  (* fix up array type using initializer info e.g. int x[] = {1,2,3}; *)
*)

  val getCoreType : Ast.ctype type_util

  val isPointer : bool type_util
  (* check if a type can be considered to be a pointer type *)

  val isConst : bool type_util
  (* check if a type contains the const qualifier *)

  val isNumberOrPointer : bool type_util
  (* check if a type can be considered to be a number or pointer type *)

  val isNumber : bool type_util
  (* check if a type can be considered to be a number type *)

  val isArray : bool type_util
  (* check if a type can be considered to be an array *)

  val isIntegral : bool type_util
  (* check if a type can be considered to be an array *)

  val deref : (Ast.ctype option) type_util
  (* if type can be considered a pointer then returns dereferenced type;
   * and otherwise returns NONE.
   *)

  val checkQuals : {redundantConst:bool, redundantVolatile:bool} type_util
  (* check for redundant qualifiers *)

  val getQuals : {const:bool, volatile:bool, ty:Ast.ctype} type_util
  (* check for redundant qualifiers *)

  val isFunction : bool type_util
  (* check if a type can be considered to be a function type *)

  val isFunctionPrototype : bool type_util
  (* check if a type is a function prototype *)

  val isNonPointerFunction : bool type_util
  (* check if a type is a function (but not a function pointer) *)

  val getFunction : ((Ast.ctype * (Ast.ctype * Ast.id option) list) option) type_util
  (* if type can be considered a function then returns return type and
   * list of argument types;
   * and otherwise returns NONE.
   *)

  val isStructOrUnion : (Ast.tid option) type_util
  (* if type is a struct or union then returns tid of that type,
   * and otherwise returns NONE.
   *)

  val isEnum : bool type_mem_util
  (* true iff type can be considered an enumerated type and pid is a
   * member of that enum
   *)

  val lookupEnum : (LargeInt.int option) type_mem_util
  (* if type can be considered an enumerated type and id is a member of
   * that enum, return the value of that member; 
   * otherwise raise a type error
   *)

  val isAssignable : Tables.tidtab
                     -> {lhs:Ast.ctype, rhs:Ast.ctype, rhsExpr0:bool}
                     -> bool
  (* type checking: expr of type rhs can be assigned to lval of type lhs *)

  val isEquable : Tables.tidtab
                  -> {ty1:Ast.ctype, exp1Zero:bool,
		      ty2:Ast.ctype, exp2Zero:bool}
                  -> Ast.ctype option

  val conditionalExp : Tables.tidtab
                       -> {ty1:Ast.ctype, exp1Zero:bool,
			   ty2:Ast.ctype, exp2Zero:bool}
                       -> Ast.ctype option

  val isComparable: Tables.tidtab
                    -> {ty1:Ast.ctype, ty2:Ast.ctype}
                    -> Ast.ctype option

  val isAddable: Tables.tidtab
                 -> {ty1:Ast.ctype, ty2:Ast.ctype}
                 -> {ty1:Ast.ctype, ty2:Ast.ctype, resTy:Ast.ctype} option

  val isSubtractable: Tables.tidtab
                      -> {ty1:Ast.ctype, ty2:Ast.ctype}
                      -> {ty1:Ast.ctype, ty2:Ast.ctype, resTy:Ast.ctype} option

  val checkFn : Tables.tidtab
                -> Ast.ctype * Ast.ctype list * bool list
                -> Ast.ctype * string list (* for type error messages *) 
                   * Ast.ctype list
  (* type checking: function applied to args is well formed *)

  val equalType : bool type_type_util
  (* type equality *)

  val compatible : bool type_type_util
  (* type compatibility *)

  val functionArgConv : Ast.ctype type_util

  val composite : Tables.tidtab
                  -> Ast.ctype * Ast.ctype
                  -> Ast.ctype option * string list (* for type error messages *)

  val isScalar : bool type_util
  (* is type numeric *)

  val usualBinaryCnv : (Ast.ctype option) type_type_util
  (* combine binary operation types *)

  val usualUnaryCnv : Ast.ctype type_util
  (* process unary operation type *)

  val preArgConv : Ast.ctype type_util
  (* converts arrays and functions to pointers *)

  val cnvFunctionToPointer2Function : Ast.ctype type_util
  (* converts functions to pointers *)

  val stdInt : Ast.ctype

end (* signature TYPE_UTIL *)

end (* local *)
