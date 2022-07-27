(* prim-types.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Support for ASDL primitive types.
 *)

structure PrimTypes : sig

  (* the implicit module that defines the primitive types *)
    val primTypesId : AST.ModuleId.t
    val primTypes   : AST.module

  (* primitive type IDs *)
    val boolTyId	: AST.TypeId.t
    val intTyId		: AST.TypeId.t
    val uintTyId	: AST.TypeId.t
    val integerTyId	: AST.TypeId.t
    val identifierTyId	: AST.TypeId.t
    val stringTyId	: AST.TypeId.t

  (* primitive types *)
    val boolTy		: AST.named_ty
    val intTy		: AST.named_ty
    val uintTy		: AST.named_ty
    val integerTy	: AST.named_ty
    val identifierTy	: AST.named_ty
    val stringTy	: AST.named_ty

  (* pseudo types for sum-type tags *)
    val tag8TyId	: AST.TypeId.t		(* tag values in 0..255 *)
    val tagTyId		: AST.TypeId.t		(* tag values in 0.. *)

  (* lookup a primitive type by name *)
    val find : Atom.atom -> AST.named_ty option

  end = struct

    structure TId = AST.TypeId

    val primTypesId = AST.ModuleId.new (Atom.atom "<primitive-types>")

    val boolTyId	= TId.new (Atom.atom "bool")
    val intTyId		= TId.new (Atom.atom "int")
    val uintTyId	= TId.new (Atom.atom "uint")
    val integerTyId	= TId.new (Atom.atom "integer")
    val identifierTyId	= TId.new (Atom.atom "identifier")
    val stringTyId	= TId.new (Atom.atom "string")

    val boolTy		= AST.BaseTy boolTyId
    val intTy		= AST.BaseTy intTyId
    val uintTy		= AST.BaseTy uintTyId
    val integerTy	= AST.BaseTy integerTyId
    val identifierTy	= AST.BaseTy identifierTyId
    val stringTy	= AST.BaseTy stringTyId

  (* pseudo types for sum-type tags.  These are used to access encode/decode
   * functions, but are not actual types either the source or generated code.
   *)
    val tag8TyId	= TId.new (Atom.atom "tag8")
    val tagTyId		= TId.new (Atom.atom "tag")

    val primTypes = let
	  val decls = ref[]
	  val module = AST.Module{
		  isPrim = true,
		  id = primTypesId,
		  decls = decls
		}
	(* make a primitive type declaration and record the type's binding *)
	  fun mkDcl id = let
		val dcl = AST.TyDcl{id = id, def = ref AST.PrimTy, owner = module}
		in
		  AST.TypeId.bind(id, dcl);
		  dcl
		end
	  in
	    decls := [
		mkDcl boolTyId,
		mkDcl intTyId,
		mkDcl uintTyId,
		mkDcl integerTyId,
		mkDcl identifierTyId,
		mkDcl stringTyId,
	      (* declarations for the sum-type tag representations *)
		mkDcl tag8TyId,
		mkDcl tagTyId
	      ];
	    module
	  end

  (* lookup a primitive type by name *)
    val find = let
	  val tbl = AtomTable.mkTable(8, Fail "prim-types")
	  fun ins (ty as AST.BaseTy id) = AtomTable.insert tbl (TId.atomOf id, ty)
	  in
	    List.app ins [
		boolTy,
		intTy,
		uintTy,
		integerTy,
		identifierTy,
		stringTy
	      ];
	    AtomTable.find tbl
	  end

  end
