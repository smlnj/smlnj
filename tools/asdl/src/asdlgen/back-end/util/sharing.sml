(* sharing.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Sharing : sig

  (* analyse the modules to determine if there are any types that have
   * shared values and, if so, determine those types that will require
   * a generated hashing function.  This function returns true if there
   * was at least one instance of sharing in the specification.
   *)
    val analysis : AST.module list -> bool

  (* returns true if the specified type requires a hashing function to support
   * sharing.
   *)
    val needsHashFn : AST.TypeId.t -> bool

  end = struct

  (* boolean property for tracking types that require hash functions *)
    val {getFn=needsHashFn, setFn} = AST.TypeId.newFlag()

    fun analysis modules = let
	  val hasSharing = ref false
	  fun chkMod (AST.Module{decls, ...}) = List.app chkDcl (!decls)
	  and chkDcl (AST.TyDcl{id, def, ...}) = if AST.TypeId.isSharedArg id
		then (
		  hasSharing := true;
		  setFn (id, true);
		  markTy (!def))
		else ()
	(* process a type definition to transitively mark types as requiring hashing *)
	  and markTy (AST.EnumTy _) = ()
	    | markTy (AST.SumTy{attribs, cons}) = (
		List.app markField attribs;
		List.app markCon cons)
	    | markTy (AST.ProdTy{fields}) = List.app markField fields
	    | markTy (AST.AliasTy ty) = markTyExp ty
	    | markTy AST.PrimTy = ()
	  and markField {label, ty} = markTyExp ty
	  and markCon (AST.Constr{fields, ...}) = List.app markField fields
	  and markTyExp (AST.Typ(AST.BaseTy _, _)) = ()
	    | markTyExp (AST.Typ(AST.ImportTy(_, tyc), _)) = (
		case AST.TypeId.bindingOf tyc
		 of SOME dcl => chkDcl dcl
		  | NONE => raise Fail "unknown imported type"
		(* end case *))
	    | markTyExp (AST.Typ(AST.LocalTy(AST.TyDcl{id, def, ...}), _)) =
		if (needsHashFn id orelse AST.TypeId.isSharedArg id)
		  then ()
		  else (
		    setFn (id, true);
		    markTy (!def))
	  in
	    List.app chkMod modules;
	    !hasSharing
	  end

  end
