(* util.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Util : sig

  (* flags that control aspects of code generation *)
    type flags = {
      (* suppress view properties *)
	suppress : {types : bool, pickler : bool, unpickler : bool},
      (* true if we are using ASDL's arena allocator *)
	arenaAlloc : bool
      }

  (* The C++ representation of a type *)
    datatype rep_kind = PrimRep | EnumRep | ClassRep

  (* return the representation kind of a type *)
    val repKindOf : AST.TypeId.t -> rep_kind

  (* is a type an enumeration? *)
    val isEnum : AST.TypeId.t -> bool

  (* is a type represented by a pointer (i.e., boxed) or by an immediate value (unboxed)? *)
    val isBoxed : AST.TypeId.t -> bool

  (* default pickler/unpickler names for primitive and enumeration types *)
    val picklerName : AST.TypeId.t -> string
    val unpicklerName : AST.TypeId.t -> string

  (* name of a enum constant *)
    val enumConstrName : AST.ConstrId.t -> string

  (* constructor tags for sum types *)
    val constrTagName : AST.ConstrId.t -> string
    val tagFieldName : string

  (* names of components for tuples and records *)
    val tupleFieldName : int -> string
    val recordFieldName : string -> string
    val fieldName : AST.label -> string
    val fieldGetName : AST.label -> string
    val fieldSetName : AST.label -> string

  (* return the representation type for a type expression *)
    val tyexpToCxx : AST.ty_exp -> Cxx.ty

  (* map a field to a C++ function parameter *)
    val fieldParam : AST.label -> Cxx.exp
    val fieldToParam : AST.field -> Cxx.param

  (* map a type to the C++ representation of optional values of the type.
   * Note that this typed is unqualified.
   *)
    val optionTy : AST.TypeId.t -> Cxx.ty

  (* map a type to the C++ representation of sequnce values of the type
   * Note that this typed is unqualified.
   *)
    val seqTy : AST.TypeId.t -> Cxx.ty

    val qualId : AST.ModuleId.t option * string -> string

    val templateTy : string * AST.ModuleId.t option * AST.TypeId.t -> Cxx.ty

  (* generate code to dynamically allocate an object using the appropriate allocation
   * mechanism.
   *)
    val genNew : flags -> Cxx.ty * Cxx.exp list -> Cxx.exp

  end = struct

    structure V = CxxView
    structure ModV = V.Module
    structure TyV = V.Type
    structure CL = Cxx

    type flags = {
	suppress : {types : bool, pickler : bool, unpickler : bool},
	arenaAlloc : bool
      }

  (* The C++ representation of a type *)
    datatype rep_kind = PrimRep | EnumRep | ClassRep

    fun defOfTyId tyId = (case AST.TypeId.bindingOf tyId
	   of SOME(AST.TyDcl{def, ...}) => !def
	    | NONE => raise Fail(concat["unknown type ID '", AST.TypeId.nameOf tyId, "'"])
	  (* end case *))

  (* return the kind of a type *)
    fun repKindOf tyId = (case defOfTyId tyId
	   of AST.EnumTy _ => EnumRep
	    | AST.AliasTy(AST.Typ(nty, AST.NoTyc)) =>
		repKindOf (AST.idOfNamedTy nty)
	    | AST.AliasTy _ => ClassRep
	    | AST.PrimTy => PrimRep
	    | _ => ClassRep
	  (* end case *))

    fun isEnum tyId = (case defOfTyId tyId
	   of AST.EnumTy _  => true
	    | AST.AliasTy(AST.Typ(nty, AST.NoTyc)) => isEnum (AST.idOfNamedTy nty)
	    | _ => false
	  (* end case *))

    fun isBoxed tyId = (case defOfTyId tyId
	   of AST.EnumTy _ => false
	    | AST.AliasTy(AST.Typ(nty, AST.NoTyc)) => isBoxed (AST.idOfNamedTy nty)
	    | AST.AliasTy _ => false (* options and sequences are not boxed *)
	    | AST.PrimTy => TyV.getBoxed tyId
	    | _ => true
	  (* end case *))

    fun picklerName tyId = "write_" ^ AST.TypeId.nameOf tyId
    fun unpicklerName tyId = "read_" ^ AST.TypeId.nameOf tyId

    fun enumConstrName id = let
	  val SOME(AST.Constr{owner, ...}) = AST.ConstrId.bindingOf id
	  in
	    concat[TyV.getName owner, "::", V.Constr.getName id]
	  end

    fun constrTagName id = "_con_" ^ V.Constr.getName id

    val tagFieldName = "_tag"

    fun tupleFieldName i = "_v" ^ Int.toString i

    fun recordFieldName lab = "_v_" ^ lab

    fun fieldName (AST.Pos i) = tupleFieldName i
      | fieldName (AST.Lab lab) = recordFieldName lab

    fun fieldGetName (AST.Pos i) = "get_" ^ Int.toString i
      | fieldGetName (AST.Lab lab) = "get_" ^ lab

    fun fieldSetName (AST.Pos i) = "set_" ^ Int.toString i
      | fieldSetName (AST.Lab lab) = "set_" ^ lab

    fun tyexpToCxx (AST.Typ(ty, tyc)) = let
	  val (isBoxed, tyName) = (case ty
		 of AST.BaseTy tyId => (TyV.getBoxed tyId, TyV.getName tyId)
		  | AST.ImportTy(modId, tyId) =>
		      (isBoxed tyId, concat[ModV.getName modId, "::", TyV.getName tyId])
		  | AST.LocalTy(AST.TyDcl{id, def, ...}) => (case !def
		       of AST.EnumTy _ => (false, TyV.getName id)
			| AST.AliasTy(AST.Typ(nty, AST.NoTyc)) =>
			    (isBoxed (AST.idOfNamedTy nty), TyV.getName id)
			| AST.AliasTy _ => (false, TyV.getName id)
			| AST.PrimTy => raise Fail "unexpected primitive type"
			| _ => (true, TyV.getName id)
		      (* end case *))
		(* end case *))
	  val cty = CL.T_Named tyName
	  in
	    case (isBoxed, tyc)
	     of (false, AST.NoTyc) => cty
	      | (true, AST.NoTyc) => CL.T_Ptr cty
	      | (false, AST.OptTyc) => CL.T_Template("asdl::option", [cty])
	      | (true, AST.OptTyc) => CL.T_Ptr cty
	      | (false, AST.SeqTyc) => CL.T_Template("asdl::sequence", [cty])
	      | (true, AST.SeqTyc) => CL.T_Template("asdl::sequence", [CL.T_Ptr cty])
	      | (_, AST.SharedTyc) => raise Fail "shared types are not yet supported"
	    (* end case *)
	  end

    fun fieldParamName (AST.Pos i) = "p" ^ Int.toString i
      | fieldParamName (AST.Lab lab) = "p_" ^ lab

    fun fieldParam label = CL.mkVar(fieldParamName label)

    fun fieldToParam {label, ty} = CL.PARAM([], tyexpToCxx ty, fieldParamName label)

  (* map a type to the C++ representation of optional values of the type. *)
    fun optionTy tyId = let
	  val cty = CL.T_Named(TyV.getName tyId)
	  in
(* FIXME: this test is too simple, because it does not handle aliases and
 * primitive types (e.g., int) correctly.
 *)
	    if isBoxed tyId
	      then CL.T_Ptr cty
	      else CL.T_Template("asdl::option", [cty])
	  end

  (* map a type to the C++ representation of sequnce values of the type *)
    fun seqTy tyId = let
	  val cty = CL.T_Named(TyV.getName tyId)
	  in
	    if isBoxed tyId
	      then CL.T_Template("asdl::sequence", [CL.T_Ptr cty])
	      else CL.T_Template("asdl::sequence", [cty])
	  end

    fun qualId (NONE, id) = id
      | qualId (SOME modId, id) = concat[ModV.getName modId, "::", id]

    fun templateTy (tyc, optModId, tyId) =
	  CL.T_Template(tyc, [CL.T_Named(qualId(optModId, TyV.getName tyId))])

    fun genNew (flags : flags) (ty, args) = if #arenaAlloc flags
	  then CL.mkNewAt(
	    CL.mkQTemplateApply([CL.SC_Namespace "asdl", CL.SC_Namespace "alloc"],"alloc", [ty], []),
	    ty, args)
	  else CL.mkNew(ty, args)

  end
