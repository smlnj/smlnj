(* gen-pickle.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Generate the implementation for an ASDL module.  The implementation includes
 * the destructor functions for boxed types and the encoder/decoder functions.
 *
 * For a  boxed ASDL type "TYPE", the encoder is a method with the following
 * signature:
 *
 *	void write (asdl::outstream &os);
 *
 * and the decoder is a static class method with the following signature:
 *
 *	TYPE *read (asdl::instream &is);
 *
 * For enumeration and alias types, we generate the functions:
 *
 *	void write_TYPE (asdl::outstream &os, TYPE v);
 *	TYPE read_TYPE (asdl::instream &is);
 *
 * TODO:
 * 	write/decoder names should be taken from view
 *	proper implementation of destructor functions
 *)

structure GenPickle : sig

  (* generate the implementation of the ASDL module.  The result will be
   * a namespace declaration enclosing the function/method definitions.
   *)
    val gen : Util.flags -> AST.module -> Cxx.decl

  end = struct

    structure PT = PrimTypes
    structure V = CxxView
    structure ModV = V.Module
    structure TyV = V.Type
    structure ConV = V.Constr
    structure E = Encoding
    structure CL = Cxx
    structure U = Util

    type suppress = {types : bool, pickler : bool, unpickler : bool}

    val osParam = CL.param(CL.T_Ref(CL.T_Named "asdl::outstream"), "os")
    val osArg = CL.mkVar "os"
    val isParam = CL.param(CL.T_Ref(CL.T_Named "asdl::instream"), "is")
    val isArg = CL.mkVar "is"
    val argArg = CL.mkVar "arg"

  (* pickler function name for primitive types *)
    fun baseWriter (optModId, tyId) = U.qualId (optModId, U.picklerName tyId)

  (* unpickler function name for primitive types *)
    fun baseReader (optModId, tyId) = U.qualId (optModId, U.unpicklerName tyId)

    fun dispatch (obj, meth, args) =
	  CL.mkExpStm(CL.mkIndirectDispatch (obj, meth, args))

  (* we use the generated `make` method for object allocation; it encapsulates the
   * allocator that is being used.
   *)
    fun makeObj (ty, args) = CL.mkQApply([CL.SC_Type ty], "make", args)

  (* invoke the pickler operation on the argument *)
    fun write (optModId, tyId, arg) = (case U.repKindOf tyId
	   of U.PrimRep => CL.mkCall (baseWriter(optModId, tyId), [osArg, arg])
	    | U.EnumRep => CL.mkCall (baseWriter(optModId, tyId), [osArg, arg])
	    | U.ClassRep => dispatch (arg, "write", [osArg])
	  (* end case *))

    fun writeTyExp (optModId, tyId, suffix, arg) = (case U.repKindOf tyId
	   of U.PrimRep => CL.mkCall (baseWriter(optModId, tyId) ^ suffix, [osArg, arg])
	    | U.EnumRep => CL.mkCall (
		U.qualId(optModId, concat["write_", TyV.getName tyId, suffix]),
		[osArg, arg])
	    | U.ClassRep => CL.mkCall (
		U.qualId(optModId, concat["write_", TyV.getName tyId, suffix]),
		[osArg, arg])
	  (* end case *))

  (* apply the unpickler operation to the input stream *)
    fun read (optModId, tyId) = (case U.repKindOf tyId
	   of U.PrimRep => CL.mkApply (baseReader(optModId, tyId), [isArg])
	    | U.EnumRep => CL.mkApply (baseReader(optModId, tyId), [isArg])
	    | U.ClassRep => CL.mkApply (
		U.qualId (optModId, concat[TyV.getName tyId, "::read"]),
		[isArg])
	  (* end case *))

    fun readTyExp (optModId, tyId, suffix, arg) = (case U.repKindOf tyId
	   of U.PrimRep => CL.mkApply (baseReader(optModId, tyId) ^ suffix, [isArg])
	    | U.EnumRep => CL.mkApply (
		U.qualId(optModId, concat["read_", TyV.getName tyId, suffix]),
		[isArg])
	    | U.ClassRep => CL.mkApply (
		U.qualId(optModId, concat["read_", TyV.getName tyId, suffix]),
		[isArg])
	  (* end case *))

  (* return a list of delete statements for boxed fields of an object *)
    fun deleteStms obj = let
	  fun field label = CL.mkIndirect(CL.mkVar "this", U.fieldName label)
	  fun delete (label, E.OPTION(_, tyId), stms) =
		if U.isBoxed tyId
		  then CL.mkIfThen(
		    CL.mkBinOp(field label, CL.#!=, CL.mkVar "nullptr"),
		    CL.mkDelete(field label)) :: stms
		  else stms
	    | delete (_, E.SEQUENCE _, stms) = stms (* std::vector is unboxed *)
	    | delete (_, E.SHARED _, _) = raise Fail "shared types not supported"
	    | delete (label, E.BASE(_, tyId), stms) =
		if U.isBoxed tyId
		  then CL.mkDelete(field label) :: stms
		  else stms
	  in
	    case obj
	     of E.TUPLE fields => List.foldr
		  (fn ((i, ty), stms) => delete(AST.Pos i, ty, stms))
		    [] fields
	      | E.RECORD fields => List.foldr
		  (fn ((l, ty), stms) => delete(AST.Lab l, ty, stms))
		    [] fields
	    (* end case *)
	  end

    fun gen (flgs : Util.flags) (AST.Module{isPrim=false, id, decls}) = let
	  val flgs' = ModV.getSuppress id
	  val suppressTypes = #types flgs' (* NOTE: `#types flgs` must be false *)
	  val flgs = {
		  suppress = {
		      types = suppressTypes,
		      pickler = #pickler(#suppress flgs) orelse #pickler flgs',
		      unpickler = #unpickler(#suppress flgs) orelse #unpickler flgs'
		    },
		  arenaAlloc = #arenaAlloc flgs
		}
	  val namespace = ModV.getName id
	  in
	    if not suppressTypes
	      then CL.D_Namespace(namespace, List.foldr (genType flgs) [] (!decls))
	      else CL.D_Comment["types suppressed for " ^ namespace]
	  end
      | gen _ _ = raise Fail "GenTypes.gen: unexpected primitive module"

    and genType (flgs : U.flags) (tyDcl as AST.TyDcl{def, ...}, dcls) = let
	  val (id, encoding) = E.encoding tyDcl
	  val name = TyV.getName id
	(* if required, add additional functions for pickling/unpickling
	 * options, sequences, and shared values of this type
	 *)
(* TODO: shared types *)
	  val dcls = if AST.TypeId.isSeqArg id
		then genSeqFns flgs (name, id, encoding, dcls)
		else dcls
	  val dcls = if AST.TypeId.isOptionArg id
		then genOptionFns flgs (name, id, encoding, dcls)
		else dcls
	  in
	    case encoding
	     of E.UNIT conId => let
		(* a single-constructor enum requires no storage in the pickle *)
		  val ty = CL.T_Named name
		  val pickler = if (#pickler(#suppress flgs))
			then CL.D_Comment["pickler suppressed for "^name]
			else CL.mkFuncDcl (
			  CL.voidTy, U.picklerName id,
			  [osParam, CL.param(ty, "v")],
			  CL.mkBlock[])
		  val unpickler = if (#unpickler(#suppress flgs))
			then CL.D_Comment["unpickler suppressed for "^name]
			else CL.mkFuncDcl (
			  ty, U.unpicklerName id,
			  [isParam],
			  CL.mkReturn(CL.mkVar(U.enumConstrName conId)))
		  in
		    pickler :: unpickler :: dcls
		  end
	      | E.ENUM(nCons, cons) => let
		  val tagTyId = E.tagTyId nCons
		  val ty = CL.T_Named name
		  val pickler = if (#pickler(#suppress flgs))
			then CL.D_Comment["pickler suppressed for "^name]
			else CL.mkFuncDcl (
			  CL.voidTy, U.picklerName id,
			  [osParam, CL.param(ty, "v")],
			  write (
			    SOME PT.primTypesId,
			    tagTyId,
			    CL.mkStaticCast(CL.T_Named(TyV.getName tagTyId), CL.mkVar "v")))
		  val unpickler = if (#unpickler(#suppress flgs))
			then CL.D_Comment["unpickler suppressed for "^name]
			else CL.mkFuncDcl (
			  ty, U.unpicklerName id,
			  [isParam],
			  CL.mkReturn(CL.mkStaticCast(ty, read (SOME PT.primTypesId, tagTyId))))
		  in
		    pickler :: unpickler :: dcls
		  end
	      | E.WRAP(_, obj) => genProdMeths flgs (id, name, obj) @ dcls
	      | E.SWITCH(attribs, nCons, cons) =>
		  genSumMeths flgs (id, name, attribs, nCons, cons) @
		  genConsMeths flgs (attribs, nCons, cons) @ dcls
	      | E.OBJ obj => genProdMeths flgs (id, name, obj) @ dcls
	      | E.ALIAS ty => let
		  val cTy = CL.T_Named name
		  val vv = CL.mkVar "v"
		  val pickler = if (#pickler(#suppress flgs))
			then CL.D_Comment["pickler suppressed for "^name]
			else CL.mkFuncDcl (
			  CL.voidTy, U.picklerName id,
			  [osParam, CL.param(cTy, "v")],
			  encodeTy(vv, ty))
		  val unpickler = if (#unpickler(#suppress flgs))
			then CL.D_Comment["unpickler suppressed for "^name]
			else CL.mkFuncDcl (
			  cTy, U.unpicklerName id,
			  [isParam],
			  CL.mkBlock[decodeTy ("v", ty), CL.mkReturn vv])
		  in
		    pickler :: unpickler :: dcls
		  end
	    (* end case *)
	  end

  (* generate functions for optional types *)
    and genOptionFns (flags : U.flags) (tyName, tyId, enc, dcls) = let
	  val fnSuffix = tyName ^ "_option"
	  val optCTy = U.optionTy tyId
	  fun mkPickler mkBody = if (#pickler(#suppress flags))
		then CL.D_Comment[fnSuffix ^ " pickler suppressed"]
		else CL.mkFuncDcl(
		  CL.voidTy, "write_" ^ fnSuffix,
		  [osParam, CL.param(CL.T_Ref optCTy, "arg")],
		  mkBody ())
	  fun mkUnpickler mkBody = if (#unpickler(#suppress flags))
		then CL.D_Comment[fnSuffix ^ " pickler suppressed"]
		else CL.mkFuncDcl(optCTy, "read_" ^ fnSuffix, [isParam], mkBody ())
	(* make the pickler/unpickler for a unit type *)
	  fun mkUnitFns () = raise Fail "Unimplemented"
	(* make the pickler/unpickler for an enumerations with ncons constructors *)
	  fun mkEnumFns ncons = let
		val tagTy = AST.TypeId.nameOf(E.tagTyId ncons)
		val pickler = mkPickler (fn () =>
		      CL.mkCall("asdl::write_" ^ tagTy, [
			  osArg,
			  CL.mkStaticCast(CL.uintTy, CL.mkDispatch(argArg, "valOf", []))
			]))
		val unpickler = mkUnpickler (fn () => CL.mkBlock[
			CL.mkDeclInit(CL.autoTy, "optV",
			  CL.mkApply("asdl::read_" ^ tagTy, [isArg])),
			CL.mkIfThenElse(
			  CL.mkBinOp(CL.E_Var "optV", CL.#==, CL.mkInt 0),
			  CL.mkReturn(CL.mkCons(optCTy, [])),
			  CL.mkReturn(CL.mkCons(optCTy, [
			      CL.mkStaticCast(CL.T_Named tyName, CL.E_Var "optV")
			    ])))
		      ])
		in
		  (pickler, unpickler)
		end
	(* make the pickler/unpickler for types that are represented by
	 * pointers (and thus the empty option is nullptr).
	 *)
	  fun mkBoxedFns () = let
		val pickler = mkPickler (fn () =>
		      CL.mkCall("asdl::write_option", [osArg, argArg]))
		val unpickler = mkUnpickler (fn () =>
		      CL.mkReturn(CL.mkTemplateApply(
			"asdl::read_option", [CL.T_Named tyName],
			[isArg])))
		in
		  (pickler, unpickler)
		end
	  val (pickler, unpickler) = (case enc
		 of E.UNIT _ => mkUnitFns ()
		  | E.ENUM(ncons, _) => mkEnumFns ncons
		  | E.WRAP _ => mkBoxedFns ()
		  | E.SWITCH _ => mkBoxedFns ()
		  | E.OBJ _ => mkBoxedFns ()
		  | E.ALIAS _ => raise Fail "FIXME"
		(* end case *))
	  in
	    pickler :: unpickler :: dcls
	  end

  (* generate functions for sequence types *)
    and genSeqFns (flags : Util.flags) (tyName, tyId, enc, dcls) = let
	  val fnSuffix = tyName ^ "_seq"
	  val optCTy = U.seqTy tyId
	  fun mkPickler mkBody = if #pickler(#suppress flags)
		then CL.D_Comment[fnSuffix ^ " pickler suppressed"]
		else CL.mkFuncDcl(
		  CL.voidTy, "write_" ^ fnSuffix,
		  [osParam, CL.param(CL.T_Ref optCTy, "arg")],
		  mkBody ())
	  fun mkUnpickler mkBody = if #unpickler(#suppress flags)
		then CL.D_Comment[fnSuffix ^ " pickler suppressed"]
		else CL.mkFuncDcl(optCTy, "read_" ^ fnSuffix, [isParam], mkBody ())
	(* make the pickler/unpickler for a unit type *)
	  fun mkUnitFns () = raise Fail "Unimplemented"
	(* make the pickler/unpickler for an enumerations with ncons constructors *)
	  fun mkEnumFns ncons = let
		val szSuffix = if E.smallTag ncons
		      then "_small_enum_seq"
		      else "_big_enum_seq"
		val pickler = mkPickler (fn () =>
		      CL.mkCall("asdl::write" ^ szSuffix, [osArg, argArg]))
		val unpickler = mkUnpickler (fn () =>
		      CL.mkReturn(CL.mkTemplateApply(
			"asdl::read" ^ szSuffix, [CL.T_Named tyName],
			[isArg])))
		in
		  (pickler, unpickler)
		end
	(* make the pickler/unpickler for types that are represented by
	 * pointers (and thus the empty option is nullptr).
	 *)
	  fun mkBoxedFns () = let
		val pickler = mkPickler (fn () =>
		      CL.mkCall("asdl::write_seq", [osArg, argArg]))
		val unpickler = mkUnpickler (fn () =>
		      CL.mkReturn(CL.mkTemplateApply(
			"asdl::read_seq", [CL.T_Named tyName],
			[isArg])))
		in
		  (pickler, unpickler)
		end
	  val (pickler, unpickler) = (case enc
		 of E.UNIT _ => mkUnitFns ()
		  | E.ENUM(ncons, _) => mkEnumFns ncons
		  | E.WRAP _ => mkBoxedFns ()
		  | E.SWITCH _ => mkBoxedFns ()
		  | E.OBJ _ => mkBoxedFns ()
		  | E.ALIAS _ => raise Fail "FIXME"
		(* end case *))
	  in
	    pickler :: unpickler :: dcls
	  end


  (* generate the unpickling an destructor functions for a sum type *)
    and genSumMeths flgs (tyId, name, optAttribs, nCons, cons) = let
	  fun mkUnpickler () = let
		val ty = CL.T_Named name
		val ptrTy = CL.T_Ptr ty
		val tagTy = CL.T_Named "_tag_t"
	      (* read common attribute fields *)
		val (getAttribs, attribArgs) = (case optAttribs
		       of NONE => ([], [])
			| SOME obj => decodeFields obj
		      (* end case *))
	      (* unpickle a constructor *)
		fun doCase (_, conId, fields) = let
		      val label = [U.constrTagName conId]
		      val (getFields, args) = (case fields
			     of NONE => ([], [])
			      | SOME obj => decodeFields obj
			    (* end case *))
		      val newExp = makeObj(CL.T_Named(ConV.getName conId), attribArgs @ args)
		      in
			(label, [CL.mkBlock(getFields @ [CL.mkReturn newExp])])
		      end
	      (* unpickler body *)
		val body = [CL.mkSwitch(CL.mkVar "tag", List.map doCase cons)]
		val body = getAttribs @ body
		val body = (* get tag *)
		      CL.mkDeclInit(tagTy, "tag",
			CL.mkStaticCast(tagTy, read (SOME PT.primTypesId, E.tagTyId nCons)))
		      :: body
		in
		  CL.D_Func(
		    [], ptrTy, [CL.SC_Type ty], "read", [isParam],
		    SOME(CL.mkBlock body))
		end
	(* destructor *)
	  val destr = CL.mkDestrDcl(name,
		case optAttribs
		 of NONE => CL.mkBlock[]
		  | SOME obj => CL.mkBlock(deleteStms obj))
	  in
	    if #unpickler(#suppress flgs)
	      then [destr]
	      else [mkUnpickler(), destr]
	  end

  (* generate the pickler and destructor methods for a sum-type constructor *)
    and genConsMeths flgs (attribs, ncons, cons) = let
	  val tagTyId = E.tagTyId ncons
	  fun doCons ((tag, conId, optArg), dcls) = let
		val name = ConV.getName conId
		val payload = E.prefixWithAttribs(attribs, optArg)
	      (* destructor *)
		val destr = CL.mkDestrDcl(name,
		      case payload
		       of NONE => CL.mkBlock[]
			| SOME obj => CL.mkBlock(deleteStms obj))
		val dcls = destr :: dcls
	      (* pickler *)
		fun mkPickler () = let
		      val body = (case payload
			     of SOME obj => encodeFields obj
			      | NONE => []
			    (* end case *))
		      val body = write(
			      SOME PT.primTypesId, tagTyId,
			      CL.mkVar(U.constrTagName conId)
			    ) :: body
		      in
			CL.D_Func(
			  [], CL.voidTy, [CL.SC_Type(CL.T_Named name)],
(* FIXME: the method name could be set in the view! *)
			  "write", [osParam],
			  SOME(CL.mkBlock body))
		      end
		in
		  if #pickler(#suppress flgs) then dcls else mkPickler() :: dcls
		end
	  in
	    List.foldr doCons [] cons
	  end

  (* generate the methods for a product type *)
    and genProdMeths flgs (id, name, obj) = let
	  val ty = CL.T_Named name
	  val ptrTy = CL.T_Ptr ty
	(* pickler *)
	  fun mkPickler () =
		CL.mkMethDcl(
		  name, CL.voidTy, "write", [osParam],
		  CL.mkBlock(encodeFields obj))
	(* unpickler *)
	  fun mkUnpickler () = let
		val (getFields, args) = decodeFields obj
		val newExp = makeObj(ty, args)
		val body = getFields @ [CL.mkReturn newExp]
		in
		  CL.D_Func (
		    [], ptrTy, [CL.SC_Type ty], "read", [isParam],
		    SOME(CL.mkBlock body))
		end
	(* destructor *)
	  val destr = CL.mkDestrDcl(name, CL.mkBlock(deleteStms obj))
	  val dcls = [destr]
	  val dcls = if #unpickler(#suppress flgs) then dcls else mkUnpickler() :: dcls
	  val dcls = if #pickler(#suppress flgs) then dcls else mkPickler() :: dcls
	  in
	    dcls
	  end

  (* generate the pickling functions for a type alias *)
    and genAliasFuncs flgs _ = [] (* FIXME *)

  (* generate code for encoding the fields of a product *)
    and encodeFields (E.TUPLE fields) = let
	  fun enc ([], stms) = List.rev stms
	    | enc ((ix, ty)::flds, stms) = let
		val field = CL.mkIndirect(CL.mkVar "this", U.tupleFieldName ix)
		val stms = encodeTy(field, ty) :: stms
		in
		  enc (flds, stms)
		end
	  in
	    enc (fields, [])
	  end
      | encodeFields (E.RECORD fields) = let
	  fun enc ([], stms) = List.rev stms
	    | enc ((label, ty)::flds, stms) = let
		val field = CL.mkIndirect(CL.mkVar "this", U.recordFieldName label)
		val stms = encodeTy(field, ty) :: stms
		in
		  enc (flds, stms)
		end
	  in
	    enc (fields, [])
	  end

  (* generate code for pickling a type expression *)
    and encodeTy (arg, E.OPTION(optModId, tyId)) =
	  writeTyExp (optModId, tyId, "_option", arg)
      | encodeTy (arg, E.SEQUENCE(optModId, tyId)) =
	  writeTyExp (optModId, tyId, "_seq",  arg)
      | encodeTy (arg, E.SHARED _) = raise Fail "shared types not supported yet"
      | encodeTy (arg, E.BASE(optModId, tyId)) = write (optModId, tyId, arg)

  (* generate code for decoding the fields of a product *)
    and decodeFields (E.TUPLE fields) = let
	  fun dec ([], stms, args) = (List.rev stms, List.rev args)
	    | dec ((ix, ty)::flds, stms, args) = let
		val name = "f" ^ Int.toString ix
		val stms = decodeTy(name, ty) :: stms
		in
		  dec (flds, stms, CL.mkVar name :: args)
		end
	  in
	    dec (fields, [], [])
	  end
      | decodeFields (E.RECORD fields) = let
	  fun dec ([], stms, args) = (List.rev stms, List.rev args)
	    | dec ((label, ty)::flds, stms, args) = let
		val name = "f" ^ label
		val stms = decodeTy(name, ty) :: stms
		in
		  dec (flds, stms, CL.mkVar name :: args)
		end
	  in
	    dec (fields, [], [])
	  end

  (* generate code for decoding a type expression *)
    and decodeTy (x, E.OPTION(optModId, tyId)) =
	  CL.mkDeclInit(CL.autoTy, x, readTyExp (optModId, tyId, "_option", [isArg]))
      | decodeTy (x, E.SEQUENCE(optModId, tyId)) =
	  CL.mkDeclInit(CL.autoTy, x, readTyExp (optModId, tyId, "_seq", [isArg]))
      | decodeTy (x, E.SHARED _) = raise Fail "shared types not supported yet"
      | decodeTy (x, E.BASE ty) = CL.mkDeclInit(CL.autoTy, x, read ty)

  end


