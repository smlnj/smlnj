(* gen-types.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * TODO:
 * 	encode/decoder names should be taken from view
 *	proper implementation of destructor functions
 *)

structure GenTypes : sig

  (* generate the type definitions for an ASDL module.  The result will be
   * a namespace declaration enclosing the definitions.
   *)
    val gen : Util.flags -> AST.module -> Cxx.decl

  end = struct

    structure V = CxxView
    structure ModV = V.Module
    structure TyV = V.Type
    structure ConV = V.Constr
    structure CL = Cxx
    structure U = Util

  (* flags for controlling code generation *)

    val osParam = CL.param(CL.T_Ref(CL.T_Named "asdl::outstream"), "os")
    val isParam = CL.param(CL.T_Ref(CL.T_Named "asdl::instream"), "is")

  (* generate the inline access-methods for a field *)
    fun genAccessMethods {label, ty} = let
	  val fieldTy = U.tyexpToCxx ty
	  val field = CL.mkIndirect(CL.mkVar "this", U.fieldName label)
	  val get = CL.mkInlineConstMethDcl(
		fieldTy, U.fieldGetName label, [],
		CL.mkReturn field)
	  val set = CL.mkInlineMethDcl(
		CL.voidTy, U.fieldSetName label, [CL.param(fieldTy, "v")],
		CL.mkAssign(field, CL.mkVar "v"))
	  in
	    [get, set]
	  end

  (* generate a field declaration *)
    fun genField {label, ty} = CL.mkVarDcl(U.tyexpToCxx ty, U.fieldName label)

  (* a field initialization expression *)
    fun genFieldInit {label, ty} =
	  CL.mkApply(U.fieldName label, [U.fieldParam label])

  (* add view-property code to the list of class-body declarations *)
    fun addCode ([], dcls) = dcls
      | addCode (code, dcls) = dcls @ [CL.D_Verbatim code]

    fun gen (flgs : U.flags) (AST.Module{isPrim=false, id, decls}) = let
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
	      then let
		val fwdDefs = List.map genForwardDcl (!decls)
		val repDefs = List.foldr (genType flgs) [] (!decls)
		in
		  CL.D_Namespace(namespace, fwdDefs @ repDefs)
		end
	      else CL.D_Comment["types suppressed for " ^ namespace]
	  end
      | gen _ _ = raise Fail "GenTypes.gen: unexpected primitive module"

  (* generate a forward declaration for a type *)
    and genForwardDcl (AST.TyDcl{id, def, ...}) = let
	  val name = TyV.getName id
	  fun verb prefix = CL.D_Verbatim[concat[prefix, name, ";"]]
	  in
	    case !def
	     of AST.EnumTy _ => verb "enum class "
	      | AST.SumTy _ => verb "class "
	      | AST.ProdTy _ => verb "class "
	      | AST.AliasTy _ => CL.D_Verbatim[]
	      | AST.PrimTy => raise Fail "unexpected primitive type"
	    (* end case *)
	  end

    and genType (flags : U.flags) (AST.TyDcl{id=tyId, def, ...}, dcls) = let
	  val {arenaAlloc, suppress} = flags
	  val tyName = TyV.getName tyId
	  val genNew = U.genNew flags
	(* if required, add additional prototypes for pickling/unpickling
	 * options, sequences, and shared values of this type
	 *)
	  fun genTyExp (cty, suffix, dcls) = let
		val fnSuffix = tyName ^ suffix
		val pickler = if #pickler suppress
		      then CL.D_Comment[fnSuffix ^ " pickler suppressed"]
		      else CL.mkProto(CL.voidTy, "write_" ^ fnSuffix, [
			  osParam, CL.param(CL.T_Ref cty, "arg")
			])
		val unpickler = if #unpickler suppress
		      then CL.D_Comment[fnSuffix ^ " pickler suppressed"]
		      else CL.mkProto(cty, "read_" ^ fnSuffix, [isParam])
		in
		  pickler :: unpickler :: dcls
		end
(* TODO: shared types *)
	  val dcls = if AST.TypeId.isSeqArg tyId
		then genTyExp (U.seqTy tyId, "_seq", dcls)
		else dcls
	  val dcls = if AST.TypeId.isOptionArg tyId
		then genTyExp (U.optionTy tyId, "_option", dcls)
		else dcls
	(* generate a enum-class definition and function declarations for an enumeration
	 * type.
	 *)
	  fun genEnumClass cons = let
		val ty = CL.T_Named tyName
		val con::conr = List.map (fn (AST.Constr{id, ...}) => ConV.getName id) cons
		val enumDcl = CL.D_EnumDef{
			isClass = true,
			name = tyName,
			repTy = NONE,
			cons = (con, SOME(CL.mkInt 1)) :: List.map (fn c => (c, NONE)) conr
		      }
	      (* prototypes for pickler/unpickler functions *)
		val pickler = if #pickler suppress
		      then CL.D_Comment["pickler suppressed for "^tyName]
		      else CL.mkProto(CL.voidTy, U.picklerName tyId, [
			  osParam, CL.param(ty, "v")
			])
		val unpickler = if #unpickler suppress
		      then CL.D_Comment["unpickler suppressed for "^tyName]
		      else CL.mkProto(ty, U.unpicklerName tyId, [isParam])
		in
		  [enumDcl, pickler, unpickler]
		end
	(* generate the base-class definition for a sum type *)
	  fun genBaseClass (tyId, attribs, cons) = let
	      (* access methods for attribute fields *)
		val accessMeths = List.foldr
		      (fn (fld, meths) => genAccessMethods fld @ meths)
			[] attribs
	      (* pickling/unpickling methods *)
		val pickleMeth = if #pickler suppress
		      then CL.D_Comment["pickler method suppressed"]
		      else CL.mkVirtualProto(
(* FIXME: the method name could be set in the view! *)
			CL.voidTy, "write", [osParam],
			true (* abstract method *))
		val unpickleMeth = if #unpickler suppress
		      then CL.D_Comment["unpickler method suppressed"]
		      else CL.mkStaticMethProto(
(* FIXME: the method name could be set in the view! *)
			CL.T_Ptr(CL.T_Named tyName), "read", [isParam])
	      (* type definition for tag values *)
		val tagTypeDcl = let
		      val con::conr = List.map (fn (AST.Constr{id, ...}) => U.constrTagName id) cons
		      in
			CL.D_EnumDef{
			    isClass = false,
			    name = "_tag_t",
			    repTy = NONE,
			    cons = (con, SOME(CL.mkInt 1)) :: List.map (fn c => (c, NONE)) conr
			  }
		      end
		val tagTy = CL.T_Named "_tag_t"
	      (* tag field *)
		val tagDcl = CL.mkVarDcl(tagTy, U.tagFieldName)
	      (* create the constructor function *)
		val initTag = CL.mkApply(U.tagFieldName, [CL.mkVar "tag"])
		val initAttribs = List.map genFieldInit attribs
		val constr = CL.mkConstrDcl(
		      tyName,
		      CL.param(tagTy, "tag") :: List.map U.fieldToParam attribs,
		      initTag :: initAttribs, CL.mkBlock[])
	      (* destructor *)
		val destr = CL.D_Destr(["virtual"], [], tyName, NONE)
		in
		  CL.D_ClassDef{
		      name = tyName, args = NONE, from = NONE,
		      public = destr ::
			pickleMeth ::
			unpickleMeth ::
			addCode(TyV.getPublicCode tyId, accessMeths),
		      protected = tagTypeDcl ::
			constr ::
			tagDcl ::
			addCode(TyV.getProtectedCode tyId, List.map genField attribs),
		      private = addCode(TyV.getPrivateCode tyId, [])
		    }
		end
	(* generate a derived-class definition for a constructor in a sum type *)
	  fun genConsClass attribs = let
		val nAttribs = List.length attribs
		val attribParams = List.map U.fieldToParam attribs
		val attribInit = List.map (U.fieldParam o #label) attribs
		fun baseInit con = CL.mkApply(
		      tyName,
		      CL.mkVar(concat[tyName, "::", U.constrTagName con]) :: attribInit)
		fun gen (AST.Constr{id, fields, ...}, dcls) = let
		      val name = ConV.getName id
		      val classTy = CL.T_Named name
		      val extra = List.drop(fields, nAttribs)
		      val accessMeths = List.foldr
			    (fn (fld, meths) => genAccessMethods fld @ meths)
			      [] extra
		    (* pickling method *)
		      val pickleMeth = if #pickler suppress
			    then CL.D_Comment["pickler method suppressed"]
(* FIXME: the method name could be set in the view! *)
			    else CL.mkMethProto(CL.voidTy, "write", [osParam])
		    (* create the constructor function and `make` method *)
		      val (constr, makeMeth) = let
			    val params = List.map U.fieldToParam fields
			    val args = List.map (U.fieldParam o #label) fields
			    val constr = CL.mkConstrDcl(
				  name, params,
			          baseInit id :: List.map genFieldInit extra, CL.mkBlock[])
			    val makeMeth = CL.mkInlineStaticMethDcl(
				  CL.T_Ptr classTy, "make", params,
				  CL.mkReturn(genNew(classTy, args)))
			    in
			      (constr, makeMeth)
			    end
		      val destr = CL.mkDestrProto name
		      in
			CL.D_ClassDef{
			    name = name, args = NONE, from = SOME("public " ^ tyName),
			    public = constr ::
			      destr ::
			      makeMeth ::
			      pickleMeth ::
			      addCode (ConV.getPublicCode id, accessMeths),
			    protected = addCode (ConV.getProtectedCode id, []),
			    private = addCode (ConV.getPrivateCode id, List.map genField fields)
			  } :: dcls
		      end
		in
		  gen
		end
	(* generate the class definition for a product type *)
	  fun genProdClass (tyId, fields) = let
		val accessMeths = List.foldr
		      (fn (fld, meths) => genAccessMethods fld @ meths)
			[] fields
	      (* pickling/unpickling methods *)
		val pickleMeth = if #pickler suppress
		      then CL.D_Comment["pickler method suppressed"]
		      else CL.mkMethProto(CL.voidTy, "write", [osParam])
		val unpickleMeth = if #unpickler suppress
		      then CL.D_Comment["unpickler method suppressed"]
		      else CL.mkStaticMethProto(
			CL.T_Ptr(CL.T_Named tyName), "read", [isParam])
	      (* create the constructor function and `make` method *)
		val (constr, makeMeth) = let
		      val classTy = CL.T_Named tyName
		      val params = List.map U.fieldToParam fields
		      val args = List.map (U.fieldParam o #label) fields
		      val constr = CL.mkConstrDcl(
			    tyName, params, List.map genFieldInit fields, CL.mkBlock[])
		      val makeMeth = CL.mkInlineStaticMethDcl(
			    CL.T_Ptr classTy, "make", params,
			    CL.mkReturn(genNew(classTy, args)))
		      in
			(constr, makeMeth)
		      end
	      (* destructor prototype *)
		val destr = CL.mkDestrProto tyName
		in
		  CL.D_ClassDef{
		      name = tyName, args = NONE, from = NONE,
		      public = constr ::
			destr ::
			makeMeth ::
			pickleMeth ::
			unpickleMeth ::
			addCode (TyV.getPublicCode tyId, accessMeths),
		      protected = addCode (TyV.getProtectedCode tyId, []),
		      private = addCode (TyV.getPrivateCode tyId, List.map genField fields)
		    }
		end
	(* generate the C++ type definition for the ASDL type and add to dcls *)
	  val dcls = (case !def
		 of AST.EnumTy cons =>
		      genEnumClass cons @ dcls
		  | AST.SumTy{attribs, cons=[AST.Constr{id, fields, ...}]} =>
		      genProdClass (tyId, attribs@fields) :: dcls
		  | AST.SumTy{attribs, cons} =>
		      genBaseClass (tyId, attribs, cons) ::
		      List.foldr (genConsClass attribs) dcls cons
		  | AST.ProdTy{fields} =>
		      genProdClass (tyId, fields) :: dcls
		  | AST.AliasTy ty =>
		      CL.D_Typedef(tyName, U.tyexpToCxx ty) :: dcls
		  | AST.PrimTy => raise Fail "unexpected primitive type"
		(* end case *))
	  in
	    dcls
	  end

  end
