(* gen-types.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Generate the SML type definitions for an ASDL module.
 *)

structure GenTypes : sig

  (* generate the structure that contains the type definitions for an
   * ASDL module.
   *)
    val gen : AST.module -> SML.top_decl

  end = struct

    structure V = SMLView
    structure ModV = V.Module
    structure TyV = V.Type
    structure ConV = V.Constr
    structure S = SML

    fun gen (AST.Module{isPrim=false, id, decls}) = if (#types (ModV.getSuppress id))
	  then S.VERBtop[concat["(* ", ModV.getName id, " suppressed *)\n"]]
	  else let
	    val name = ModV.getName id
	    fun genGrp (dcls, dcls') = (case List.foldr genType ([], []) dcls
		   of ([], tbs) => List.map S.TYPEdec tbs @ dcls'
		    | (dbs, tbs) => S.DATATYPEdec(dbs, tbs) :: dcls'
		  (* end case *))
	    val {prologue, epilogue} = ModV.getImplementationCode id
	  (* sort and group the ASDL declarations *)
	    val asdlDecls = SortDecls.sort (!decls)
	  (* add optional epilogue code *)
	    val dcls = if null epilogue
		  then []
		  else [S.VERBdec epilogue]
	  (* generate declarations *)
	    val dcls = List.foldr genGrp dcls asdlDecls
	  (* add optional prologue code *)
	    val dcls = if null prologue
		  then dcls
		  else S.VERBdec prologue :: dcls
	    in
	      S.STRtop(name, genSig(id, asdlDecls), S.BASEstr dcls)
	    end
      | gen _ = raise Fail "GenTypes.gen: unexpected primitive module"

  (* generate the optional signature constraint for the module.  This is only
   * generated when the view includes an interface_prologue or interface_epilogue
   * property for the module.
   *)
    and genSig (id, decls) = (
	  case ModV.getInterfaceCode id
	   of {prologue=[], epilogue=[]} => NONE
	    | {prologue, epilogue} => let
		fun tySpec (tvs, name, ty) = S.TYPEspec(false, tvs, name, SOME ty)
		fun genGrp (dcls, dcls') = (case List.foldr genType ([], []) dcls
		       of ([], tbs) => List.map tySpec tbs @ dcls'
			| (dbs, tbs) => S.DATATYPEspec(dbs, tbs) :: dcls'
		      (* end case *))
	      (* add optional epilogue code *)
		val specs = if null epilogue
		      then []
		      else [S.VERBspec epilogue]
	      (* generate declarations for any specified wrapper/unwrappers *)
		fun addWrappers (AST.TyDcl{id, ...}, specs) = let
		      fun valSpec (f, ty1, ty2) =
			    S.VALspec(f, S.FUNty(S.CONty([], ty1), S.CONty([], ty2)))
		      val specs = (case TyV.getUnwrapper id
			     of NONE => specs
			      | SOME f =>
				  valSpec(f, TyV.getNaturalType id, TyV.getName id)
				    :: specs
			    (* end case *))
		      val specs = (case TyV.getWrapper id
			     of NONE => specs
			      | SOME f =>
				  valSpec(f, TyV.getName id, TyV.getNaturalType id)
				    :: specs
			    (* end case *))
		      in
			specs
		      end
		val specs = List.foldr
		      (fn (dcls, specs) => List.foldr addWrappers specs dcls)
			specs
			  decls
	      (* generate declarations *)
		val specs = List.foldr genGrp specs decls
	      (* add optional prologue code *)
		val specs = if null prologue
		      then specs
		      else [S.VERBspec prologue]
		in
		  SOME(false, S.BASEsig specs)
		end
	  (* end case *))

    and genType (AST.TyDcl{id, def, ...}, (dbs, tbs)) = let
	  val name = TyV.getName id
	  fun db cons = let
		fun con (AST.Constr{id, fields=[], ...}) = (ConV.getName id, NONE)
		  | con (AST.Constr{id, fields, ...}) =
		      (ConV.getName id, SOME(genProdTy fields))
		in
		  (S.DB([], name, List.map con cons)::dbs, tbs)
		end
	  in
	    case !def
	     of AST.EnumTy cons => db cons
	      | AST.SumTy{attribs, cons} => db cons
	      | AST.ProdTy{fields} => (dbs, ([], name, genProdTy fields)::tbs)
	      | AST.AliasTy ty => (dbs, ([], name, genTyExp ty)::tbs)
	      | AST.PrimTy => raise Fail "unexpected primitive type"
	    (* end case *)
	  end

  (* generate a type expression for a list of fields *)
    and genProdTy [] = S.CONty([], "unit")
      | genProdTy [{label=AST.Pos _, ty}] = genTyExp ty
      | genProdTy (fields as {label=AST.Pos _, ...}::_) =
	  S.TUPLEty(List.map (genTyExp o #ty) fields)
      | genProdTy fields = let
	  fun field {label=AST.Lab lab, ty} = (lab, genTyExp ty)
	    | field _ = raise Fail "missing label in record type"
	  in
	    S.RECORDty(List.map field fields)
	  end

  (* get the SML type expression for an ASDL type expression *)
    and genTyExp (AST.Typ(ty, tyc)) = let
	  val tyName = (case ty
		 of AST.BaseTy tyId => TyV.getNaturalType tyId
		  | AST.ImportTy(modId, tyId) => String.concat[
			ModV.getName modId, ".", TyV.getNaturalType tyId
		      ]
		  | AST.LocalTy(AST.TyDcl{id, ...}) => TyV.getNaturalType id
		(* end case *))
	  val ty' = S.CONty([], tyName)
	  in
	    case tyc
	     of AST.NoTyc => ty'
	      | AST.OptTyc => S.CONty([ty'], "option")
	      | AST.SeqTyc => S.CONty([ty'], "list")
	      | AST.SharedTyc => ty'
	    (* end case *)
	  end

  end
