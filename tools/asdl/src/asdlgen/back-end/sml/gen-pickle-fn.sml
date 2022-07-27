(* gen-pickle-fn.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Generate the memory and file pickler structures.
 *)

functor GenPickleFn (

  (* function to get the name of the pickler module for an ASDL module *)
    val getPklModName : AST.ModuleId.t -> string
  (* the names of the functions for reading/writing bytes *)
    val getByte : string
    val putByte : string

  ) : sig

  (* generate the pickler structure *)
    val gen : AST.module -> SML.top_decl

  end = struct

    structure PT = PrimTypes
    structure V = SMLView
    structure ModV = V.Module
    structure TyV = V.Type
    structure ConV = V.Constr
    structure E = Encoding
    structure S = SML

  (***** Structure generation *****)

  (* generate a simple application *)
    fun funApp (f, args) = S.appExp(S.IDexp f, S.tupleExp args)
  (* pairs *)
    fun pairPat (a, b) = S.TUPLEpat[a, b]
    fun pairExp (a, b) = S.TUPLEexp[a, b]

    fun pklMod () = getPklModName PrimTypes.primTypesId
    fun outStrmTy () = S.CONty([], pklMod() ^ ".outstream")
    fun inStrmTy () = S.CONty([], pklMod() ^ ".instream")

    val outSV = S.IDexp "outS"
    val outSP = S.IDpat "outS"
    val inSV = S.IDexp "inS"

    fun gen (AST.Module{isPrim, id, decls}) = let
	  val typeModName = ModV.getName id (* name of module that defines the types *)
	  val pickleModName = getPklModName id
	  val sign = S.AUGsig(
(* TODO: move Util.sigName to SML view *)
		S.IDsig(Util.sigName(ModV.getPickleSigName id, NONE)),
		[ S.WHERETY([], ["instream"], inStrmTy()),
		  S.WHERETY([], ["outstream"], outStrmTy())])
	  val decls = if isPrim
		then let
		  val gen = genPrimType typeModName
		  in
		    List.foldr gen [] (!decls)
		  end
		else let
	          val gen = genType typeModName
		  fun genGrp (dcls, dcls') = S.FUNdec(List.foldr gen [] dcls) :: dcls'
		  in
		    List.foldr genGrp [] (SortDecls.sort (!decls))
		  end
	  val decls = S.VERBdec[
		  StringSubst.expand [("PICKLER", pklMod ())]
		    (if isPrim then Fragments.streams else Fragments.pickleUtil)
		] :: decls
	  in
	    S.STRtop(pickleModName, SOME(false, sign), S.BASEstr decls)
	  end

    and genPrimType typeModName (AST.TyDcl{id, ...}, decls) = let
	  val pklModName = typeModName ^ "Pickle"
	  val wrId = Util.picklerName id
	  val wr = S.simpleVB(wrId,
		funApp (concat[pklModName, ".", wrId], [S.IDexp putByte]))
	  val rdId = Util.unpicklerName id
	  val rd = S.simpleVB(rdId,
		funApp (concat[pklModName, ".", rdId], [S.IDexp getByte]))
	  in
	    wr :: rd :: decls
	  end

    and genType typeModName (dcl, fbs) = let
	  val (id, encoding) = E.encoding dcl
	  val name = TyV.getName id
	  in
	    genWriter (typeModName, id, encoding) ::
	    genReader (typeModName, id, encoding) ::
	      fbs
	  end

    and genWriter (typeModName, tyId, encoding) = let
	  fun qId id = concat[typeModName, ".", id]
	  fun getConName conId = qId (ConV.getName conId)
	  fun baseWriter (NONE, tyId) = Util.picklerName tyId
	    | baseWriter (SOME modId, tyId) = concat[
		  getPklModName modId, ".", Util.picklerName tyId
		]
	  fun genTag (tagTyId, tag) = funApp(
		baseWriter(SOME PT.primTypesId, tagTyId),
		[outSV, S.NUMexp("0w" ^ Int.toString tag)])
	  fun gen (arg, E.UNIT conId) = S.unitExp (* no storage required *)
	    | gen (arg, E.ENUM(nCons, cons)) = let
		val tagTyId = E.tagTyId nCons
		fun genRule (tag, conId) =
		      (S.IDpat(getConName conId), genTag (tagTyId, tag))
		in
		  S.caseExp(arg, List.map genRule cons)
		end
	    | gen (arg, E.WRAP(conId, fields)) = let
		val (lhsPat, xs) = objPat fields
		in
		  S.letExp(
		    [S.VALdec(S.CONpat(getConName conId, lhsPat), arg)],
		    S.SEQexp(List.map genTy' xs))
		end
	    | gen (arg, E.SWITCH(optAttribs, nCons, cons)) = let
		val tagTyId = E.tagTyId nCons
		fun genRule (tag, conId, optArg) = let
		      val encTag = genTag(tagTyId, tag)
		      val conName = getConName conId
		      in
			case E.prefixWithAttribs(optAttribs, optArg)
			 of NONE => (S.IDpat conName, encTag)
			  | SOME obj => let
			      val (pat, flds) = objPat obj
			      val pat = S.CONpat(conName, pat)
			      val exp = S.SEQexp(encTag :: List.map genTy' flds)
			      in
				(pat, exp)
			      end
			(* end case *)
		      end
		in
		  S.caseExp(arg, List.map genRule cons)
		end
	    | gen (arg, E.OBJ obj) = genProd (arg, obj)
	    | gen (arg, E.ALIAS ty) = genTy (arg, ty)
	  and genProd (arg, obj) = let
		val (pat, args) = objPat obj
		in
		  S.letExp(
		    [S.VALdec(pat, arg)],
		    S.SEQexp(List.map genTy' args))
		end
	(* create a pattern for matching against a product type; returns the
	 * pattern and the bound variables with their types.
	 *)
	  and objPat (E.TUPLE tys) = let
		val args = List.map (fn (i, ty) => ("x"^Int.toString i, ty)) tys
		in
		  (S.tuplePat(List.map (S.IDpat o #1) args), args)
		end
	    | objPat (E.RECORD flds) = let
		val pat = S.RECORDpat{
			fields = List.map (fn fld => (#1 fld, S.IDpat(#1 fld))) flds,
			flex = false
		      }
		in
		  (pat, flds)
		end
	  and genTy (arg, E.OPTION ty) =
		S.appExp(
		  funApp ("writeOption", [S.IDexp(baseWriter ty)]),
		  pairExp(outSV, arg))
	    | genTy (arg, E.SEQUENCE ty) =
		S.appExp(
		  funApp ("writeSeq", [S.IDexp(baseWriter ty)]),
		  pairExp(outSV, arg))
	    | genTy (arg, E.SHARED ty) = raise Fail "shared types not supported yet"
	    | genTy (arg, E.BASE ty) = funApp (baseWriter ty, [outSV, arg])
	  and genTy' (x, ty) = genTy (S.IDexp x, ty)
        (* generate the body of the writer; which may involve a conversion
	 * from the natural type.
	 *)
	  val body = (case TyV.getUnwrapper tyId
		 of NONE => gen(S.IDexp "obj", encoding)
		  | SOME f => S.simpleLet("pkl", funApp(qId f, [S.IDexp "obj"]),
		      gen(S.IDexp "pkl", encoding))
		(* end case *))
	  in
	    S.simpleFB(Util.picklerName tyId, ["outS", "obj"], body)
	  end

    and genReader (typeModName, tyId, encoding) = let
	  fun qId id = concat[typeModName, ".", id]
	  fun getConName conId = qId(ConV.getName conId)
	  fun baseReader (NONE, tyId) = Util.unpicklerName tyId
	    | baseReader (SOME modId, tyId) = concat[
		  getPklModName modId, ".", Util.unpicklerName tyId
		]
	  fun genTag tagTyId = funApp(
		baseReader(SOME PT.primTypesId, tagTyId),
		[inSV])
	  val dfltRule = (S.WILDpat, S.raiseExp(S.IDexp "ASDL.DecodeError"))
	  fun gen (E.UNIT conId) = S.IDexp(getConName conId)
	    | gen (E.ENUM(nCons, cons)) = let
		fun genRule (tag, conId) = let
		      val pat = S.NUMpat("0w"^Int.toString tag)
		      in
			(pat, S.IDexp(getConName conId))
		      end
		in
		  S.caseExp(genTag(E.tagTyId nCons), List.map genRule cons @ [dfltRule])
		end
	    | gen (E.WRAP(conId, fields)) =
		genObj(fields, fn  x => S.appExp(S.IDexp(getConName conId), x))
	    | gen (E.SWITCH(optAttribs, nCons, cons)) = let
		fun genRule (tag, conId, optArg) = let
		      val conName = getConName conId
		      val pat = S.NUMpat("0w"^Int.toString tag)
		      in
			case E.prefixWithAttribs(optAttribs, optArg)
			 of NONE => (pat, S.IDexp conName)
			  | SOME obj =>
			      (pat, genObj(obj, fn x => S.appExp(S.IDexp conName, x)))
			(* end case *)
		      end
		in
		  S.caseExp(genTag(E.tagTyId nCons), List.map genRule cons @ [dfltRule])
		end
	    | gen (E.OBJ obj) = genObj(obj, fn x => x)
	    | gen (E.ALIAS ty) = genTy ty
	  and genObj (E.TUPLE tys, k) = let
		val xs = List.mapi (fn (i, _) => "x"^Int.toString i) tys
		val decs = ListPair.map
		      (fn (x, (_, ty)) => S.VALdec(S.IDpat x, genTy ty))
			(xs, tys)
		in
		  S.letExp(decs, k (S.TUPLEexp(List.map S.IDexp xs)))
		end
	    | genObj (E.RECORD fields, k) = let
		val decs = List.map
		      (fn (lab, ty) => S.VALdec(S.IDpat lab, genTy ty))
			fields
		val fields = List.map (fn (lab, _) => (lab, S.IDexp lab)) fields
		in
		  S.letExp(decs, k (S.RECORDexp fields))
		end
	  and genTy (E.OPTION ty) =
		S.appExp(
		  funApp ("readOption", [S.IDexp(baseReader ty)]),
		  inSV)
	    | genTy (E.SEQUENCE ty) =
		S.appExp(
		  funApp ("readSeq", [S.IDexp(baseReader ty)]),
		  inSV)
	    | genTy (E.SHARED ty) = raise Fail "shared types not supported yet"
	    | genTy (E.BASE ty) = funApp (baseReader ty, [inSV])
        (* generate the body of the reader; which may involve a conversion
	 * to the natural type.
	 *)
	  val body = (case TyV.getWrapper tyId
		 of NONE => gen encoding
		  | SOME f =>
		      S.simpleLet("pkl", gen encoding, funApp(qId f, [S.IDexp "pkl"]))
		(* end case *))
	  in
	    S.simpleFB(Util.unpicklerName tyId, ["inS"], body)
	  end

  end
