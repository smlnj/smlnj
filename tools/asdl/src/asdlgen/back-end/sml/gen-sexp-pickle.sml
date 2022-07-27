(* gen-sexp-pickle.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Generate the S-expression I/O code.
 *)

structure GenSExpPickle : sig

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

  (* out stream as a variable expression *)
    val outSV = S.IDexp "outS"
  (* generate a simple application *)
    fun funApp (f, args) = S.appExp(S.IDexp f, S.tupleExp args)
  (* expression to add a space to the output *)
    val space = funApp("space", [outSV])
  (* pairs *)
    fun pairExp (a, b) = S.TUPLEexp[a, b]

    fun sexpPklMod () = ModV.getSExpName PrimTypes.primTypesId
    fun outStrmTy () = S.CONty([], sexpPklMod() ^ ".outstream")
    fun inStrmTy () = S.CONty([], sexpPklMod() ^ ".instream")

    fun gen (AST.Module{isPrim=false, id, decls}) = let
	  val sign = S.AUGsig(
(* TODO: move Util.sigName to SML view *)
		S.IDsig(Util.sigName(ModV.getPickleSigName id, NONE)),
		[ S.WHERETY([], ["instream"], inStrmTy()),
		  S.WHERETY([], ["outstream"], outStrmTy())])
	  val typeModName = ModV.getName id
	  val sexpModName = ModV.getSExpName id
	  val sigName = Util.sigName(sexpModName, NONE)
	  fun genGrp (dcls, dcls') = S.FUNdec(List.foldr (genType typeModName) [] dcls) :: dcls'
	  val decls = List.foldr genGrp [] (SortDecls.sort (!decls))
	  val decls = S.VERBdec[
		  StringSubst.expand [("PICKLER", sexpPklMod ())] Fragments.sexpUtil
		] :: decls
	  in
	    S.STRtop(sexpModName, SOME(false, sign), S.BASEstr decls)
	  end
      | gen _ = raise Fail "GenSExpPickle.gen: unexpected primitive module"

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
		  ModV.getSExpName modId, ".", Util.picklerName tyId
		]
	  fun genTag (tagTyId, tag) = funApp(
		baseWriter(SOME PT.primTypesId, tagTyId),
		[outSV, S.NUMexp("0w" ^ Int.toString tag)])
	  fun genEnum name = funApp("writeEnum", [outSV, S.STRINGexp name])
	  fun genSExp (f, genContents) = let
		  val contents = S.SEQexp(List.foldr
			(fn (stm, stms) => space::stm::stms)
			  []
			    (genContents()))
		  in
		    funApp("writeSExp", [
			outSV, S.STRINGexp f, S.fnExp[(S.unitPat, contents)]
		      ])
		  end
	  fun gen (arg, E.UNIT conId) = genEnum (getConName conId)
	    | gen (arg, E.ENUM(nCons, cons)) = let
		fun genRule (tag, conId) = let
		      val name = getConName conId
		      in
			(S.IDpat name, genEnum name)
		      end
		in
		  S.caseExp(arg, List.map genRule cons)
		end
	    | gen (arg, E.WRAP(conId, fields)) = let
		val (isLabeled, lhsPat, xs) = objPat fields
		in
		  S.LETexp(
		    [S.VALdec(S.CONpat(getConName conId, lhsPat), arg)],
		    genSExp (getConName conId, fn () => List.map (genTy' isLabeled) xs))
		end
	    | gen (arg, E.SWITCH(optAttribs, nCons, cons)) = let
		val tagTyId = E.tagTyId nCons
		fun genRule (_, conId, optArg) = let
		      val conName = getConName conId
		      in
			case E.prefixWithAttribs(optAttribs, optArg)
			 of NONE => (S.IDpat conName, genEnum conName)
			  | SOME obj => let
			      val (isLabeled, pat, flds) = objPat obj
			      val pat = S.CONpat(conName, pat)
			      val exp = genSExp(conName,
				    fn () => List.map (genTy' isLabeled) flds)
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
		val (isLabeled, pat, args) = objPat obj
		val arity = List.length args
		in
		  S.LETexp(
		    [S.VALdec(pat, arg)],
		    genSExp (
		      Int.toString arity ^ "-tuple",
		      fn () => List.map (genTy' isLabeled) args))
		end
	(* create a pattern for matching against a product type; returns the pattern and the
	 * bound variables with their types.
	 *)
	  and objPat (E.TUPLE tys) = let
		val args = List.map (fn (i, ty) => ("x"^Int.toString i, ty)) tys
		in
		  (false, S.tuplePat(List.map (S.IDpat o #1) args), args)
		end
	    | objPat (E.RECORD flds) = let
		val pat = S.RECORDpat{
			fields = List.map (fn fld => (#1 fld, S.IDpat(#1 fld))) flds,
			flex = false
		      }
		in
		  (true, pat, flds)
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
	  and genTy' false (x, ty) = genTy (S.IDexp x, ty)
	    | genTy' true (x, ty) = genSExp(x, fn () => [genTy (S.IDexp x, ty)])
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
	  val inSV = S.IDexp "inS"
(*
	  fun getConName conId = concat[typeModName, ".", ConV.getName conId]
	  fun baseReader (NONE, tyId) = Util.unpicklerName tyId
	    | baseReader (SOME modId, tyId) = concat[
		  ModV.getSExpName modId, ".", Util.unpicklerName tyId
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
			  | SOME obj => (pat, genObj(obj, fn x => S.appExp(S.IDexp conName, x)))
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
		  S.LETexp(decs, k(S.TUPLEexp(List.map S.IDexp xs)))
		end
	    | genObj (E.RECORD fields, k) = let
		val decs = List.map
		      (fn (lab, ty) => S.VALdec(S.IDpat lab, genTy ty))
			fields
		val fields = List.map (fn (lab, _) => (lab, S.IDexp lab)) fields
		in
		  S.LETexp(decs, k(S.RECORDexp fields))
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
*)
	  in
	    S.simpleFB(Util.unpicklerName tyId, ["inS"],
	      S.raiseExp(funApp("Fail", [
		  S.STRINGexp "S-Expression reading is not implemented yet"
		])))

	  end

  end
