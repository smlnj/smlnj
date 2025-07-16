(* gen-pickle-fn.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * Generate the memory and file pickler structures.
 *
 * TODO:
 *    finish support for suppression of the generation of the pickler or unpickler
 *)

functor GenPickleFn (

    (* function to get the name of the pickler module for an ASDL module *)
    val getPklModName : AST.ModuleId.t -> string

    (* the names of the functions for reading/writing bytes *)
    val getByte : string
    val putByte : string

  ) : sig

    (* generate the pickler structure; the boolean is true if there is any
     * sharing in any of the picklers.
     *)
    val gen : bool -> AST.module -> SML.top_decl

  end = struct

    structure BT = BaseTypes
    structure V = SMLView
    structure FileV = V.File
    structure ModV = V.Module
    structure TyV = V.Type
    structure ConV = V.Constr
    structure E = Encoding
    structure GenShCxt = GenSharingContext
    structure S = SML

  (***** Structure generation *****)

  (* generate a simple application *)
    fun funApp (f, args) = S.appExp(S.IDexp f, S.tupleExp args)
  (* pairs *)
    fun pairPat (a, b) = S.TUPLEpat[a, b]
    fun pairExp (a, b) = S.TUPLEexp[a, b]

    fun pklMod () = getPklModName BT.asdlTypesId

    val cxtSV = S.IDexp "cxt"
    val outSV = S.IDexp "outS"
    val inSV = S.IDexp "inS"

    fun outS true = S.selectExp("outS", cxtSV)
      | outS false = outSV
    fun inS true = S.selectExp("inS", cxtSV)
      | inS false = inSV

    fun outP true = S.CONSTRAINTpat(S.IDpat "cxt", S.CONty([], "outstream"))
      | outP false = S.CONSTRAINTpat(S.IDpat "outS", S.CONty([], "outstream"))
    val objP = S.IDpat "obj"
    fun inP true = S.CONSTRAINTpat(S.IDpat "cxt", S.CONty([], "instream"))
      | inP false = S.CONSTRAINTpat(S.IDpat "inS", S.CONty([], "instream"))

    fun gen anySharing (AST.Module{isPrim, id=modId, decls}) = let
          val tyModName = ModV.getName modId (* name of module that defines the types *)
          val pickleModName = getPklModName modId
          (* is either the pickler or unpickler suppressed? *)
          val {pickler=suppressWr, unpickler=suppressRd, ...} = FileV.getSuppress ()
          (* the ASDL stream types *)
          val instrmTy = S.CONty([], pklMod() ^ ".instream")
          val outstrmTy = S.CONty([], pklMod() ^ ".outstream")
          (* compute augmented signature *)
          val sign = let
                val inTy = if anySharing andalso not suppressRd
                      then GenShCxt.rdContextTy instrmTy
                      else instrmTy
                val outTy = if anySharing andalso not suppressWr
                      then GenShCxt.wrContextTy outstrmTy
                      else outstrmTy
                val whereTys =
                      (if suppressRd then [] else [S.WHERETY([], ["instream"], inTy)]) @
                      (if suppressWr then [] else [S.WHERETY([], ["outstream"], outTy)])
                in
                  S.AUGsig(
(* TODO: move Util.sigName to SML view *)
                    S.IDsig(Util.sigName(ModV.getPickleSigName modId, NONE)),
                    whereTys)
                end
          (* define the declarations in reverse order *)
          val decls = if isPrim
                then let
                  val pklModName = tyModName ^ "Pickle"
                  fun gen (AST.TyDcl{id, ...}, decls) = let
                        val decls = if suppressWr
                              then []
                              else let
                                val wrId = Util.picklerName id
                                val rhs = funApp (concat[pklModName, ".", wrId], [S.IDexp putByte])
                                in
                                  S.simpleVB(wrId, rhs) :: decls
                                end
                        val decls = if suppressRd
                              then decls
                              else let
                                val rdId = Util.unpicklerName id
                                val rhs = funApp (concat[pklModName, ".", rdId], [S.IDexp getByte])
                                in
                                  S.simpleVB(rdId, rhs) :: decls
                                end
                        in
                          decls
                        end
                  in
                    List.foldl gen [] (!decls)
                  end
                else let
                  val grps = SortDecls.sort (!decls)
                  fun genFB genFn (dcl : AST.type_decl, fbs) = let
                        val (id, encoding) = E.encoding dcl
                        val name = TyV.getName id
                        in
                          genFn (modId, anySharing, tyModName, id, encoding) :: fbs
                        end
                  fun genFunDec gen (dcls : AST.type_decl list, dcls' : SML.dec list) =
                        S.FUNdec([], List.foldr gen [] dcls) :: dcls'
                  val decls = if suppressWr
                        then []
                        else List.foldr (genFunDec (genFB genWriter)) [] grps
                  val decls = if suppressRd
                        then decls
                        else List.foldr (genFunDec (genFB genReader)) decls grps
(*
                  fun gen (dcl, fbs) = let
                        val (id, encoding) = E.encoding dcl
                        val name = TyV.getName id
                        val fbs = if suppressWr
                              then fbs
                              else genWriter (modId, anySharing, tyModName, id, encoding)
                                :: fbs
                        val fbs = if suppressRd
                              then fbs
                              else genReader (modId, anySharing, tyModName, id, encoding)
                                :: fbs
                        in
                          fbs
                        end
                  fun genGrp (dcls, dcls') =
                        S.FUNdec([], List.foldr gen [] dcls) :: dcls'
                  in
(*DEBUG*)print "# gen decls\n";
                    List.foldl genGrp [] (SortDecls.sort (!decls))
                  end
*)
                  in
                    decls
                  end
          (* add utility functions *)
          val verbDcls = if isPrim
                  then []
                else if anySharing
                  then let
                    val expand = StringSubst.expand [("PICKLER", pklMod ())]
                    val verb = if suppressWr
                          then []
                          else [expand Fragments.shareWrUtil]
                    val verb = if suppressRd
                          then verb
                          else expand Fragments.shareRdUtil :: verb
                    in
                      verb
                    end
                  else let
                    val expand = StringSubst.expand [("PICKLER", pklMod ())]
                    val verb = if suppressWr
                          then []
                          else [expand Fragments.wrUtil]
                    val verb = if suppressRd
                          then verb
                          else expand Fragments.rdUtil :: verb
                    in
                      verb
                    end
          val decls = S.VERBdec verbDcls :: decls
          (* add declaration of outstream *)
          val decls = if suppressWr
                  then decls
                else if anySharing
                  then S.TYPEdec([], "outstream", GenShCxt.wrContextTy outstrmTy) :: decls
                  else S.TYPEdec([], "outstream", outstrmTy) :: decls
          (* add declaration of instream *)
          val decls = if suppressRd
                  then decls
                else if anySharing
                  then S.TYPEdec([], "instream", GenShCxt.rdContextTy instrmTy) :: decls
                  else S.TYPEdec([], "instream", instrmTy) :: decls
          in
            S.STRtop(pickleModName, SOME(false, sign), S.BASEstr decls)
          end

    and genWriter (modId : AST.ModuleId.t, anySharing, tyModName, tyId, encoding) = let
          (* outstream variable, which is a context when we have sharing *)
          val outSV = if anySharing then cxtSV else outSV
          (* expression that defines the underlying outstream *)
          val outS = outS anySharing
          fun qId id = concat[tyModName, ".", id]
          fun getConName conId = qId (ConV.getName conId)
          (* writer for a primitive type*)
          fun primWriter tyId = S.IDexp(concat[
                  getPklModName BT.asdlTypesId, ".", Util.picklerName tyId
                ])
          (* writer expression for a type *)
          fun tyWriter (E.BASE tyId) = if anySharing
                then S.APPexp(S.IDexp "writePrim", primWriter tyId)
                else primWriter tyId
            | tyWriter (E.IMPORT(modId, tyId)) = S.IDexp(concat[
                  getPklModName modId, ".", Util.picklerName tyId
                ])
            | tyWriter (E.LOCAL(_, tyId)) = S.IDexp(Util.picklerName tyId)
          fun genTag (tagTyId, tag) = S.APPexp(
                primWriter tagTyId,
                S.TUPLEexp[outS, S.NUMexp("0w" ^ Int.toString tag)])
          (* gen : SML.exp * Encoding.t -> SML.exp
           * generate the expression that writes the given object (the first
           * argument) using the specified encoding.
           *)
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
                    S.SEQexp(List.map genTyExp' xs))
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
                              val exp = S.SEQexp(encTag :: List.map genTyExp' flds)
                              in
                                (pat, exp)
                              end
                        (* end case *)
                      end
                in
                  S.caseExp(arg, List.map genRule cons)
                end
            | gen (arg, E.OBJ obj) = genProd (arg, obj)
            | gen (arg, E.ALIAS ty) = genTyExp (arg, ty)
          (* generate code to write a product value *)
          and genProd (arg, obj) = let
                val (pat, args) = objPat obj
                in
                  S.letExp(
                    [S.VALdec(pat, arg)],
                    S.SEQexp(List.map genTyExp' args))
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
          (* generate an expression to write the argument according to the specified
           * type operator.
           *)
          and genTyExp (arg, E.OPTION ty) =
                S.appExp(
                  funApp ("writeOption", [tyWriter ty]),
                  pairExp(outSV, arg))
            | genTyExp (arg, E.SEQUENCE ty) =
                S.appExp(
                  funApp ("writeSeq", [tyWriter ty]),
                  pairExp(outSV, arg))
            | genTyExp (arg, E.SHARED ty) =
                S.appExp(
                  funApp ("writeShared", [tyWriter ty, GenShCxt.selector ty]),
                  pairExp(outSV, arg))
            | genTyExp (arg, E.TYP ty) = S.appExp (tyWriter ty, pairExp(outSV, arg))
          and genTyExp' (x, ty) = genTyExp (S.IDexp x, ty)
          (* the pickler's arguments *)
          val paramPat = S.TUPLEpat[outP anySharing, objP]
          (* generate the body of the writer; which may involve a conversion
           * from the natural type.
           *)
          val body = (case TyV.getUnwrapper tyId
                 of NONE => gen(S.IDexp "obj", encoding)
                  | SOME f => S.simpleLet("pkl", funApp(qId f, [S.IDexp "obj"]),
                      gen(S.IDexp "pkl", encoding))
                (* end case *))
          in
            S.funBind(Util.picklerName tyId, [([paramPat], body)])
          end

    and genReader (modId, anySharing, tyModName, tyId, encoding) = let
          (* instream variable, which is a context when we have sharing *)
          val inSV = if anySharing then cxtSV else inSV
          (* expression that defines the underlying outstream *)
          val inS = inS anySharing
          fun qId id = concat[tyModName, ".", id]
          fun getConName conId = qId(ConV.getName conId)
          fun primReader tyId = S.IDexp(concat[
                  getPklModName BT.asdlTypesId, ".", Util.unpicklerName tyId
                ])
          fun tyReader (E.BASE tyId) = if anySharing
                then S.APPexp(S.IDexp "readPrim", primReader tyId)
                else primReader tyId
            | tyReader (E.IMPORT(modId, tyId)) = S.IDexp(concat[
                  getPklModName modId, ".", Util.unpicklerName tyId
                ])
            | tyReader (E.LOCAL(_, tyId)) = S.IDexp(Util.unpicklerName tyId)
          fun genTag tagTyId = S.APPexp(primReader tagTyId, inSV)
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
            | gen (E.ALIAS ty) = genTyExp ty
          and genObj (E.TUPLE tys, k) = let
                val xs = List.mapi (fn (i, _) => "x"^Int.toString i) tys
                val decs = ListPair.map
                      (fn (x, (_, ty)) => S.VALdec(S.IDpat x, genTyExp ty))
                        (xs, tys)
                in
                  S.letExp(decs, k (S.TUPLEexp(List.map S.IDexp xs)))
                end
            | genObj (E.RECORD fields, k) = let
                val decs = List.map
                      (fn (lab, ty) => S.VALdec(S.IDpat lab, genTyExp ty))
                        fields
                val fields = List.map (fn (lab, _) => (lab, S.IDexp lab)) fields
                in
                  S.letExp(decs, k (S.RECORDexp fields))
                end
          and genTyExp (E.OPTION ty) =
                S.appExp(
                  funApp ("readOption", [tyReader ty]),
                  inS)
            | genTyExp (E.SEQUENCE ty) =
                S.appExp(
                  funApp ("readSeq", [tyReader ty]),
                  inS)
            | genTyExp (E.SHARED ty) =
                S.appExp(
                  funApp ("readShared", [tyReader ty, GenShCxt.selector ty]),
                  inS)
            | genTyExp (E.TYP ty) = S.appExp (tyReader ty, inS)
        (* generate the body of the reader; which may involve a conversion
         * to the natural type.
         *)
          val body = (case TyV.getWrapper tyId
                 of NONE => gen encoding
                  | SOME f =>
                      S.simpleLet("pkl", gen encoding, funApp(qId f, [S.IDexp "pkl"]))
                (* end case *))
          in
            S.funBind(Util.unpicklerName tyId, [([inP anySharing], body)])
          end

  end
