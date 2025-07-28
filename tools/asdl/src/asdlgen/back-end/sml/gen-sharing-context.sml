(* gen-sharing-context.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * Generate a structure that defines the representation of the sharing
 * context.  This module is only generated when the ASDL specification
 * includes shared values (i.e., specified by the `!` type operation)
 * and output is not suppressed.
 *)

structure GenSharingContext : sig

    val gen : (AST.ModuleId.t * AST.TypeId.t) list -> SML.top_decl

    (* return the name of the field holding sharing information for the given type. *)
    val fldName : Encoding.ty -> string

    (* return the field selector as an expression; i.e., `#fld_name` *)
    val selector : Encoding.ty -> SML.exp

    val contextStructName : unit -> string
    val wrContextTy : SML.ty -> SML.ty
    val rdContextTy : SML.ty -> SML.ty

  end = struct

    structure V = SMLView
    structure ModV = V.Module
    structure TyV = V.Type
    structure S = SML

    fun mkRecordTyDec (tyParams, name, fields) =
          S.TYPEdec(tyParams, name, S.RECORDty fields)

    val toLower = CharVector.map Char.toLower

    (* make the name of the field for the given type *)
    fun mkFieldName' (modName, tyName) = [toLower modName, "_", toLower tyName]
    val mkFieldName = concat o mkFieldName'

    (* compare two qualified types *)
    fun compare ((mId1, tyId1), (mId2, tyId2)) = (
          case AST.ModuleId.compare(mId1, mId2)
           of EQUAL => AST.TypeId.compare(tyId1, tyId2)
            | order => order
          (* end case *))

    (* type parameters *)
    val inTV = "'inS"
    val outTV = "'outS"

    (* the top-level declarations for the read context.  This has the form
     *
     * ```
     *    type 'inS rd = {
     *        inS : 'inS,
     *        mod1_ty1 : Mod1.ty1 list ref,
     *        ...
     *        modn_tyn : Modn.tyn list ref
     *      }
     *    fun 'inS mk (inS : 'inS) : 'inS rd = {
     *            inS = inS,
     *            mod1_ty1 = ref [],
     *            ...
     *            modn_tyn = ref []
     *          }
     * ```
     *
     * where the `modi_tyi` fields correspond to the shared types.
     *)
    fun genRdTopDecls fields = let
          val inTy = S.VARty inTV
          (* the context type for reading a pickle *)
          val rdContextTyDec = mkRecordTyDec (
                [inTV], "rd",
                ("inS", inTy)
                  :: List.map (fn (f, ty) => (f, S.refTy(S.listTy ty))) fields)
          fun mkFieldInit (f, _) = (f, S.APPexp(S.IDexp "ref", S.LISTexp[]))
          val body = S.RECORDexp(("inS", S.IDexp "inS") :: List.map mkFieldInit fields)
          val mkFunDec = S.FUNdec([inTV], [
                  S.FB("mk",
                    [([S.typedIdPat("inS", inTy)], body)],
                    SOME(S.CONty([inTy], "t")))
                ])
          in
            [ rdContextTyDec, mkFunDec ]
          end

    (* the top-level declarations for the write context.  This has the form
     *
     * ```
     *    type 'outS wr = {
     *        outS : 'outS,
     *        mod1_ty1 : Mod1.ty1 ASDLShareMap.t,
     *        ...
     *        modn_tyn : Modn.tyn ASDLShareMap.t
     *      }
     *    type init_wr = {
     *        mod1_ty1 : unit -> Mod1.ty1 ASDLShareMap.t,
     *        ...
     *        modn_tyn : unit -> Modn.tyn ASDLShareMap.t
     *      }
     *    fun 'outS mk (init : init_wr) (outS : 'outS) : 'outS wr = {
     *            outS = outS,
     *            mod1_ty1 = #mod1_ty1(),
     *            ...
     *            modn_tyn = #modn_tyn()
     *          }
     * ```
     *
     * where the `modi_tyi` fields correspond to the shared types.
     *)
    fun genWrTopDecls fields = let
          val outTy = S.VARty outTV
          fun shareMapTy ty = S.CONty([ty], "ASDLShareMap.t")
          (* the context type for writing a pickle *)
          val wrContextTyDec = mkRecordTyDec (
                [outTV], "t",
                ("outS", outTy)
                  :: List.map (fn (f, ty) => (f, shareMapTy ty)) fields)
          val wrInitTyDec = mkRecordTyDec (
                [], "init_wr",
                List.map (fn (f, ty) => (f, S.FUNty(S.unitTy, shareMapTy ty))) fields)
          fun mkFieldInit (f, _) =
                (f, S.APPexp(S.selectExp(f, S.IDexp "init"), S.unitExp))
          val body = S.RECORDexp(("outS", S.IDexp "outS") :: List.map mkFieldInit fields)
          val mkFunDec = S.FUNdec([outTV], [
                  S.FB("mk",
                    [(
                      [S.typedIdPat("init", S.CONty([], "init_wr")),
                       S.typedIdPat("outS", outTy)],
                      body)],
                    SOME(S.CONty([outTy], "t")))
                ])
          in
            [ wrContextTyDec, wrInitTyDec, mkFunDec ]
          end

    fun gen sharedTys = let
          val {pickler, unpickler, ...} = V.File.getSuppress ()
          val sharedTys = ListMergeSort.uniqueSort compare sharedTys
          val fields = let
                fun mkField (modId, tyId) = let
                      val modName = ModV.getName modId
                      val tyName = TyV.getName tyId
                      val ty = S.CONty([], concat[modName, ".", tyName])
                      in
                        (mkFieldName (modName, tyName), ty)
                      end
                in
                  List.map mkField sharedTys
                end
          (* generate the top-level declarations for reading *)
          fun genRdContext () = if unpickler then [] else genRdTopDecls fields
          (* generate the top-level declarationse for writing *)
          fun genWrContext () = if pickler then [] else genWrTopDecls fields
          val contextStructName = V.File.getShareContextName()
          in
            S.STRtop(contextStructName, NONE,
              S.BASEstr(genRdContext() @ genWrContext()))
          end

    fun fldName ty = let
          val (modId, tyId) = Encoding.qualNameOf ty
          in
            mkFieldName (ModV.getName modId, TyV.getName tyId)
          end

    fun selector ty = let
          val (modId, tyId) = Encoding.qualNameOf ty
          in
            SML.IDexp(
              concat("#" :: mkFieldName' (ModV.getName modId, TyV.getName tyId)))
          end

    fun contextStructName () = V.File.getShareContextName()

    fun wrContextTy ty = S.CONty([ty], contextStructName() ^ ".wr")

    fun rdContextTy ty = S.CONty([ty], contextStructName() ^ ".rd")

  end
