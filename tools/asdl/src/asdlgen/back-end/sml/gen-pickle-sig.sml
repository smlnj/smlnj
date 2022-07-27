(* gen-pickle-sig.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Generate the common pickler signatures.  These have the format
 *
 *	signature <MSIG>_PICKLE =
 *	  sig
 *	    type instream
 *	    type outstream
 *	    ...
 *	    val write_<type> : outstream * <qualified-type> -> unit
 *	    val read_<type> : instream -> <qualified-type>
 *          ...
 *	  end
 *
 * where
 *      <MSIG>			-- is the signature name for the ASDL module
 *	<type>			-- is the name of an ASDL type in the module
 *	<qualified-type>	-- is the qualified type for the ASDL
 *				   type
 *)

structure GenPickleSig : sig

  (* generate the signature for the pickler structure *)
    val gen : AST.module -> SML.top_decl

  end = struct

    structure V = SMLView
    structure ModV = V.Module
    structure TyV = V.Type
    structure S = SML

    val streamSpecs = [
	    S.TYPEspec(false, [], "instream", NONE),
	    S.TYPEspec(false, [], "outstream", NONE)
	  ]

    val outStrmTy = S.CONty([], "outstream")
    val inStrmTy = S.CONty([], "instream")
    val unitTy = S.CONty([], "unit")

    fun gen (AST.Module{id, decls, ...}) = let
	  val typeModName = ModV.getName id
(* TODO: move Util.sigName to SML view *)
	  val sigName = Util.sigName(ModV.getPickleSigName id, NONE)
	  val specs = List.foldr (genSpec typeModName) [] (!decls)
	  in
	    S.SIGtop(sigName, S.BASEsig(streamSpecs @ specs))
	  end

  (* generate the encoder/decoder specifications for a type *)
    and genSpec modName (AST.TyDcl{id, ...}, specs) = let
	  val ty = S.CONty([], concat[modName, ".", TyV.getNaturalType id])
	(* writer *)
	  val wrTy = S.FUNty(S.TUPLEty[outStrmTy, ty], unitTy)
	  val wrSpc = S.VALspec(Util.picklerName id, wrTy)
	(* reader *)
	  val rdTy = S.FUNty(inStrmTy, ty)
	  val rdSpc = S.VALspec(Util.unpicklerName id, rdTy)
	  in
	    wrSpc :: rdSpc :: specs
	  end

  end
