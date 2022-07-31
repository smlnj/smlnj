(* typp.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *
 * A pretty-printer for ML type expressions.
 *)

use "base.sml";

datatype ty
  = VarTy of string
  | BaseTy of (ty list * string)
  | FnTy of (ty * ty)
  | TupleTy of ty list
  | RecordTy of (string * ty) list

fun ppTy (strm, ty) = let
      fun ppComma () = (PP.string strm ","; PP.space strm 1)
      fun ppStar () = (PP.space strm 1; PP.string strm "*"; PP.nbSpace strm 1)
      fun pp (VarTy s) = PP.string strm s
	| pp (BaseTy([], s) = PP.string strm s
	| pp (BaseTy([ty], s) =
	| pp (BaseTy(l, s) =
	| pp (FnTy(ty1, ty2)) =
	| pp (TupleTy []) = PP.string strm "()"
	| pp (TupleTy [ty]) = pp ty
	| pp (TupleTy l) =
	| pp (RecordTy []) = PP.string strm "{}"
	| pp (RecordTy l) = let
	    fun ppElem (lab, ty) = (
		  PP.openHVBox strm (PP.Abs 2);
		    PP.string lab;
		    PP.space strm 1;
		    PP.string strm ":";
		    PP.nbSpace strm;
		    pp ty
		  PP.closeBox())
	    in
	      PP.openHBox strm;
	        PP.string strm "{";
	        PP.openHVBox (strm, PP.Abs 4);
	          ppl (ppElem, ppComma) l;
		  PP.break strm {nsp=0, offset=2};
		PP.closeBox strm;
	        PP.string strm "}";
	      PP.closeBox strm
	    end
      and ppParenTy ty =
      and ppl (ppElem, ppSep) l = let
	    fun ppl' [] = ()
	      | ppl' [ty] = ppElem ty
	      | ppl' (ty::r) = (ppElem ty; ppSep(); ppl' r)
	    in
	      ppl' l
	    end
      in
	PP.openHOVBox (strm, PP.Abs 2);
	pp ty;
	PP.closeBox strm
      end;

local
  val stringTy = BaseTy([], "string")
  val intTy = BaseTy([], "int")
  val boolTy = BaseTy([], "bool")
  val unitTy = BaseTy([], "unit")
  val posTy = BaseTy([], "pos")
  fun optionTy arg = BaseTy([arg], "option")
  val vecBufTy = RecordTy [
	  ("buf", BaseTy([], "vector")),
	  ("i", intTy),
	  ("sz", optionTy intTy)
	]
  val arrBufTy = RecordTy [
	  ("buf", BaseTy([], "array")),
	  ("i", intTy),
	  ("sz", optionTy intTy)
	]
in
val wrTy = RecordTy of [
	("name", stringTy),
	("chunkSize", intTy),
	("writeVec", optionTy(FnTy(vecBufTy, intTy))),
	("writeArr", optionTy(FnTy(arrBufTy, intTy))),
	("writeVecNB", optionTy(FnTy(vecBufTy, optionTy intTy))),
	("writeArrNB", optionTy(FnTy(arrBufTy, optionTy intTy))),
	("block", optionTy(FnTy(unitTy, unitTy)),
	("canOutput", optionTy(FnTy(unitTy, boolTy)),
	("getPos", optionTy(FnTy(unitTy, posTy))),
	("setPos", optionTy(FnTy(posTy, unitTy))),
	("endPos", optionTy(FnTy(unitTy, posTy))),
	("verifyPos", optionTy(FnTy(unitTy, posTy))),
	("close", optionTy(FnTy(unitTy, unitTy))),
	("ioDesc", optionTy(BaseTy([], "OS.IO.iodesc")))
      ]
end;
