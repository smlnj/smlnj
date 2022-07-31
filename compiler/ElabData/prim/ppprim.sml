(* ppprim.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure PPPrim =
struct

local
  structure PP = PrettyPrint
  structure PU = PPUtil
  open PPUtil
in

fun ppPrim ppstrm prim =
    let val pps = PU.pps ppstrm
    in (case prim
	  of PrimopId.NonPrim => pps "<NonPrim>"
	   | PrimopId.Prim prim => (pps "<PrimE "; pps(PrimopBind.nameOf prim); pps ">"))
    end (* function ppPrim *)

fun ppStrPrimInfo ppstrm strPrimInfo =
    let val {openHOVBox, closeBox, pps, ...} = en_pp ppstrm
	fun ppStrPrimElem ppstrm (PrimopId.PrimE p) = ppPrim ppstrm p
	  | ppStrPrimElem ppstrm (PrimopId.StrE ps) = ppStrPrimInfo ppstrm ps
    in
	ppSequence ppstrm
         {sep = fn ppstrm => (PP.string ppstrm ", ";
                              PP.break ppstrm {nsp=1, offset=0}),
          pr = (fn _ => fn elem =>
                           (openHOVBox 1;
                            pps "(";
                            ppStrPrimElem ppstrm elem;
                            pps ")";
                            closeBox())),
          style = INCONSISTENT}
	 strPrimInfo
    end (* function ppStrPrimInfo *)

end (* local *)

end (* structure PPPrim *)
