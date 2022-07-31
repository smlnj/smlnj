(* transtkind.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure TransTKind =
  struct

(* translation from the front-end simplified version of tkind (TKind.tkind) to
 * PLambdaType.tkind *)

    local
      structure TK = TKind   (* the "front-end" version of Lty.tkind(I) *)
      structure LD = LtyDef
    in

    (* trans : TKind.tkind -> PlambdaType.tkind *)
    fun trans (TK.TKCint i) = LD.tkc_int i
      | trans (TK.TKCfun (args,res)) = LD.tkc_fun (map trans args, trans res)
      | trans (TK.TKCseq tks) = LD.tkc_seq(map trans tks)

    end

end (* structure TransTKind *)
