(* cfg-util.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure CFGUtil : sig

    (* does a cluster contain any raw C calls? *)
    val hasRCC : CFG.cluster -> bool

    val tyToString : CFG.ty -> string

    val compareTy : CFG.ty * CFG.ty -> order

    (* `rawSelect (ty, idx, arg)` builds a CFG expression for selecting
     * a raw number from a raw or mixed record.  The `ty` argument specifies
     * the kind and size of the field, `idx` is the slot index, and `arg` is
     * the expression denoting the record.
     * NOTE: this code assumes that values occupy a full 64-bit slot; even
     * if their type is smaller.
     *)
    val rawSelect : CFG_Prim.raw_ty * int * CFG.exp -> CFG.exp

  end = struct

    structure C = CFG
    structure P = CFG_Prim

    fun hasRCC (C.Cluster{attrs={hasRCC, ...},...}) = hasRCC

    fun tyToString (C.NUMt{sz}) = "i" ^ Int.toString sz
      | tyToString (C.FLTt{sz}) = "f" ^ Int.toString sz
      | tyToString C.PTRt = "ptr"
      | tyToString C.LABt = "label"
      | tyToString C.TAGt = "int"

    fun compareTy (C.NUMt{sz}, C.NUMt{sz=sz'}) = Int.compare(sz, sz')
      | compareTy (C.NUMt _, _) = LESS
      | compareTy (_, C.NUMt _) = GREATER
      | compareTy (C.FLTt{sz}, C.FLTt{sz=sz'}) = Int.compare(sz, sz')
      | compareTy (C.FLTt _, _) = LESS
      | compareTy (_, C.FLTt _) = GREATER
      | compareTy (C.PTRt, C.PTRt) = EQUAL
      | compareTy (_, C.PTRt) = LESS
      | compareTy (C.PTRt, _) = GREATER
      | compareTy (C.LABt, C.LABt) = EQUAL
      | compareTy (C.LABt, _) = LESS
      | compareTy (_, C.LABt) = GREATER
      | compareTy (C.TAGt, C.TAGt) = EQUAL

    (* select from a raw or mixed record *)
    fun rawSelect ({kind, sz}, idx, r) = let
          val offset = 8 * idx
          val oper = C.PURE{
                  oper = P.RAW_SELECT{kind=kind, sz=64, offset=offset},
                  args = [r]
                }
          in
            case kind
             of P.INT => if (sz = 64)
                  then oper
                  else C.PURE{oper = P.TRUNC{from=64, to=sz}, args = [oper]}
              | P.FLT => oper
            (* end case *)
          end

  end
