(* cfg-util.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure CFGUtil : sig

  (* does a cluster contain any raw C calls? *)
    val hasRCC : CFG.cluster -> bool

    val tyToString : CFG.ty -> string

    val compareTy : CFG.ty * CFG.ty -> order

  end = struct

    structure C = CFG

    fun hasRCC (C.Cluster{attrs={hasRCC, ...},...}) = hasRCC

    fun tyToString (CFG.NUMt{sz}) = "i" ^ Int.toString sz
      | tyToString (CFG.FLTt{sz}) = "f" ^ Int.toString sz
      | tyToString CFG.PTRt = "ptr"
      | tyToString CFG.LABt = "label"
      | tyToString CFG.TAGt = "int"

    fun compareTy (CFG.NUMt{sz}, CFG.NUMt{sz=sz'}) = Int.compare(sz, sz')
      | compareTy (CFG.NUMt _, _) = LESS
      | compareTy (_, CFG.NUMt _) = GREATER
      | compareTy (CFG.FLTt{sz}, CFG.FLTt{sz=sz'}) = Int.compare(sz, sz')
      | compareTy (CFG.FLTt _, _) = LESS
      | compareTy (_, CFG.FLTt _) = GREATER
      | compareTy (CFG.PTRt, CFG.PTRt) = EQUAL
      | compareTy (_, CFG.PTRt) = LESS
      | compareTy (CFG.PTRt, _) = GREATER
      | compareTy (CFG.LABt, CFG.LABt) = EQUAL
      | compareTy (CFG.LABt, _) = LESS
      | compareTy (_, CFG.LABt) = GREATER
      | compareTy (CFG.TAGt, CFG.TAGt) = EQUAL

  end
