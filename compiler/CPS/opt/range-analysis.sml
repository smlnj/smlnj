(* range-analysis.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure RangeAnalysis : sig

    type t

    val analyze : CPS.function -> t

  end = struct

    structure C = CPS

  end
