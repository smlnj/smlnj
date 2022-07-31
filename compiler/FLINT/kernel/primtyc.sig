(* primtyc.sig
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature PRIM_TYC =
  sig

    eqtype primtyc

  (** the primitive type constructors *)
    val ptc_num    : int -> primtyc
    val ptc_int    : primtyc	(* = ptc_num Target.defaultIntSz *)
    val ptc_real   : primtyc
(* REAL32:
    val ptc_real32 : primtyc
    val ptc_real64 : primtyc
*)
    val ptc_string : primtyc
    val ptc_exn    : primtyc

    val ptc_array  : primtyc
    val ptc_vector : primtyc
    val ptc_ref    : primtyc

    val ptc_cont   : primtyc
    val ptc_ccont  : primtyc
    val ptc_arrow  : primtyc

    val ptc_obj    : primtyc
    val ptc_pointer : primtyc	(* raw runtime-system pointer; include C functions *)
    val ptc_barray : primtyc
    val ptc_rarray : primtyc
    val ptc_slock  : primtyc

  (* ptc_void and pct_etag do not correspond to "real" primitive types (from BasisTypes) *)
    val ptc_void   : primtyc
    val ptc_etag   : primtyc

  (*
   * val ptc_boxed  : primtyc
   * val ptc_tgd    : primtyc
   * val ptc_utgd   : primtyc
   * val ptc_tnsp   : primtyc
   * val ptc_dyn    : primtyc
   *)

  (** misc utility functions on primtyc *)
    val pt_arity   : primtyc -> int
    val pt_print   : primtyc -> string

  (** hash-consing each prim tyc *)
    val pt_toint   : primtyc -> int
    val pt_fromint : int -> primtyc
    val pt_fromtyc : Types.tycon -> primtyc

  (** equality of primtycs *)
    val pt_eq : primtyc * primtyc -> bool

  (** primitive real types *)
    val realPrimTyc : int -> primtyc

  (** extract size of number/real type (or NONE) *)
    val numSize : primtyc -> int option
    val realSize : primtyc -> int option

  (** check the boxity of values of each prim tyc; returns true if the primitive
   ** type has a non-uniform unboxed representation (e.g., reals)
   *)
    val unboxed : primtyc -> bool

  (** return true if an update to a ref/array of this type can be safely
   ** treated as an unboxed update (i.e., no store list record).
   **)
    val ubxupd : primtyc -> bool

    val isvoid : primtyc -> bool

  end (* signature PRIM_TYC *)
