(* Copyright (c) 1998 YALE FLINT PROJECT *)
(* ltydef.sig *)

(*
 * This interface hides the implementation details of FLINT tkind, tyc, and 
 * lty defined inside Lty. For each entity, we provide a series of 
 * constructor funtions, deconstructor functions, predicate functions, and
 * functions that test equivalence and do pretty-printing. This interface 
 * should only refer to DebIndex, Lty, PrimTyc, and Symbol. 
 *)

signature LTYDEF = 
sig

(** basic entities *)


(* FLINT fflags used to classify different kinds of monomorphic 
 * functions. The fflags datatype is defined in Lty.
 *
 * We treat both as abstract types so pattern matching no longer applies.
 * NOTE: FF_VAR flags are used by FLINTs before we perform representation
 * analysis while FF_FIXED is used by FLINTs after we perform representation
 * analysis. 
 *)

(** fflag constructors *)
val ffc_var    : bool * bool -> Lty.fflag
val ffc_fixed  : Lty.fflag

(** fflag deconstructors *)
val ffd_var    : Lty.fflag -> bool * bool
val ffd_fixed  : Lty.fflag -> unit

(** fflag predicates *)
val ffp_var    : Lty.fflag -> bool
val ffp_fixed  : Lty.fflag -> bool

(** fflag one-arm switch *)
val ffw_var    : Lty.fflag * (bool * bool -> 'a) * (Lty.fflag -> 'a) -> 'a
val ffw_fixed  : Lty.fflag * (unit -> 'a) * (Lty.fflag -> 'a) -> 'a


(* FLINT tkind is the hash-consed version of the following ML datatype:
 *
 *    datatype tkind 
 *      = TK_MONO 
 *      | TK_BOX
 *      | TK_SEQ of tkind list
 *      | TK_FUN of tkind * tkind
 *
 * We treat tkind as an abstract type so we can no longer use 
 * pattern matching, so we provide constructors, destructors, predicates,
 * and one-armed switches for the type. We also defined a "subkind"
 * relation on tkinds.
 *)


(** tkind constructors *)
val tkc_mono   : Lty.tkind
val tkc_box    : Lty.tkind
val tkc_seq    : Lty.tkind list -> Lty.tkind
val tkc_fun    : Lty.tkind list * Lty.tkind -> Lty.tkind

(** subkind relation **)
val tksSubkind : Lty.tkind list * Lty.tkind list -> bool
val tkSubkind : Lty.tkind * Lty.tkind -> bool

(** utility functions for constructing tkinds *)
val tkc_int : int -> Lty.tkind
val tkc_arg : int -> Lty.tkind list

(* is a kind monomorphic? *)
val tkIsMono : Lty.tkind -> bool

(* 
 * FLINT tyc is roughly equivalent to the following ML datatype 
 *
 *    datatype tyc
 *      = TC_VAR of index * int
 *      | TC_NVAR of tvar
 *      | TC_PRIM of primtyc
 *      | TC_FN of tkind list * tyc
 *      | TC_APP of tyc * tyc list
 *      | TC_SEQ of tyc list
 *      | TC_PROJ of tyc * int
 *      | TC_SUM of tyc list
 *      | TC_FIX of tyc * int
 *      | TC_WRAP of tyc                   (* used after rep. analysis only *)
 *      | TC_BOX of tyc                    (* NOT USED *)
 *      | TC_TUPLE of tyc list
 *      | TC_ARROW of Lty.fflag * tyc list * tyc list 
 *
 * We treat tyc as an abstract type so we can no longer use 
 * pattern matching. Type applications (TC_APP) and projections 
 * (TC_PROJ) are automatically reduced as needed, that is, the
 * client does not need to worry about whether a tyc is in the
 * normal form or not, all functions defined here automatically 
 * take care of this.
 *)

(** tyc constructors *)
val tcc_var    : DebIndex.index * int -> Lty.tyc
val tcc_nvar   : Lty.tvar -> Lty.tyc
val tcc_prim   : PrimTyc.primtyc -> Lty.tyc
val tcc_fn     : Lty.tkind list * Lty.tyc -> Lty.tyc
val tcc_app    : Lty.tyc * Lty.tyc list -> Lty.tyc
val tcc_seq    : Lty.tyc list -> Lty.tyc
val tcc_proj   : Lty.tyc * int -> Lty.tyc
val tcc_sum    : Lty.tyc list -> Lty.tyc
val tcc_fix    : (int * string vector * Lty.tyc * Lty.tyc list) * int -> Lty.tyc 
val tcc_wrap   : Lty.tyc -> Lty.tyc
val tcc_box    : Lty.tyc -> Lty.tyc
val tcc_tuple  : Lty.tyc list -> Lty.tyc

(** tyc deconstructors *)
val tcd_var    : Lty.tyc -> DebIndex.index * int 
val tcd_nvar   : Lty.tyc -> Lty.tvar
val tcd_prim   : Lty.tyc -> PrimTyc.primtyc 
val tcd_fn     : Lty.tyc -> Lty.tkind list * Lty.tyc 
val tcd_app    : Lty.tyc -> Lty.tyc * Lty.tyc list 
val tcd_seq    : Lty.tyc -> Lty.tyc list 
val tcd_proj   : Lty.tyc -> Lty.tyc * int 
val tcd_sum    : Lty.tyc -> Lty.tyc list 
val tcd_fix    : Lty.tyc -> (int * Lty.tyc * Lty.tyc list) * int 
val tcd_wrap   : Lty.tyc -> Lty.tyc
val tcd_box    : Lty.tyc -> Lty.tyc 
val tcd_tuple  : Lty.tyc -> Lty.tyc list 
val tcd_arrow  : Lty.tyc -> Lty.fflag * Lty.tyc list * Lty.tyc list 

(** tyc predicates *)
val tcp_var    : Lty.tyc -> bool
val tcp_nvar   : Lty.tyc -> bool
val tcp_prim   : Lty.tyc -> bool
val tcp_fn     : Lty.tyc -> bool
val tcp_app    : Lty.tyc -> bool
val tcp_seq    : Lty.tyc -> bool
val tcp_proj   : Lty.tyc -> bool
val tcp_sum    : Lty.tyc -> bool
val tcp_fix    : Lty.tyc -> bool
val tcp_wrap   : Lty.tyc -> bool
val tcp_box    : Lty.tyc -> bool
val tcp_tuple  : Lty.tyc -> bool
val tcp_arrow  : Lty.tyc -> bool

(** tyc one-arm switch *)
val tcw_var    : Lty.tyc * (DebIndex.index * int -> 'a) * (Lty.tyc -> 'a) -> 'a
val tcw_nvar   : Lty.tyc * (Lty.tvar -> 'a) * (Lty.tyc -> 'a) -> 'a
val tcw_prim   : Lty.tyc * (PrimTyc.primtyc -> 'a) * (Lty.tyc -> 'a) -> 'a
val tcw_fn     : Lty.tyc * (Lty.tkind list * Lty.tyc -> 'a) * (Lty.tyc -> 'a) -> 'a
val tcw_app    : Lty.tyc * (Lty.tyc * Lty.tyc list -> 'a) * (Lty.tyc -> 'a) -> 'a
val tcw_seq    : Lty.tyc * (Lty.tyc list -> 'a) * (Lty.tyc -> 'a) -> 'a
val tcw_proj   : Lty.tyc * (Lty.tyc * int -> 'a) * (Lty.tyc -> 'a) -> 'a
val tcw_sum    : Lty.tyc * (Lty.tyc list -> 'a) * (Lty.tyc -> 'a) -> 'a
val tcw_fix    : Lty.tyc * ((int * Lty.tyc * Lty.tyc list) * int -> 'a) * (Lty.tyc -> 'a) -> 'a
val tcw_wrap   : Lty.tyc * (Lty.tyc -> 'a) * (Lty.tyc -> 'a) -> 'a
val tcw_box    : Lty.tyc * (Lty.tyc -> 'a) * (Lty.tyc -> 'a) -> 'a
val tcw_tuple  : Lty.tyc * (Lty.tyc list -> 'a) * (Lty.tyc -> 'a) -> 'a
val tcw_arrow  : Lty.tyc * (Lty.fflag * Lty.tyc list * Lty.tyc list -> 'a) 
                     * (Lty.tyc -> 'a) -> 'a
                                      

(* FLINT lty (Lty.lty) is the hash-consed version of the datatype ltyI.
 * lty is treated as an abstract type so we can not use pattern
 * matching on lty. Clients do not need to worry about whether an lty
 * is in normal form or not (when?). *)

(** lty constructors *)
val ltc_tyc    : Lty.tyc -> Lty.lty
val ltc_str    : Lty.lty list -> Lty.lty
val ltc_fct    : Lty.lty list * Lty.lty list -> Lty.lty
val ltc_poly   : Lty.tkind list * Lty.lty list -> Lty.lty    

exception DeconExn

(** lty deconstructors *)
val ltd_tyc    : Lty.lty -> Lty.tyc
val ltd_str    : Lty.lty -> Lty.lty list
val ltd_fct    : Lty.lty -> Lty.lty list * Lty.lty list
val ltd_poly   : Lty.lty -> Lty.tkind list * Lty.lty list

(** lty predicates *)
val ltp_tyc    : Lty.lty -> bool
val ltp_str    : Lty.lty -> bool
val ltp_fct    : Lty.lty -> bool
val ltp_poly   : Lty.lty -> bool

(** lty one arm switches *)
val ltw_tyc    : Lty.lty * (Lty.tyc -> 'a) * (Lty.lty -> 'a) -> 'a
val ltw_str    : Lty.lty * (Lty.lty list -> 'a) * (Lty.lty -> 'a) -> 'a
val ltw_fct    : Lty.lty * (Lty.lty list * Lty.lty list -> 'a) * (Lty.lty -> 'a) -> 'a
val ltw_poly   : Lty.lty * (Lty.tkind list * Lty.lty list -> 'a) * (Lty.lty -> 'a) -> 'a
                                        

(* Because FLINT tyc is embedded inside FLINT lty, we supply the
 * the following utility functions on building ltys that are based
 * on simple mono tycs. *)

(** tyc-lty constructors *)
val ltc_var    : DebIndex.index * int -> Lty.lty
val ltc_prim   : PrimTyc.primtyc -> Lty.lty
val ltc_tuple  : Lty.lty list -> Lty.lty
val ltc_arrow  : Lty.fflag * Lty.lty list * Lty.lty list -> Lty.lty

(** tyc-lty deconstructors *)
val ltd_var    : Lty.lty -> DebIndex.index * int
val ltd_prim   : Lty.lty -> PrimTyc.primtyc
val ltd_tuple  : Lty.lty -> Lty.lty list
val ltd_arrow  : Lty.lty -> Lty.fflag * Lty.lty list * Lty.lty list

(** tyc-lty predicates *)
val ltp_var    : Lty.lty -> bool
val ltp_prim   : Lty.lty -> bool
val ltp_tuple  : Lty.lty -> bool
val ltp_arrow  : Lty.lty -> bool

(** tyc-lty one-arm switches *)
val ltw_var    : Lty.lty * (DebIndex.index * int -> 'a) * (Lty.lty -> 'a) -> 'a
val ltw_prim   : Lty.lty * (PrimTyc.primtyc -> 'a) * (Lty.lty -> 'a) -> 'a
val ltw_tuple  : Lty.lty * (Lty.tyc list -> 'a) * (Lty.lty -> 'a) -> 'a
val ltw_arrow  : Lty.lty * (Lty.fflag * Lty.tyc list * Lty.tyc list -> 'a) 
                     * (Lty.lty -> 'a) -> 'a


(* The following functions are written for CPS only. If you are writing
 * code for FLINT, you should not use any of these functions. 
 * The continuation referred here is the internal continuation introduced
 * via CPS conversion; it is different from the source-level continuation 
 * ('a cont) or control continuation ('a control-cont) where are represented 
 * as PT.ptc_cont and PT.ptc_ccont respectively. *)

(** cont-tyc-lty constructors *)
val tcc_cont   : Lty.tyc list -> Lty.tyc      
val ltc_cont   : Lty.lty list -> Lty.lty                

(** cont-tyc-lty deconstructors *)
val tcd_cont   : Lty.tyc -> Lty.tyc list      
val ltd_cont   : Lty.lty -> Lty.lty list        

(** cont-tyc-lty predicates *)
val tcp_cont   : Lty.tyc -> bool          
val ltp_cont   : Lty.lty -> bool            

(** cont-tyc-lty one-arm switches *)
val tcw_cont   : Lty.tyc * (Lty.tyc list -> 'a) * (Lty.tyc -> 'a) -> 'a
val ltw_cont   : Lty.lty * (Lty.lty list -> 'a) * (Lty.lty -> 'a) -> 'a


(* 
 * The following functions are written for LtyExtern (PLambdaType) only. If you
 * are writing code for FLINT only, don't use any of these functions. 
 * The idea is that in PLambda, all (value or type) functions have single
 * argument and single return-result. Ideally, we should define 
 * another set of datatypes for tycs and ltys. But we want to avoid
 * the translation from LtyExtern (PLambdaType) to FLINT types, so we let them
 * share the same representations as much as possible. 
 * [DBM, 2020.10] PLambda types and FLINT types are the same. No translation needed.
 * -- there may be a difference in "argument-results" conventions, say PLambda-style
 * and FLINT-style types.
 *)

(** plambda tyc-lty constructors *)
val tcc_parrow : Lty.tyc * Lty.tyc -> Lty.tyc     
val ltc_parrow : Lty.lty * Lty.lty -> Lty.lty
val ltc_ppoly  : Lty.tkind list * Lty.lty -> Lty.lty  
val ltc_pfct   : Lty.lty * Lty.lty -> Lty.lty         

(** plambda tyc-lty deconstructors *)
val tcd_parrow : Lty.tyc -> Lty.tyc * Lty.tyc
val ltd_parrow : Lty.lty -> Lty.lty * Lty.lty    
val ltd_ppoly  : Lty.lty -> Lty.tkind list * Lty.lty
val ltd_pfct   : Lty.lty -> Lty.lty * Lty.lty       

(** plambda tyc-lty predicates *)
val tcp_parrow : Lty.tyc -> bool          
val ltp_parrow : Lty.lty -> bool
val ltp_ppoly  : Lty.lty -> bool
val ltp_pfct   : Lty.lty -> bool            

(** plambda tyc-lty one-arm switches *)
val tcw_parrow : Lty.tyc * (Lty.tyc * Lty.tyc -> 'a) * (Lty.tyc -> 'a) -> 'a
val ltw_parrow : Lty.lty * (Lty.tyc * Lty.tyc -> 'a) * (Lty.lty -> 'a) -> 'a
val ltw_ppoly  : Lty.lty * (Lty.tkind list * Lty.lty -> 'a) * (Lty.lty -> 'a) -> 'a
val ltw_pfct   : Lty.lty * (Lty.lty * Lty.lty -> 'a) * (Lty.lty -> 'a) -> 'a

end (* signature LTYDEF *)
