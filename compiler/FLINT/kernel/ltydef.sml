(* Copyright (c) 1997 YALE FLINT PROJECT *)
(* ltydef.sml *)

structure LtyDef : LTYDEF =
struct

local
  structure PT = PrimTyc
  structure DI = DebIndex
  structure LT = Lty
  structure LK = LtyKernel


  (* debugging *)
  structure PP = PrettyPrint
  structure PU = PPUtil
  structure EM = ErrorMsg

  fun bug msg = ErrorMsg.impossible("LtyDef: "^msg)

  val with_pp = PP.with_default_pp
  val debugging : bool ref = ref false
  val dp : int ref = Control_Print.printDepth

in

(*
 * FLINT fflag is used to classify different kinds of monomorphic
 * functions and records. As of now, they are defined in Lty as:
 *
 *    datatype fflag
 *      = FF_VAR of bool * bool
 *      | FF_FIXED
 *
 * We treat both as abstract types so pattern matching no longer applies.
 * NOTE: FF_VAR flags are used by FLINTs before we perform representation
 * analysis while FF_FIXED is used by FLINTs after we perform representation
 * analysis.
 *)

(** fflag constructors *)
val ffc_var    : bool * bool -> LT.fflag = fn x => LT.FF_VAR x
val ffc_fixed  : LT.fflag = LT.FF_FIXED

(** fflag deconstructors *)
val ffd_var    : LT.fflag -> bool * bool = fn x =>
      (case x of LT.FF_VAR x => x | _ => bug "unexpected fflag in ffd_var")
val ffd_fixed  : LT.fflag -> unit = fn x =>
      (case x of LT.FF_FIXED => () | _ => bug "unexpected fflag in ffd_fixed")

(** fflag predicates *)
val ffp_var    : LT.fflag -> bool = fn x =>
      (case x of LT.FF_VAR _ => true | _ => false)
val ffp_fixed  : LT.fflag -> bool = fn x =>
      (case x of LT.FF_FIXED => true | _ => false)

(** fflag one-arm switch *)
fun ffw_var (ff, f, g) =
      (case ff of LT.FF_VAR x => f x | _ => g ff)
fun ffw_fixed (ff, f, g) =
      (case ff of LT.FF_FIXED => f () | _ => g ff)


(** utility functions for tkinds. Moved from Lty to LtyDef. **)

(* tkSubkind returns true if k1 is a subkind of k2, or if they are
 * equivalent kinds.  it is NOT commutative.  tksSubkind is the same
 * thing, component-wise on lists of kinds.
 * NOTE: TK_MONO and TK_BOX cannot be distinguished -- they are each a
 *   subkind of the other.  [Is TK_BOX superfluous?]
 *)
fun tksSubkind (ks1, ks2) =
    ListPair.all tkSubkind (ks1, ks2)   (* component-wise *)

and tkSubkind (k1, k2) =
    LT.tk_eq (k1, k2) orelse              (* reflexive *)
    case (LT.tk_out k1, LT.tk_out k2) of
        (LT.TK_BOX, LT.TK_MONO) => true (* ground kinds (base case) *)
      (* this next case is WRONG, but necessary until the
       * infrastructure is there to give proper boxed kinds to
       * certain tycons (e.g., ref : Omega -> Omega_b)
       *)
      | (LT.TK_MONO, LT.TK_BOX) => true
      | (LT.TK_SEQ ks1, LT.TK_SEQ ks2) =>
          tksSubkind (ks1, ks2)
      | (LT.TK_FUN (ks1, k1'), LT.TK_FUN (ks2, k2')) =>
          tksSubkind (ks2, ks1) andalso (* contravariant *)
          tkSubkind (k1', k2')
      | _ => false

(** tkind constructors *)
val tkc_mono   : LT.tkind = LT.tk_inj (LT.TK_MONO)
val tkc_box    : LT.tkind = LT.tk_inj (LT.TK_BOX)  (* only appears in opt/specialize.sml *)
val tkc_seq    : LT.tkind list -> LT.tkind = LT.tk_inj o LT.TK_SEQ
val tkc_fun    : LT.tkind list * LT.tkind -> LT.tkind = LT.tk_inj o LT.TK_FUN

(** utility functions for constructing tkinds *)
fun tkc_arg n = List.tabulate (n, (fn _ => tkc_mono))

fun tkc_int 0 = tkc_mono
  | tkc_int i = tkc_fun(tkc_arg i, tkc_mono)

(* is a kind monomorphic? True only for TK_MONO and TK_BOX*)
fun tkIsMono k = tkSubkind (k, tkc_mono)

(*
 * FLINT tyc is roughly equivalent to the following ML datatype
 *
 *    datatype tyc
 *      = TC_VAR of index * int  (* DeBruijn style (DB depth, position in tuple) *)
 *      | TC_NVAR of tvar        (* "explicit" type variables with TFN bindings, debnames *)
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
 *      | TC_ARROW of fflag * tyc list * tyc list
 *
 * We treat tyc as an abstract type so we can no longer use
 * pattern matching. Type applications (TC_APP) and projections
 * (TC_PROJ) are automatically reduced as needed, that is, the
 * client does not need to worry about whether a tyc is in the
 * normal form or not, all functions defined here automatically
 * take care of this.
 *)

(** tyc constructors *)
val tcc_var    : DI.index * int -> LT.tyc = LT.tc_inj o LT.TC_VAR
val tcc_nvar   : Lty.tvar -> LT.tyc = LT.tc_inj o LT.TC_NVAR
val tcc_prim   : PT.primtyc -> LT.tyc = LT.tc_inj o LT.TC_PRIM
val tcc_fn     : LT.tkind list * LT.tyc -> LT.tyc = LT.tc_inj o LT.TC_FN
val tcc_app    : LT.tyc * LT.tyc list -> LT.tyc = LT.tc_inj o LT.TC_APP
val tcc_seq    : LT.tyc list -> LT.tyc = LT.tc_inj o LT.TC_SEQ
val tcc_proj   : LT.tyc * int -> LT.tyc = LT.tc_inj o LT.TC_PROJ
val tcc_sum    : LT.tyc list -> LT.tyc = LT.tc_inj o LT.TC_SUM
val tcc_fix    : (int * string vector * LT.tyc * LT.tyc list) * int -> LT.tyc =
    fn ((s,ns,g,p),i) =>
       LT.tc_inj(LT.TC_FIX{family={size=s,names=ns,gen=g,params=p},index=i})
val tcc_wrap   : LT.tyc -> LT.tyc = fn tc => LT.tc_inj (LT.TC_WRAP tc)
val tcc_box    : LT.tyc -> LT.tyc = LT.tc_inj o LT.TC_BOX
val tcc_tuple  : LT.tyc list -> LT.tyc = fn ts => LT.tc_inj (LT.TC_TUPLE ts)

(** tyc deconstructors *)
val tcd_var    : LT.tyc -> DI.index * int = fn tc =>
      (case LK.tc_whnm_out tc of LT.TC_VAR x => x
                       | _ => bug "unexpected tyc in tcd_var")
val tcd_nvar   : LT.tyc -> Lty.tvar = fn tc =>
      (case LK.tc_whnm_out tc of LT.TC_NVAR x => x
                       | _ => bug "unexpected tyc in tcd_nvar")
val tcd_prim   : LT.tyc -> PT.primtyc = fn tc =>
      (case LK.tc_whnm_out tc of LT.TC_PRIM x => x
                       | _ => bug "unexpected tyc in tcd_prim")
val tcd_fn     : LT.tyc -> LT.tkind list * LT.tyc = fn tc =>
      (case LK.tc_whnm_out tc of LT.TC_FN x => x
                       | _ => bug "unexpected tyc in tcd_fn")
val tcd_app    : LT.tyc -> LT.tyc * LT.tyc list = fn tc =>
      (case LK.tc_whnm_out tc of LT.TC_APP x => x
                       | _ => bug "unexpected tyc in tcd_app")
val tcd_seq    : LT.tyc -> LT.tyc list = fn tc =>
      (case LK.tc_whnm_out tc of LT.TC_SEQ x => x
                       | _ => bug "unexpected tyc in tcd_seq")
val tcd_proj   : LT.tyc -> LT.tyc * int = fn tc =>
      (case LK.tc_whnm_out tc of LT.TC_PROJ x => x
                       | _ => bug "unexpected tyc in tcd_proj")
val tcd_sum    : LT.tyc -> LT.tyc list = fn tc =>
      (case LK.tc_whnm_out tc of LT.TC_SUM x => x
                       | _ => bug "unexpected tyc in tcd_sum")
val tcd_fix    : LT.tyc -> (int * LT.tyc * LT.tyc list) * int = fn tc =>
      (case LK.tc_whnm_out tc of LT.TC_FIX{family={size,names,gen,params},index} =>
                           ((size,gen,params),index)
                       | _ => bug "unexpected tyc in tcd_fix")
val tcd_wrap   : LT.tyc -> LT.tyc = fn tc =>
      (case LK.tc_whnm_out tc
        of LT.TC_WRAP x => x
         | _ => bug "unexpected regular LT.tyc in tcd_wrap")
val tcd_box    : LT.tyc -> LT.tyc = fn tc =>
      (case LK.tc_whnm_out tc of LT.TC_BOX x => x
                       | _ => bug "unexpected tyc in tcd_box")
val tcd_tuple  : LT.tyc -> LT.tyc list = fn tc =>
      (case LK.tc_whnm_out tc of LT.TC_TUPLE x => x
                       | _ => bug "unexpected tyc in tcd_tuple")
val tcd_arrow  : LT.tyc -> LT.fflag * LT.tyc list * LT.tyc list = fn tc =>
      (case LK.tc_whnm_out tc of LT.TC_ARROW x => x
                       | _ => bug "unexpected tyc in tcd_arrow")

(** LT.tyc predicates *)
val tcp_var    : LT.tyc -> bool = fn tc =>
      (case LK.tc_whnm_out tc of LT.TC_VAR _ => true | _ => false)
val tcp_nvar   : LT.tyc -> bool = fn tc =>
      (case LK.tc_whnm_out tc of LT.TC_NVAR _ => true | _ => false)
val tcp_prim   : LT.tyc -> bool = fn tc =>
      (case LK.tc_whnm_out tc of LT.TC_PRIM _ => true | _ => false)
val tcp_fn     : LT.tyc -> bool = fn tc =>
      (case LK.tc_whnm_out tc of LT.TC_FN _ => true | _ => false)
val tcp_app    : LT.tyc -> bool = fn tc =>
      (case LK.tc_whnm_out tc of LT.TC_APP _ => true | _ => false)
val tcp_seq    : LT.tyc -> bool = fn tc =>
      (case LK.tc_whnm_out tc of LT.TC_SEQ _ => true | _ => false)
val tcp_proj   : LT.tyc -> bool = fn tc =>
      (case LK.tc_whnm_out tc of LT.TC_PROJ _ => true | _ => false)
val tcp_sum    : LT.tyc -> bool = fn tc =>
      (case LK.tc_whnm_out tc of LT.TC_SUM _ => true | _ => false)
val tcp_fix    : LT.tyc -> bool = fn tc =>
      (case LK.tc_whnm_out tc of LT.TC_FIX _ => true | _ => false)
val tcp_wrap   : LT.tyc -> bool = fn tc =>
      (case LK.tc_whnm_out tc of LT.TC_WRAP _ => true | _ => false)
val tcp_box    : LT.tyc -> bool = fn tc =>
      (case LK.tc_whnm_out tc of LT.TC_BOX _ => true | _ => false)
val tcp_tuple  : LT.tyc -> bool = fn tc =>
      (case LK.tc_whnm_out tc of LT.TC_TUPLE _ => true | _ => false)
val tcp_arrow  : LT.tyc -> bool = fn tc =>
      (case LK.tc_whnm_out tc of LT.TC_ARROW _ => true | _ => false)

(** tyc one-arm switches *)
fun tcw_var (tc, f, g) =
      (case LK.tc_whnm_out tc of LT.TC_VAR x => f x | _ => g tc)
fun tcw_nvar (tc, f, g) =
      (case LK.tc_whnm_out tc of LT.TC_NVAR x => f x | _ => g tc)
fun tcw_prim (tc, f, g) =
      (case LK.tc_whnm_out tc of LT.TC_PRIM x => f x | _ => g tc)
fun tcw_fn (tc, f, g) =
      (case LK.tc_whnm_out tc of LT.TC_FN x => f x | _ => g tc)
fun tcw_app (tc, f, g) =
      (case LK.tc_whnm_out tc of LT.TC_APP x => f x | _ => g tc)
fun tcw_seq (tc, f, g) =
      (case LK.tc_whnm_out tc of LT.TC_SEQ x => f x | _ => g tc)
fun tcw_proj (tc, f, g) =
      (case LK.tc_whnm_out tc of LT.TC_PROJ x => f x | _ => g tc)
fun tcw_sum (tc, f, g) =
      (case LK.tc_whnm_out tc of LT.TC_SUM x => f x | _ => g tc)
fun tcw_fix (tc, f, g) =
      (case LK.tc_whnm_out tc
        of LT.TC_FIX{family={size,names,gen,params},index} => f((size,gen,params),index)
         | _ => g tc)
fun tcw_wrap (tc, f, g) =
      (case LK.tc_whnm_out tc
        of LT.TC_WRAP x => f x
         | _ => g tc)
fun tcw_box (tc, f, g) =
      (case LK.tc_whnm_out tc of LT.TC_BOX x => f x | _ => g tc)
fun tcw_tuple (tc, f, g) =
      (case LK.tc_whnm_out tc of LT.TC_TUPLE x => f x | _ => g tc)
fun tcw_arrow (tc, f, g) =
      (case LK.tc_whnm_out tc of LT.TC_ARROW x => f x | _ => g tc)


(*
 * FLINT lty is roughly equivalent to the following ML datatype
 *
 *    datatype lty
 *      = LT_TYC of tyc       -- coerce a tyc to an lty (tyc subset lty)
 *      | LT_STR of lty list  -- "structure"
 *      | LT_FCT of lty list * lty list    -- "functor"
 *      | LT_POLY of tkind list * lty list
 *
 * We treat lty as an abstract type so we can no longer use pattern
 * matching. The client does not need to worry about whether an lty
 * is in normal form or not.
 *)

(** lty constructors *)
val ltc_tyc    : LT.tyc -> LT.lty = LT.lt_inj o LT.LT_TYC
val ltc_str    : LT.lty list -> LT.lty = LT.lt_inj o LT.LT_STR
val ltc_fct    : LT.lty list * LT.lty list -> LT.lty = LT.lt_inj o LT.LT_FCT
val ltc_poly   : LT.tkind list * LT.lty list -> LT.lty = LT.lt_inj o LT.LT_POLY

exception DeconExn

(** lty deconstructors *)
val ltd_tyc    : LT.lty -> LT.tyc = fn lt =>
      (case LK.lt_whnm_out lt of LT.LT_TYC x => x
		       | LT.LT_ENV _ => bug "unexpected lty in ltd_tyc (i.e. LT_ENV)"
		       | LT.LT_STR _ => bug "unexpected LT_STR"
		       | LT.LT_FCT _ => bug "unexpected LT_FCT"
		       | LT.LT_POLY _ => (PrettyPrint.with_default_pp(fn s => PPLty.ppLty 10 s lt);
					  raise DeconExn;
					  bug "unexpected LT_POLY")
		       | LT.LT_CONT _ => bug "unexpected LT_CONT"
		       | LT.LT_IND _ => bug "unexpected LT_IND"
                       (*| _ => bug "unexpected lty in ltd_tyc" *))
val ltd_str    : LT.lty -> LT.lty list = fn lt =>
      (case LK.lt_whnm_out lt of LT.LT_STR x => x
                       | _ => bug "unexpected lty in ltd_str")
val ltd_fct    : LT.lty -> LT.lty list * LT.lty list = fn lt =>
      (case LK.lt_whnm_out lt of LT.LT_FCT x => x
                       | _ => bug "unexpected lty in ltd_fct")
val ltd_poly   : LT.lty -> LT.tkind list * LT.lty list = fn lt =>
    (case LK.lt_whnm_out lt
      of LT.LT_POLY x => x
       | _ => (with_pp(fn s =>
			  let val {break,newline,openHVBox,openHOVBox,openVBox,
				   closeBox, pps, ppi} = PU.en_pp s
			  in openHVBox 0;
                          pps "***ltd_poly***"; break{nsp=1,offset=0};
                          pps "arg:"; newline();
                          PPLty.ppLty (!dp) s lt; break{nsp=1,offset=0};
                          closeBox ()
			  end);
	       bug "unexpected lty in ltd_poly"))

(** lty predicates *)
val ltp_tyc    : LT.lty -> bool = fn lt =>
      (case LK.lt_whnm_out lt of LT.LT_TYC _ => true | _ => false)
val ltp_str    : LT.lty -> bool = fn lt =>
      (case LK.lt_whnm_out lt of LT.LT_STR _ => true | _ => false)
val ltp_fct    : LT.lty -> bool = fn lt =>
      (case LK.lt_whnm_out lt of LT.LT_FCT _ => true | _ => false)
val ltp_poly   : LT.lty -> bool = fn lt =>
      (case LK.lt_whnm_out lt of LT.LT_POLY _ => true | _ => false)

(** lty one-arm switches *)
fun ltw_tyc (lt, f, g) =
      (case LK.lt_whnm_out lt of LT.LT_TYC x => f x | _ => g lt)
fun ltw_str (lt, f, g) =
      (case LK.lt_whnm_out lt of LT.LT_STR x => f x | _ => g lt)
fun ltw_fct (lt, f, g) =
      (case LK.lt_whnm_out lt of LT.LT_FCT x => f x | _ => g lt)
fun ltw_poly (lt, f, g) =
      (case LK.lt_whnm_out lt of LT.LT_POLY x => f x | _ => g lt)


(*
 * Because FLINT tyc is embedded inside FLINT lty, we supply the
 * the following utility functions on building ltys that are based
 * on simple mono tycs.
 *)

(** tyc-lty constructors *)
val ltc_var    : DI.index * int -> LT.lty = ltc_tyc o tcc_var
val ltc_prim   : PT.primtyc -> LT.lty = ltc_tyc o tcc_prim
val ltc_tuple  : LT.lty list -> LT.lty =
    ltc_tyc o
    (tcc_tuple o (map (fn x => ltd_tyc x handle DeconExn => bug "ltc_tuple")))
val ltc_arrow  : LT.fflag * LT.lty list * LT.lty list -> LT.lty =
    fn (r, t1, t2) =>
       let val ts1 = map ltd_tyc t1
	   val ts2 = map ltd_tyc t2
	in ltc_tyc (LK.tcc_arrow(r, ts1, ts2))
       end handle DeconExn => bug "ltc_arrow"

(** tyc-lty deconstructors *)
val ltd_var    : LT.lty -> DI.index * int =
    tcd_var o (fn x => ltd_tyc x handle DeconExn => bug "ltd_var")
val ltd_prim   : LT.lty -> PT.primtyc =
    tcd_prim o (fn x => ltd_tyc x handle DeconExn => bug "ltd_prim")
val ltd_tuple  : LT.lty -> LT.lty list =
    (map ltc_tyc) o (tcd_tuple o (fn x => ltd_tyc x handle DeconExn => bug "ltd_tuple"))
val ltd_arrow  : LT.lty -> LT.fflag * LT.lty list * LT.lty list =
    fn t =>
       let val (r, ts1, ts2) = tcd_arrow (ltd_tyc t)
	in (r, map ltc_tyc ts1, map ltc_tyc ts2)
       end (* handle DeconExn => bug "ltd_arrow" *)

(** tyc-lty predicates *)
val ltp_var    : LT.lty -> bool = fn t =>
  (case LK.lt_whnm_out t of LT.LT_TYC x => tcp_var x | _ => false)
val ltp_prim   : LT.lty -> bool = fn t =>
  (case LK.lt_whnm_out t of LT.LT_TYC x => tcp_prim x | _ => false)
val ltp_tuple  : LT.lty -> bool = fn t =>
  (case LK.lt_whnm_out t of LT.LT_TYC x => tcp_tuple x | _ => false)
val ltp_arrow  : LT.lty -> bool = fn t =>
  (case LK.lt_whnm_out t of LT.LT_TYC x => tcp_arrow x | _ => false)

(** tyc-lty one-arm switches *)
fun ltw_var (lt, f, g) =
  (case LK.lt_whnm_out lt
    of LT.LT_TYC tc =>
         (case LK.tc_whnm_out tc of LT.TC_VAR x => f x | _ => g lt)
     | _ => g lt)

fun ltw_prim (lt, f, g) =
  (case LK.lt_whnm_out lt
    of LT.LT_TYC tc =>
         (case LK.tc_whnm_out tc of LT.TC_PRIM x => f x | _ => g lt)
     | _ => g lt)

fun ltw_tuple (lt, f, g) =
  (case LK.lt_whnm_out lt
    of LT.LT_TYC tc =>
         (case LK.tc_whnm_out tc of LT.TC_TUPLE x => f x | _ => g lt)
     | _ => g lt)

fun ltw_arrow (lt, f, g) =
  (case LK.lt_whnm_out lt
    of LT.LT_TYC tc =>
         (case LK.tc_whnm_out tc of LT.TC_ARROW x => f x | _ => g lt)
     | _ => g lt)


(*
 * The following functions are written for CPS only. If you are writing
 * writing code for FLINT, you should not use any of these functions.
 * The continuation referred here is the internal continuation introduced
 * via CPS conversion; it is different from the source-level continuation
 * ('a cont) or control continuation ('a control-cont) where are represented
 * as PT.ptc_cont and PT.ptc_ccont respectively.
 *
 *)

(** cont-tyc-lty constructors *)
val tcc_cont   : LT.tyc list -> LT.tyc = LT.tc_inj o LT.TC_CONT
val ltc_cont   : LT.lty list -> LT.lty = LT.lt_inj o LT.LT_CONT

(** cont-tyc-lty deconstructors *)
val tcd_cont   : LT.tyc -> LT.tyc list = fn tc =>
      (case LK.tc_whnm_out tc of LT.TC_CONT x => x
                       | _ => bug "unexpected tyc in tcd_cont")
val ltd_cont   : LT.lty -> LT.lty list = fn lt =>
      (case LK.lt_whnm_out lt of LT.LT_CONT x => x
                       | _ => bug "unexpected lty in ltd_cont")

(** cont-tyc-lty predicates *)
val tcp_cont   : LT.tyc -> bool = fn tc =>
      (case LK.tc_whnm_out tc of LT.TC_CONT _ => true | _ => false)
val ltp_cont   : LT.lty -> bool = fn lt =>
      (case LK.lt_whnm_out lt of LT.LT_CONT _ => true | _ => false)

(** cont-tyc-lty one-arm switches *)
fun tcw_cont (tc, f, g) =
      (case LK.tc_whnm_out tc of LT.TC_CONT x => f x | _ => g tc)
fun ltw_cont (lt, f, g) =
      (case LK.lt_whnm_out lt of LT.LT_CONT x => f x | _ => g lt)



(*
 * The following functions are written for LtyExtern (PLambdaType) only. If you
 * are writing code for FLINT only, don't use any of these functions.
 * The idea is that in PLambda, all (value or type) functions have single
 * argument and single return-result. Ideally, we should define
 * another sets of datatypes for tycs and ltys. But we want to avoid
 * the translation from LtyExtern (PLambdaType) to FLINT types, so we let them
 * share the same representations as much as possible.
 *
 * Ultimately, LtyDef should be separated into two files: one for
 * FLINT, another for PLambda, but we will see if this is necessary.
 * [DBM, 9/19/21: there are no separate FLINT types, so Lty ... LtyExtern serve
 * for both PLambda and FLINT.
 *)

(*
 * The implementation here is TEMPORARY; Stefan needs to take a look at
 * this. Note parrow could share the representation with arrow if there
 * is one-to-one mapping between parrow and arrow.
 *)

(** plambda tyc-lty constructors *)
val tcc_parrow : LT.tyc * LT.tyc -> LT.tyc =
  fn (x, y) => LK.tcc_arrow(ffc_var (false, false), [x], [y])
val ltc_parrow : LT.lty * LT.lty -> LT.lty =
  fn (x, y) => ltc_tyc (tcc_parrow (ltd_tyc x, ltd_tyc y)) handle DeconExn => bug "ltc_parrow"
val ltc_ppoly  : LT.tkind list * LT.lty -> LT.lty = fn (ks, t) => ltc_poly(ks, [t])
val ltc_pfct   : LT.lty * LT.lty -> LT.lty = fn (x, y) => ltc_fct ([x], [y])

(** plambda tyc-lty deconstructors *)
val tcd_parrow : LT.tyc -> LT.tyc * LT.tyc = fn tc =>
  (case LK.tc_whnm_out tc
    of LT.TC_ARROW (_, xs, ys) => (LK.tc_autotuple xs, LK.tc_autotuple ys)
     | _ => bug "unexpected tyc in tcd_parrow")
val ltd_parrow : LT.lty -> LT.lty * LT.lty = fn t =>
  let val (t1, t2) = tcd_parrow (ltd_tyc t)
   in (ltc_tyc t1, ltc_tyc t2)
  end handle DeconExn => bug "ltd_parrow"
val ltd_ppoly  : LT.lty -> LT.tkind list * LT.lty = fn t =>
  let val (ks, ts) = ltd_poly t
   in case ts of [x] => (ks, x)
               | _ => bug "unexpected case in ltd_ppoly"
  end
val ltd_pfct   : LT.lty -> LT.lty * LT.lty = fn t =>
  let val (ts1, ts2) = ltd_fct t
   in case (ts1, ts2) of ([x], [y]) => (x, y)
                       | _ => bug "unexpected case in ltd_pfct"
  end

(** plambda tyc-lty predicates *)
val tcp_parrow : LT.tyc -> bool = fn tc =>
  (case LK.tc_whnm_out tc of LT.TC_ARROW (_, [x], [y]) => true | _ => false)
val ltp_parrow : LT.lty -> bool = fn t =>
  (case LK.lt_whnm_out t of LT.LT_TYC x => tcp_parrow x | _ => false)
val ltp_ppoly  : LT.lty -> bool = fn t =>
  (case LK.lt_whnm_out t of LT.LT_POLY (_, [x]) => true | _ => false)
val ltp_pfct   : LT.lty -> bool = fn t =>
  (case LK.lt_whnm_out t of LT.LT_FCT ([x], [y]) => true | _ => false)

(** plambda tyc-lty one-arm switches *)
fun tcw_parrow (tc, f, g) =
  (case LK.tc_whnm_out tc of LT.TC_ARROW (_,[x],[y]) => f (x,y) | _ => g tc)
fun ltw_parrow (lt, f, g) =
  (case LK.lt_whnm_out lt
    of LT.LT_TYC tc =>
         (case LK.tc_whnm_out tc of LT.TC_ARROW (_,[x],[y]) => f(x,y) | _ => g lt)
     | _ => g lt)
fun ltw_ppoly (lt, f, g) =
  (case LK.lt_whnm_out lt of LT.LT_POLY(ks,[x]) => f(ks,x) | _ => g lt)
fun ltw_pfct (lt, f, g) =
  (case LK.lt_whnm_out lt of LT.LT_FCT([x],[y]) => f(x,y) | _ => g lt)

end (* top-level local *)
end (* structure LtyDef *)
