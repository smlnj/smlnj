(* COPYRIGHT (c) 1997 YALE FLINT PROJECT *)
(* lty.sig *)

signature LTY = sig 

(* definitions of kind and kind-environment *)
type tkind

datatype tkindI
  = TK_MONO                                   (* ground mono tycon *)
  | TK_BOX				      (* boxed/tagged tycon *)
  | TK_SEQ of tkind list                      (* sequence of tycons *)
  | TK_FUN of tkind list * tkind              (* tycon function *)

(* enc_tvar: encoded type variables = deBruijn indexes * bind-arity indexes *)
type enc_tvar
val tvEncode : int * int -> enc_tvar
val tvDecode : enc_tvar -> int * int

(* definitions of named tyc variables *)
type tvar = LambdaVar.lvar                    (* temporary, not used *)
val mkTvar : unit -> tvar                     (* temporary, not used *)

(* internal information in hash-consed tycs *)
datatype aux_info
  = AX_REG of bool                      (* normalization flag *)
            * enc_tvar list             (* free debruijn-indexed type vars *)
            * tvar list                 (* free named type vars *)
  | AX_NO                               (* no aux_info available *)

(* definitions of tyc and tyc-environment *)
type tyc

(* tycEnv is a sequence of tycEnvElems representing a substitution or
 * type environment produced by lazy beta-reductions or pushing through
 * a lambda abstraction. It is encoded as a tyc so that it can be hash-consed
 * using the tyc hash consing mechanism. 
 *)
type tycEnv 

(* tycEnvs are represented by an encoding as tycs. The abstract representation
 * of tycEnvs would be given by:
 *
 *   datatype teBinder
 *     = Beta of int * tyc list * tkind list
 *     | Lamb of int * tkind list
 *
 *   type tycEnv = teBinder list
 *
 * Invariant: a tycEnv cannot terminate with a Lamb, i.e. the last binder
 *   in a tycEnv must be a Beta. tycEnvs are created when a closure is created
 *   when reducing a beta-redex (rule r1), and they are always initially of
 *   of the form Beta(0,args,ks)::nil.
 *)
             
datatype teBinder
  = Beta of int * tyc list * tkind list
      (* Beta(j,args,ks):
         created when reducing a beta redex (r1);
         j: the embedding level of the original redex -- 0 if the redex was
            created by r1, or the nesting level of the new closure if by r12;
         args: the tycs bound by the n-ary beta reduction, i.e. the arguments;
         ks: the operator domain kinds *)
  | Lamb of int * tkind list
      (* Lamb(j,ks):
         created when pushing a closure (Env) through a lambda (r10);
         j: the nesting level of the closure just before r10 is applied,
            i.e. the nesteing level of the abstraction relative to the
            point where the closure was originally created;
         ks: the kinds of the abstraction parameters *)

val teToBinders : tycEnv -> teBinder list

datatype fflag                                (* function "calling conventions" *)
  = FF_VAR of bool * bool                     (* arg/result representations known? *)
  | FF_FIXED                                  (* used after rep. analysis *)

datatype tycI
  = TC_VAR of DebIndex.index * int            (* tyc variable *)
  | TC_NVAR of tvar                           (* named tyc variable *)
  | TC_PRIM of PrimTyc.primtyc                (* primitive tyc *)
  | TC_FN of tkind list * tyc                 (* tyc abstraction *)
  | TC_APP of tyc * tyc list                  (* tyc application *)
  | TC_SEQ of tyc list                        (* tyc sequence *)
  | TC_PROJ of tyc * int                      (* tyc projection *)

  | TC_SUM of tyc list                        (* sum tyc *)
  | TC_FIX of {family: {size: int,            (* recursive tyc *) 
                        names: string vector,
                        gen : tyc,
                        params : tyc list},
               index: int}

  | TC_TUPLE of tyc list              (* std record tyc *)
  | TC_ARROW of fflag * tyc list * tyc list   (* std function tyc *)
  | TC_PARROW of tyc * tyc                    (* special fun tyc, not used *)

  | TC_BOX of tyc                             (* boxed tyc *)
  | TC_WRAP of tyc                            (* wrapped tyc *)
  | TC_CONT of tyc list                       (* std continuation tyc *)
  | TC_IND of tyc * tycI                      (* indirect tyc thunk *)
  | TC_ENV of tyc * int * int * tycEnv        (* tyc closure *)

(* definition of lty (hashed) and ltyI (internal, raw) *)
type lty

datatype ltyI          
  = LT_TYC of tyc                             (* monomorphic type *)  
  | LT_STR of lty list                        (* structure type *)
  | LT_FCT of lty list * lty list             (* functor type *)
  | LT_POLY of tkind list * lty list          (* polymorphic type *)
    
  | LT_CONT of lty list                       (* internal cont type *)
  | LT_IND of lty * ltyI                      (* indirect type thunk *)
  | LT_ENV of lty * int * int * tycEnv        (* type closure *)

(* unknown and wrap_is_whnm *)
val unknown : tyc -> bool
val wrap_is_whnm : tyc -> bool

(** injections and projections on tkind, tyc, and lty *)
val tk_inj : tkindI -> tkind 
val tc_inj : tycI -> tyc
val lt_inj : ltyI -> lty

val tk_out : tkind -> tkindI
val tc_out : tyc -> tycI
val lt_out : lty -> ltyI

(** key comparison for tkind, tyc, and lty; used in pickling *)
val tk_cmp : tkind * tkind -> order
val tc_cmp : tyc * tyc -> order
val lt_cmp : lty * lty -> order

(** get the hash key of each lty, used by reps/coerce.sml; a hack! *)
val lt_key   : lty -> int

(** utility functions on tycEnv *)

val teEmpty : tycEnv
val teCons : teBinder * tycEnv -> tycEnv
val teDest : tycEnv -> (teBinder * tycEnv) option
val teLookup : tycEnv * int -> teBinder option
val teLength : tycEnv -> int

(** utility functions on tkindEnv *)
type tkindEnv 
exception tkUnbound
val initTkEnv        : tkindEnv
val tkLookup         : tkindEnv * int * int -> tkind
val tkInsert         : tkindEnv * tkind list -> tkindEnv

(* simple equality operations *)
val tk_eq : tkind * tkind -> bool
val tc_eq : tyc * tyc -> bool
val lt_eq : lty * lty -> bool

(** updating tycs and ltys *)
val tyc_upd : tyc * tyc -> unit
val lty_upd : lty * lty -> unit

(** testing if a tyc or lty is in normal form *)
val tcp_norm : tyc -> bool
val ltp_norm : lty -> bool

(** accessing free deBruijn tyvars *)
val tc_vs : tyc -> enc_tvar list option
val lt_vs : lty -> enc_tvar list option

(** accessing free named tyvars *)
val tc_nvars : tyc -> tvar list
val lt_nvars : lty -> tvar list

end (* signature LTY *)
