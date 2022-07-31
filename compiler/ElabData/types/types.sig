(* types.sig
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature TYPES =
sig

(* not quite abstract types... *)
type label (* = Symbol.symbol *)
type polysign (* = bool list *)

datatype eqprop = YES | NO | IND | OBJ | DATA | ABS | UNDEF

type varSource = Symbol.symbol * SourceMap.region
type litSource = IntInf.int * SourceMap.region

datatype openTvKind
  = META
  | FLEX of (label * ty) list

(* In future, may need to add real, char, string literals as new forms of
 * overload tvKinds *)
and tvKind
  = INSTANTIATED of ty
  | OPEN of {depth: int, eq: bool, kind: openTvKind}
  | UBOUND of {depth: int, eq: bool, name: Symbol.symbol}
     (* name does not include the leading apostrophy(s) *)
  | OVLDV of  (* overloaded variable (operator) *)
    {eq: bool,  (* equality attribute, may be set by unification *)
     sources: varSource list} (* names and locations of overloaded
				  variables or literals *)
     (* used to instantiate overloaded operator type scheme,
      * representing one of a finite set of possible ground types used as
      * "indicator" types to resolve the overloading *)
  | OVLDI of litSource list  (* overloaded integer literal *)
  | OVLDW of litSource list  (* overloaded word literal *)
  | LBOUND of {depth: int, eq: bool, index: int}
     (* FLINT-style de Bruijn index for notional "lambda"-bound type variables
      * associated with polymorphic bindings (including val bindings and
      * functor parameter bindings). The depth is depth of type lambda bindings,
      * (1-based), and the index is the index within a sequence of
      * type variables bound at a given binding site. LBOUNDs must carry
      * equality type information for signature matching because the OPENs
      * are turned into LBOUNDs before equality type information is matched. *)

and tycpath
  = TP_VAR of
      { tdepth: DebIndex.depth,
        num: int, kind: TKind.tkind }
  | TP_TYC of tycon
  | TP_FCT of tycpath list * tycpath list
  | TP_APP of tycpath * tycpath list
  | TP_SEL of tycpath * int

and tyckind
   = PRIMITIVE 		(* primitive tycons *)
  | ABSTRACT of tycon
  | DATATYPE of
     {index: int,			(* index for RECtyc *)
      stamps: Stamps.stamp vector,
      root : EntPath.entVar option,    (* the root field used by type spec only *)
      freetycs: tycon list,            (* tycs derived from functor params *)
      family : dtypeFamily,
      stripped : bool}                 (* true if datatype has matched a simple type spec *)
  | FLEXTYC of tycpath          (* instantiated formal type constructor *)
  | FORMAL                      (* used only inside signatures *)
  | TEMP                        (* used only during datatype elaborations *)

and tycon
  = GENtyc of gtrec
  | DEFtyc of
      {stamp : Stamps.stamp,
       tyfun : tyfun,
       strict: bool list,
       path  : InvPath.path}
  | PATHtyc of
      {arity : int,
       entPath : EntPath.entPath,
       path : InvPath.path}
  | RECORDtyc of label list
  | RECtyc of int                (* used only in domain type of dconDesc *)
  | FREEtyc of int               (* used only in domain type of dconDesc *)
  | ERRORtyc

and ty
  = VARty of tyvar
  | IBOUND of int		(* DeBruijn index; must be in a tyfun *)
  | CONty of tycon * ty list
  | POLYty of {sign: polysign, tyfun: tyfun}
  | MARKty of ty * SourceMap.region
  | WILDCARDty			(* used for avoiding cascading errors *)
  | UNDEFty			(* pre-type-checking type *)

and tyfun
  = TYFUN of {arity : int, body : ty}

(* datacon description used in dtmember *)
withtype dconDesc =
    {name: Symbol.symbol,
     rep: Access.conrep,
     domain: ty option}

(* member of a family of (potentially) mutually recursive datatypes *)
and dtmember =
    {tycname: Symbol.symbol,
     arity: int,
     eq: eqprop ref,
     lazyp : bool,
     dcons: dconDesc list,
     sign: Access.consig}

and dtypeFamily =
    {mkey: Stamps.stamp,
     members: dtmember vector,
     properties: PropList.holder}

and stubinfo =
    {owner : PersStamps.persstamp,
     lib   : bool}

(* The "stub" field will be set for any GENtyc that comes out of the
 * unpickler.  The stub owner pid is the pid of the compilation unit on whose
 * behalf this GENtyc was pickled.  Normally, this is expected to be the
 * same as the pid in the (global) "stamp", but there are odd cases related
 * to recursive types where this assumption breaks.  (Is there a way of
 * fixing this? -M.) *)
and gtrec =
    {stamp : Stamps.stamp,
     arity : int,
     eq    : eqprop ref,
     kind  : tyckind,
     path  : InvPath.path,
     stub  : stubinfo option}

and tyvar = tvKind ref

val infinity : int
val mkTyvar  : tvKind -> tyvar
val copyTyvar : tyvar -> tyvar

datatype datacon                    (* data constructors *)
  = DATACON of
      {name   : Symbol.symbol,
       typ    : ty,
       rep    : Access.conrep,
       lazyp  : bool,            (* LAZY *)
       const  : bool,
       sign   : Access.consig}

end (* signature TYPES *)
