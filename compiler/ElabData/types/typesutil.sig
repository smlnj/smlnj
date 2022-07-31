(* typesutil.sig
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature TYPESUTIL =
sig

  val eqpropToString : Types.eqprop -> string

(* operations to build tyvars and VARtys *)
  val mkMETA : int -> Types.tvKind
  val mkFLEX : ((Symbol.symbol * Types.ty) list) * int -> Types.tvKind
  val mkUBOUND : Symbol.symbol -> Types.tvKind
  val mkMETAty : unit -> Types.ty
  val mkMETAtyBounded : int -> Types.ty

(* operations to "mark" tyvars as generalized and assign an "identifier",
 * i.e. an int, to them when generalized. Based (temporarily) on an
 * abuse of the LBOUND tyvar kind (LBOUND(0,1000+)). *)
  val markGeneralizedTyvar : Types.tyvar -> unit
  val isGeneralizedTyvar : Types.tyvar -> bool
  val generalizedTyvarId : Types.tyvar -> int option
  val resetGeneralizedTyvarCount : unit -> unit

  (* primitive operations on tycons *)
  val tycName : Types.tycon -> Symbol.symbol
  val tycStamp : Types.tycon -> Stamps.stamp
  val tycPath : Types.tycon -> InvPath.path option

  val tycEntPath : Types.tycon -> EntPath.entPath
  val tyconArity : Types.tycon -> int
  val setTycPath : Types.tycon * InvPath.path -> Types.tycon
  val eqTycon : Types.tycon * Types.tycon -> bool
  val eqDatacon : Types.datacon * Types.datacon -> bool

  val prune : Types.ty -> Types.ty
  val pruneTyvar : Types.tyvar -> Types.ty

  val eqTyvar : Types.tyvar * Types.tyvar -> bool
  val bindTyvars : Types.tyvar list -> unit
  val bindTyvars1 : Types.tyvar list -> Types.polysign
  val tyvarIsEq : Types.tyvar -> bool

  (* type reduction *)
  exception ReduceType
  val mapTypeFull: (Types.tycon -> Types.tycon) -> Types.ty -> Types.ty
  val applyTyfun : Types.tyfun * Types.ty list -> Types.ty
  val applyPoly : Types.ty * Types.ty list -> Types.ty
  val reduceType : Types.ty -> Types.ty
  val headReduceType : Types.ty -> Types.ty

  (* equality of types *)
  val equalType  : Types.ty * Types.ty -> bool
  val equalTypeP : Types.ty * Types.ty -> bool
  val equalTycon : Types.tycon * Types.tycon -> bool

(* `calcStrictness (arity, ty)` returns a list of bools of length arity,
 * where the ith element indicates whether DB index `(IBOUND i)` actually
 * occurs in ty; if not, then the type is not "strict" in that type parameter.
 *)
  val calcStrictness : int * Types.ty -> bool list

(* making a "generic" copy of a type *)
  val typeArgs : int -> Types.ty list
  val mkPolySign : int -> Types.polysign

  val dataconTyc : Types.datacon -> Types.tycon
  val dconType : Types.tycon * Types.ty option  -> Types.ty

(* get rid of INSTANTIATED indirections throughout a type *)
  val compressTy : Types.ty -> unit

  val instantiatePoly : Types.ty -> Types.ty * Types.tyvar list

  val compareTypes : Types.ty * Types.ty -> bool

  val indexBoundTyvars : int * Types.tyvar list -> unit

  val matchInstTypes : bool * int * Types.ty * Types.ty ->
                         (Types.tyvar list * Types.tyvar list) option
   (* matchInstTypes probably supercedes compareTypes, and if so,
    * compareTypes should be deleted *)

  val tyvarType : Types.ty -> Types.tyvar

  (*
   * Check if a bound tyvar has occurred in some datatypes, e.g. 'a list.
   * this is useful for representation analysis; but it should be
   * obsolete very soon -- zsh.
   *)
  val getRecTyvarMap : int * Types.ty -> (int -> bool)
  val gtLabel : Symbol.symbol * Symbol.symbol -> bool

  (* return true if there exists a value that the pattern does _not_ match *)
  val refutable: Absyn.pat -> bool

  val isValue : Absyn.exp -> bool
  (* checks whether an expression is nonexpansive; used to determine
   * when type generalization is permitted under the value rule *)
  (*
  dbm: where has this moved to? typecheck.sml?
  gk: restoring this function because PrimopId is now self-contained.
  *)
  val isVarTy : Types.ty -> bool

  val sortFields : (Absyn.numberedLabel * 'a) list
        -> (Absyn.numberedLabel * 'a) list
  val projectField : Symbol.symbol * Types.ty -> Types.ty option

  val mapUnZip : ('a -> 'b * 'c) -> 'a list -> 'b list * 'c list
  (* this is obviously a generic list utility fn, so should be in
   * a more general utility module *)

  type tycset
  val mkTycSet : unit -> tycset
  val addTycSet : Types.tycon * tycset -> tycset
  val filterSet : Types.ty * tycset -> Types.tycon list

  val dtSibling : int * Types.tycon -> Types.tycon
  val extractDcons: Types.tycon -> Types.datacon list

  val wrapDef : Types.tycon * Stamps.stamp -> Types.tycon
      (* make a tycon into a DEFtyc by "eta-expanding" if necessary *)

  val unWrapDef1 : Types.tycon -> Types.tycon option
  val unWrapDefStar : Types.tycon -> Types.tycon

  val dummyTyGen : unit -> unit -> Types.ty
      (* create dummy type generators used to instantiate ungeneralizable
       * free type variables in Typechecking.generalizeTy *)

  val tyToString : Types.ty -> string

  val numInfo : Types.ty -> { wid : int, signed : bool }
  (* return size and signedness information about integer and word types.
   * returns size (wid) zero for IntInf.int. *)

  val numInRange : IntInf.int * Types.ty -> bool
  (* check an integer/word literal value for being in range as defined by its type *)

  val dataconToTycon : Types.datacon -> Types.tycon
  (* returns the datatype tycon that the datacon belongs to *)

  val datatypeWidth : Types.tycon -> int
  (* returns the number of datacons belonging to a datatype tycon *)

  val dataconWidth : Types.datacon -> int
  (* returns the number of datacons of the datacon's owner datatype *)

  val typeVariants : Types.ty -> int
  (* "width" of the head tycon of the type argument *)

  val dataconName : Types.datacon -> Symbol.symbol
  (* the name (a symbol) of a datacon *)

  val dataconType : Types.datacon -> Types.ty
  (* returns the typ field of a datacon *)

  val dataconSign : Types.datacon -> Access.consig
  (* the "sign" of a datacon, which is the sign of its datatype *)

  val dataconIsConst : Types.datacon -> bool
  (* returns true if the datacon is a constant *)

  val vectorElemTy : Types.ty -> Types.ty
  (* given the type of a vector, returns the type of elements of the vector *)

  val replicateTy : Types.ty * int -> Types.ty list
  (* replicateTy(ty,n) returns a list of n copies of ty *)

  (* val matchPoly : ty * int * ty -> ty vector *)
  (* matchPoly(target, arity, body) matches body against target to create a vector of
   * IBOUND instantiations, where body is assumed to the body of a polymorphic type *)

  (* val instTy : ty vector -> ty -> ty *)
  (* instTy vec body: the instance vector is used to instantiate IBOUND type variables
   * in body *)

  val destructDataconTy : Types.ty * Types.datacon -> Types.ty
  (* given an instance, ty, of the range of a dcon, returns corresponding
   * instance of the domain of the dcon *)

  val destructRecordTy : Types.ty -> Types.ty list
  (* returns the field types (in canonical order) of a record type (-> void in FLINT) *)

  val dePoly : Types.ty -> Types.ty
  (* de-polymorphise a type; brutal version, instantiating bound variables to UNDEFty *)

  val dePolyVar : Variable.var -> Types.ty
  (* de-polymorphise the type of a variable *)

end  (* signature TYPESUTIL *)
