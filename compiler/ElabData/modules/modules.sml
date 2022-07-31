(* modules.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Modules : MODULES =
struct

local structure S  = Symbol
      structure SP = SymPath
      structure IP = InvPath
      structure EP = EntPath
      structure ST = Stamps
      structure T = Types
      structure A = Access
      structure E = Env
in

(* -------------------- signature-related definitions -------------------- *)

type sharespec = SP.path list  (* only internal sharing *)

datatype Signature
  = SIG of sigrec
  | ERRORsig

(*
 * 1. tyc spec should only be GENtyc, with FORMAL or DATATYPE tyckinds, or DEFtyc.
 * 2. the stamp and the path for the GENtyc or DEFtyc should be meaningless
 *    (but the stamps are in fact used for relativization of withtype bodies and
 *     the datacon domains of datatype repl specs)
 * 3. if VALspec and CONspec are using typspec instead of T.ty, then
 *    the whole thing can be further cleaned up.
 *)
and spec
  = TYCspec of {entVar : EP.entVar, info: tycSpecInfo}
  | STRspec of {entVar : EP.entVar, sign : Signature,
		def : (strDef * int) option, slot : int}
  | FCTspec of {entVar : EP.entVar, sign : fctSig, slot : int}
  | VALspec of {spec : T.ty, slot : int}
  | CONspec of {spec : T.datacon, slot : int option}

(* there are two forms of TYCspec. One for regular, explicitly defined signatures,
 * and the other for inferred signatures, where all the type info is always in the
 * realization. But we need some info for printing in the one case where a
 * realization is not available with the signature, namely an inferred result
 * signature for a functor. *)
and tycSpecInfo
  = RegTycSpec of {spec : T.tycon, repl: bool, scope: int} (* normal signature *)
  | InfTycSpec of {name: S.symbol, arity: int} (* inferred signature *)

and fctSig
  = FSIG of {kind     : S.symbol option,
	     paramsig : Signature,
	     paramvar : EP.entVar,
	     paramsym : S.symbol option,
	     bodysig  : Signature}
  | ERRORfsig

and extDef
  = TYCdef of
      {path : SymPath.path,
       tyc : T.tycon,
       relative : bool} (* does tyc contain entity paths *)
  | STRdef of SP.path * strDef

and strDef
  = CONSTstrDef of Structure  (* constant *)
  | VARstrDef of Signature * EP.entPath (* relative *)

(* ------------------------- structures and functors ---------------------- *)

and Structure
  = STR of strrec
  | STRSIG of {sign: Signature, entPath : EP.entPath}
  | ERRORstr

and Functor
  = FCT of fctrec
  | ERRORfct

(* ----------------------- entity-related definitions -------------------- *)

and entity (* elements of a entityEnv *)
  = TYCent of tycEntity
  | STRent of strEntity
  | FCTent of fctEntity
  | ERRORent
       (* no entities for val, con, exn, but this may change [? DBM] *)

and fctClosure (* realization for functors *)
  = CLOSURE of {param : EP.entVar, body : strExp, env : entityEnv}

and stampExp
  = (* CONST of ST.stamp  (* an existing stamp *)
  | *) GETSTAMP of strExp
  | NEW                (* generate a new stamp *)

and tycExp (* expression evaluating to a TYCentity *)
  = VARtyc of EP.entPath    (* selection from cur-EE *)
  | CONSTtyc of T.tycon     (* actual tycon *)
  | FORMtyc of T.tycon      (* formal tycon *)

(* entity expressions for structures *)
and strExp
  = VARstr of EP.entPath       (* variable (assumed bound in current entityEnv) *)
  | CONSTstr of strEntity      (* an existing strEntity as "constant" *)
  | STRUCTURE of {stamp : stampExp, entDec : entityDec}
  | APPLY of fctExp * strExp   (* entity-level functor application *)
      (* the arg strExp contains coercions to match the fct param sig *)
  | LETstr of entityDec * strExp  (* entity-level let *)
  | ABSstr of Signature * strExp  (* shortcut for abstraction matching (explain?) *)
  | FORMstr of fctSig             (* formal functor _body_ structure *)
  | CONSTRAINstr of {boundvar : EP.entVar, raw : strExp, coercion: strExp}
      (* similar to LETstr(M.STRdec(boundvar, strExp), coercion),
       * but with special treatment of rpath propagation to support
       * accurate type names in functor results where the functor has
       * a result signature constraint. *)

(* entity expressions for functors *)
and fctExp
  = VARfct of EP.entPath   (* variable (assumed bound in current entityEnv) *)
  | CONSTfct of fctEntity  (* an existing functor entity as "constant" *)
  | LAMBDA of {param : EP.entVar, body : strExp}  (* base form of functor abstraction *)
  | LAMBDA_TP of {param : EP.entVar, body : strExp, sign : fctSig} (* _TP ??? *)
  | LETfct of entityDec * fctExp  (* entity-level let expression for functor entity *)

(* general entity expressions *)
and entityExp
  = TYCexp of tycExp
  | STRexp of strExp
  | FCTexp of fctExp
  | DUMMYexp
  | ERRORexp

(* entity declarations *)
and entityDec
  = TYCdec of EP.entVar * tycExp
  | STRdec of EP.entVar * strExp * S.symbol
  | FCTdec of EP.entVar * fctExp
  | SEQdec of entityDec list
  | LOCALdec of entityDec * entityDec
  | ERRORdec
  | EMPTYdec

and entityEnv
  = MARKeenv of envrec
  | BINDeenv of entity EP.EvDict.map * entityEnv
  | NILeenv
  | ERReenv

(* linkage information for compilation units (pickling) *)
and modtree
  = TYCNODE of Types.gtrec
  | SIGNODE of sigrec
  | STRNODE of strrec
  | FCTNODE of fctrec
  | ENVNODE of envrec
  | BRANCH of modtree list

withtype stubinfo =
    {owner : PersStamps.persstamp,
     lib   : bool,
     tree  : modtree}

and elements = (S.symbol * spec) list

and sigrec =
    {stamp      : ST.stamp,
     name       : S.symbol option,
     closed     : bool,
     fctflag    : bool,
     elements   : elements,
     properties : PropList.holder, (* FLINT/trans *)
     typsharing : sharespec list,
     strsharing : sharespec list,
     stub       : stubinfo option}

and envrec =
    {stamp : ST.stamp,
     env   : entityEnv,
     stub  : stubinfo option}

and strEntity =
    {stamp    : ST.stamp,
     entities : entityEnv,
     properties: PropList.holder, (* FLINT/trans *)
     rpath    : IP.path,
     stub     : stubinfo option}

and strrec =
    {sign   : Signature,
     rlzn   : strEntity,
     access : A.access,
     prim   : PrimopId.str_prim_info}

and fctEntity =
    {stamp    : ST.stamp,
     closure  : fctClosure,
     properties: PropList.holder, (* FLINT/trans *)
     tycpath  : T.tycpath option,
     rpath    : IP.path,
     stub     : stubinfo option}

and fctrec =
    {sign   : fctSig,
     rlzn   : fctEntity,
     access : A.access,
     prim   : PrimopId.str_prim_info}

(* the stamp and arith inside T.tycon are critical *)
and tycEntity = T.tycon

val bogusStrStamp = ST.special "bogusStr"
val bogusFctStamp = ST.special "bogusFct"
val bogusSigStamp = ST.special "bogusSig"
val bogusRpath = IP.IPATH[S.strSymbol "Bogus"]

val bogusStrEntity : strEntity =
    { stamp = bogusStrStamp,
      entities = ERReenv,
      properties = PropList.newHolder (),
      rpath = bogusRpath,
      stub = NONE}

val bogusSig : Signature =
    SIG {stamp = bogusSigStamp,
	 name=NONE, closed=true, fctflag=false,
	 elements=[],
	 properties = PropList.newHolder (),
	 typsharing=[], strsharing=[],
	 stub = NONE}

val bogusFctEntity : fctEntity =
    {stamp = bogusFctStamp,
     closure = CLOSURE{param=EP.bogusEntVar,
		       body= CONSTstr bogusStrEntity,
		       env=NILeenv},
     tycpath=NONE,
     properties = PropList.newHolder (),
     rpath = bogusRpath,
     stub = NONE}

end (* local *)
end (* structure Modules *)
