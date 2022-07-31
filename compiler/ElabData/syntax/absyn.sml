(* absyn.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Absyn : ABSYN =
  struct

    structure S = Symbol
    structure F = Fixity
    structure SP = SymPath
    structure B = Bindings
    structure T = Types
    structure V = Variable

    type region = Ast.region  (* = int * int *)

    (* "value" -- "denotation" of a core language "value" identifier,
     *  which denotes either a variable or data constructor;
     *  better names would be "atomicSym" or "atomId" or "valueId" or "coreId" *)
    datatype value
      = VAR of V.var
      | CON of T.datacon
      | ERRORid

    datatype numberedLabel = LABEL of {name: S.symbol, number: int}

    (* datatype con -- switch case discriminators  (moved here from matchcomp/paths.sml
     *  con translates direclty to PLambda.con, except for VLENcon (n,t), which is translated
     *  to PLambda.INTcon n (in Generate.generate). *)
    datatype con
      = DATAcon of T.datacon * T.tyvar list
      | INTcon of int IntConst.t
      | WORDcon of int IntConst.t
      | STRINGcon of string
      | VLENcon of int * T.ty
	 (* element type ty is used in VLENcon case of Generate..genDecTree, where it is
	  * eliminated by translation to INTcon. *)

    datatype exp
      = VARexp of V.var ref * T.tyvar list
	(* the 2nd arg is a type metavar list used to capture the instantiation
	   parameters for this occurence of V.var when its type is polymorphic.
	   FLINT will use these to provide explicit type parameters for V.var
           if V.var is bound to a primop, where they will be needed to specialize
	   the primop. *)
      | CONexp of Types.datacon * T.tyvar list (* ditto *)
      | NUMexp of string * num_lit	(* string is source text of literal *)
      | REALexp of string * real_lit	(* string is source text of literal *)
      | STRINGexp of string
      | CHARexp of char
      | RECORDexp of (numberedLabel * exp) list
      | RSELECTexp of exp * int
          (* 0-based record/tuple selections, produced only by match compiler for VBs
           * or translating path suffixes. RSELECTexp and VSELECTexp will be nested when
           * translating path suffixes in Generate.generate. *)
      | VSELECTexp of exp * T.ty * int  (* need elemTy? *)
          (* 0-based vector selections, produced only by match compiler, as for RSELECTexp *)
      | VECTORexp of exp list * T.ty
      | APPexp of exp * exp
      | FNexp of fnrules
      | HANDLEexp of exp * fnrules
      | CASEexp of exp * fnrules
      | SWITCHexp of exp * srule list * exp option
      | VSWITCHexp of exp * T.ty * srule list * exp
          (* SWITCHexp, VSWITCHexp created only by match compiler,
           * VSWITCHexp for vector length, where default is required(?) *)
      | RAISEexp of exp * T.ty
      | IFexp of { test: exp, thenCase: exp, elseCase: exp }
      | ANDALSOexp of exp * exp
      | ORELSEexp of exp * exp
      | WHILEexp of { test: exp, expr: exp }
      | LETexp of dec * exp
      | LETVexp of V.var * exp * exp  (* only produced by match compiler *)
      | SEQexp of exp list
      | CONSTRAINTexp of exp * T.ty
      | MARKexp of exp * region

    and rule = RULE of pat * exp  (* in FNexp, CASEexp, HANDLEexp via fnrules *)

    and srule = SRULE of con * V.var option * exp  (* in SWITCHexp, VSWITCHexp *)
       (* INVARIANT: var option will be SOME iff con is DATAcon(datacon,_) where datacon
	* is not a constant. The var, when present, will be bound to the decon[con] of the
        * subject value (where?) *)

    and pat
      = WILDpat
      | VARpat of V.var
      | NUMpat of string * num_lit	(* string is source text of literal *)
      | STRINGpat of string
      | CHARpat of char
      | CONpat of T.datacon * T.tyvar list (* See comment for VARexp *)
      | RECORDpat of {fields: (T.label * pat) list, flex: bool, typ: T.ty ref}
      | APPpat of T.datacon * T.tyvar list * pat
      | CONSTRAINTpat of pat * T.ty
      | LAYEREDpat of pat * pat
      | ORpat of pat * pat
      | VECTORpat of pat list * T.ty
      | MARKpat of pat * region
      | NOpat

    and dec
      = VALdec of vb list  (* always a single element list (FLINT normalization) *)
      | VALRECdec of rvb list
      | DOdec of exp
      | VARSELdec of V.var * V.var * int (* produced (only) by match compiler *)
      | TYPEdec of T.tycon list
      | DATATYPEdec of {datatycs: T.tycon list, withtycs: T.tycon list}
      | ABSTYPEdec of {abstycs: T.tycon list, withtycs: T.tycon list, body: dec}
      | EXCEPTIONdec of eb list
      | STRdec of strb list
      | FCTdec of fctb list
      | SIGdec of Modules.Signature list
      | FSIGdec of Modules.fctSig list
      | OPENdec of (SP.path * Modules.Structure) list
      | LOCALdec of dec * dec
      | SEQdec of dec list
      | OVLDdec of V.var
      | FIXdec of {fixity: F.fixity, ops: S.symbol list}
      | MARKdec of dec * region

    (*
     * [FLINT] The "argtycs" field in APPstr is used to record the list of instantiated
     * HO-tycs passed to functor during the functor application.
     *)
    and strexp
      = VARstr of Modules.Structure
      | STRstr of B.binding list
      | APPstr of {oper: Modules.Functor,
		   arg: Modules.Structure,
		   argtycs: T.tycpath list}
      | LETstr of dec * strexp
      | MARKstr of strexp * region

    (*
     * [FLINT] For typing purpose, a functor is viewed as a high-order type constructor
     * (hotyc) that takes a list of hotycs returns another list of hotycs. The
     * "argtycs" field in FCTfct records the list of formal hotyc paramaters.
     *)
    and fctexp
      = VARfct of Modules.Functor
      | FCTfct of {param: Modules.Structure, argtycs: T.tycpath list, def: strexp}
      | LETfct of dec * fctexp
      | MARKfct of fctexp * region

    (*
     * Each value binding vb only binds one variable identifier [FLINT "normalization"].
     * That is, pat is always a simple VARpat (with type constraints) or it simply
     * does not contain any variable patterns; boundtvs gives the list of
     * type variables that are being generalized at this binding, as determined
     * during type checking.
     *)
    and vb = VB of {pat: pat, exp: exp,
		    typ: T.ty,                 (* the common type of the pat and exp *)
		    boundtvs: T.tyvar list,    (* "generalized" metatyvars of whole pattern *)
		    tyvars: T.tyvar list ref}  (* used for tracking "explicit" tyvars *)

    (*
     * As for value bindings, vb, boundtvs gives a list of type variables
     * being generalized at this binding. However, the mutually recursive
     * list of RVBs could share type variables, that is, the boundtvs sets
     * used in these RVBs could contain overlapping sets of type variables.
     * The resultty (if SOME ty) is the full type of var, the bound function
     * variable, not just the result type of the function.
     *)
    and rvb = RVB of {var: V.var, exp: exp, resultty: T.ty option,
		      tyvars: T.tyvar list ref}

    and eb = EBgen of {exn: T.datacon, etype: T.ty option (*, ident: exp *)}
	   | EBdef of {exn: T.datacon, edef: T.datacon}

    and strb = STRB of {name: S.symbol, str: Modules.Structure, def: strexp}
    and fctb = FCTB of {name: S.symbol, fct: Modules.Functor, def: fctexp}

    withtype fnrules = rule list * T.ty * T.ty
         and num_lit = Types.ty IntConst.t
         and real_lit = Types.ty RealConst.t

  end
