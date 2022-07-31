(* absyn.sig
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature ABSYN =
  sig

    type region = Ast.region  (* = int * int *)

    datatype value
      = VAR of Variable.var
      | CON of Types.datacon
      | ERRORid

    datatype numberedLabel = LABEL of {name: Symbol.symbol, number: int}

    (* datatype con -- switch case discriminators  (moved here from matchcomp/paths.sml
     *  con translates direclty to PLambda.con, except for VLENcon (n,t), which is translated
     *  to PLambda.INTcon n (in Generate.generate). *)
    datatype con
      = DATAcon of Types.datacon * Types.tyvar list
      | INTcon of int IntConst.t
      | WORDcon of int IntConst.t
      | STRINGcon of string
      | VLENcon of int * Types.ty
	 (* element type ty is used in VLENcon case of Generate..genSwitch, then
	  * VLENcon is eliminated by translation to INTcon in transVecCase *)

    datatype exp
      = VARexp of Variable.var ref * Types.tyvar list (* instance type *)
      | CONexp of Types.datacon * Types.tyvar list (* instance type *)
      | NUMexp of string * num_lit	(* string is source text of literal *)
      | REALexp of string * real_lit	(* string is source text of literal *)
      | STRINGexp of string
      | CHARexp of char
      | RECORDexp of (numberedLabel * exp) list
      | RSELECTexp of exp * int
          (* record selections, generated only by match compiler *)
      | VSELECTexp of exp * Types.ty * int
          (* vector selections, generated only by match compiler; type is element type *)
      | VECTORexp of exp list * Types.ty
      | APPexp of exp * exp
      | FNexp of fnrules
      | CASEexp of exp * fnrules
      | HANDLEexp of exp * fnrules
      | RAISEexp of exp * Types.ty
      | SWITCHexp of exp * srule list * exp option
      | VSWITCHexp of exp * Types.ty * srule list * exp
          (* SWITCHexp, VSWITCHexp are created only by match compiler, for shallow case dispatch;
	   * SWITCHexp is general case, for constants and datatypes; default is optional
           * VSWITCH is a special case dispatching on vector length; default is mandatory *)
      | IFexp of { test: exp, thenCase: exp, elseCase: exp }
      | ANDALSOexp of exp * exp
      | ORELSEexp of exp * exp
      | WHILEexp of { test: exp, expr: exp }
      | LETexp of dec * exp
      | LETVexp of Variable.var * exp * exp
      | SEQexp of exp list
      | CONSTRAINTexp of exp * Types.ty
      | MARKexp of exp * region

    and rule = RULE of pat * exp

    and srule = SRULE of con * Variable.var option * exp
       (* INVARIANT: var option will be SOME iff con is DATAcon(datacon,_) where datacon
	* is not a constant. The var, when present, will be bound to the decon[con] of the
        * subject value (where/how?). *)

    and pat
      = WILDpat
      | VARpat of Variable.var
      | NUMpat of string * num_lit	(* string is source text of literal *)
      | STRINGpat of string
      | CHARpat of char
      | CONpat of Types.datacon * Types.tyvar list (* instance type *)
      | RECORDpat of {fields : (Types.label * pat) list,
		      flex : bool, typ : Types.ty ref}
      | APPpat of Types.datacon * Types.tyvar list * pat
      | CONSTRAINTpat of pat * Types.ty
      | LAYEREDpat of pat * pat
      | ORpat of pat * pat
      | VECTORpat of pat list * Types.ty
      | MARKpat of pat * region
      | NOpat

    and dec
      = VALdec of vb list
      | VALRECdec of rvb list
      | DOdec of exp
      | VARSELdec of Variable.var * Variable.var * int
      | TYPEdec of Types.tycon list
      | DATATYPEdec of {datatycs: Types.tycon list, withtycs: Types.tycon list}
      | ABSTYPEdec of {abstycs: Types.tycon list,
		       withtycs: Types.tycon list, body: dec}
      | EXCEPTIONdec of eb list
      | STRdec of strb list
      | FCTdec of fctb list
      | SIGdec of Modules.Signature list
      | FSIGdec of Modules.fctSig list
      | OPENdec of (SymPath.path * Modules.Structure) list
      | LOCALdec of dec * dec
      | SEQdec of dec list
      | OVLDdec of Variable.var
      | FIXdec of {fixity: Fixity.fixity, ops: Symbol.symbol list}
      | MARKdec of dec * region

    and strexp
      = VARstr of Modules.Structure
      | STRstr of Bindings.binding list
      | APPstr of {oper: Modules.Functor, arg: Modules.Structure,
		   argtycs: Types.tycpath list}
      | LETstr of dec * strexp
      | MARKstr of strexp * region

    and fctexp
      = VARfct of Modules.Functor
      | FCTfct of {param: Modules.Structure, argtycs: Types.tycpath list,
		   def: strexp}
      | LETfct of dec * fctexp
      | MARKfct of fctexp * region

    and vb = VB of {pat: pat, exp: exp,
		    typ: Types.ty,                (* the common type of the pat and exp *)
		    boundtvs: Types.tyvar list,   (* "generalized" metatyvars of whole pattern *)
		    tyvars: Types.tyvar list ref} (* used for tracking "explicit" tyvars *)

    and rvb = RVB of {var: Variable.var, exp: exp, resultty: Types.ty option,
		      tyvars: Types.tyvar list ref}

    and eb = EBgen of {exn: Types.datacon, etype: Types.ty option (* , ident: exp *)}
	   | EBdef of {exn: Types.datacon, edef: Types.datacon}

    and strb = STRB of {name: Symbol.symbol, str: Modules.Structure, def: strexp}
    and fctb = FCTB of {name: Symbol.symbol, fct: Modules.Functor, def: fctexp}

    withtype fnrules = rule list * Types.ty * Types.ty
             (* ty is the type of the domain (lhs) of the rules *)
         and num_lit = Types.ty IntConst.t
         and real_lit = Types.ty RealConst.t

  end (* signature ABSYN *)
