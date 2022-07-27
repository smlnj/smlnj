(* sml.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Syntax trees for a subset of SML.  Used to generate SML code.
 *)

structure SML =
  struct

    type id = string

    datatype top_decl
      = SIGtop of id * sigexp
      | STRtop of id * (bool * sigexp) option * strexp
      | VERBtop of string list

    and sigexp
      = IDsig of id				(* signature variable *)
      | AUGsig of sigexp * where_ty list	(* sig augmented with where specs *)
      | BASEsig of spec list			(* basic signature (sig...end) *)

    and where_ty
      = WHERETY of id list * id list * ty

    and strexp
      = IDstr of id
      | BASEstr of dec list
      | VERBstr of string list

    and spec
      = STRspec of id * sigexp
      | TYPEspec of bool * id list * id * ty option
      | DATATYPEspec of db list * (id list * id * ty) list
      | VALspec of id * ty
      | EXNspec of id * ty option
      | VERBspec of string list

    and dec
      = VALdec of pat * exp
      | FUNdec of fb list
      | TYPEdec of id list * id * ty
      | DATATYPEdec of db list * (id list * id * ty) list
      | EXCEPTIONdec of id * ty option
      | STRdec of id * sigexp option * strexp
      | OPENdec of id list
      | LOCALdec of dec list * dec list
      | VERBdec of string list

  (* function binding *)
    and fb
      = FB of (id * (pat list * exp) list)

  (* datatype binding *)
    and db
      = DB of id list * id * (id * ty option) list

    and exp
      = IDexp of id			(* variables and constuctors *)
      | NUMexp of string
      | STRINGexp of string
      | CHARexp of string
      | RECORDexp of (id * exp) list
      | TUPLEexp of exp list
      | SELECTexp of id * exp
      | APPexp of exp * exp
      | INFIXexp of exp * id * exp
      | HANDLEexp of exp * (pat * exp) list
      | RAISEexp of exp
      | CASEexp of exp * (pat * exp) list
      | IFexp of exp * exp * exp
      | FNexp of (pat * exp) list
      | LETexp of dec list * exp
      | SEQexp of exp list
      | CONSTRAINTexp of exp * ty
      | GRPexp of exp			(* '(' exp ')' *)
      | VERBexp of string

    and pat
      = WILDpat
      | IDpat of id
      | NUMpat of string
      | STRINGpat of string
      | CHARpat of string
      | CONpat of id * pat
      | INFIXpat of pat * id * pat
      | RECORDpat of {fields : (id * pat) list, flex : bool}
      | TUPLEpat of pat list
      | CONSTRAINTpat of pat * ty
      | ASpat of id * pat
      | GRPpat of pat			(* '(' pat ')' *)

    and ty
      = VARty of id			(* type variable *)
      | CONty of ty list * id		(* type constructor *)
      | FUNty of ty * ty		(* function type *)
      | RECORDty of (id * ty) list 	(* record *)
      | TUPLEty of ty list		(* tuple *)
      | VERBty of string		(* verbatim type expression *)

    local
      fun grpArg e = (case e
	     of APPexp _ => GRPexp e
	      | INFIXexp _ => GRPexp e
	      | HANDLEexp _ => GRPexp e
	      | RAISEexp _ => GRPexp e
	      | CASEexp _ => GRPexp e
	      | IFexp _ => GRPexp e
	      | FNexp _ => GRPexp e
	      | SEQexp _ => GRPexp e
	      | CONSTRAINTexp _ => GRPexp e
	      | VERBexp _ => GRPexp e
	      | _ => e
	    (* end case *))
    in
  (* smart constructor for application *)
    fun appExp (e1, e2) = let
	  val e1 = (case e1
		 of INFIXexp _ => GRPexp e1
		  | HANDLEexp _ => GRPexp e1
		  | RAISEexp _ => GRPexp e1
		  | CASEexp _ => GRPexp e1
		  | IFexp _ => GRPexp e1
		  | FNexp _ => GRPexp e1
		  | SEQexp _ => GRPexp e1
		  | CONSTRAINTexp _ => GRPexp e1
		  | VERBexp _ => GRPexp e1
		  | _ => e1
		(* end case *))
	  in
	    APPexp(e1, grpArg e2)
	  end

  (* smart constructor for field-select *)
    fun selectExp (proj, e) = SELECTexp(proj, grpArg e)

  (* smart constructor for raise expression *)
    fun raiseExp e = RAISEexp(grpArg e)
    end (* local *)

    local
      fun grpAction e = (case e
	     of HANDLEexp _ => GRPexp e
	      | CASEexp _ => GRPexp e
	      | FNexp _ => GRPexp e
	      | SEQexp _ => GRPexp e
	      | CONSTRAINTexp _ => GRPexp e
	      | _ => e
	    (* end case *))
    in
    fun funBind (f, rules) = let
	  fun mkArgPat p = (case p
		 of CONpat _ => GRPpat p
		  | INFIXpat _ => GRPpat p
		  | CONSTRAINTpat _ => GRPpat p
		  | ASpat _ => GRPpat p
		  | p => p
		(* end case *))
	  fun mkRule (pats, e) = (List.map mkArgPat pats, grpAction e)
	  in
	    FB(f, List.map mkRule rules)
	  end

    fun caseExp (e, rules) = let
	  fun mkRule (pat, e) = (pat, grpAction e)
	  in
	    CASEexp(e, List.map mkRule rules)
	  end

  (* smart constructor for fn expression *)
    fun fnExp rules = let
	  fun mkRule (pat, e) = (pat, grpAction e)
	  in
	    FNexp(List.map mkRule rules)
	  end
    end (* local *)

  (* construct a simple function binding of the form `f (x1, ..., xn) = e` *)
    fun simpleFB (f, [x], e) = funBind(f, [([IDpat x], e)])
      | simpleFB (f, xs, e) = funBind(f, [([TUPLEpat(List.map IDpat xs)], e)])

  (* construct a simple variable binding of the form `val x = e` *)
    fun simpleVB (x, e) = VALdec(IDpat x, e)

    fun letExp (decs, LETexp(decs', e)) = LETexp(decs @ decs', e)
      | letExp arg = LETexp arg

  (* construct a simple let binding of the form `let val x = e1 in e2` *)
    fun simpleLet (x, e1, e2) = letExp([simpleVB (x, e1)], e2)

    fun tupleTy [] = CONty([], "unit")
      | tupleTy [ty] = ty
      | tupleTy tys = TUPLEty tys

    fun tupleExp [e] = e
      | tupleExp es = TUPLEexp es

    fun tuplePat [p] = p
      | tuplePat ps = TUPLEpat ps

    val unitPat = TUPLEpat[]
    val unitExp = TUPLEexp[]

  end


