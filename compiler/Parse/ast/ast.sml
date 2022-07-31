(* ast.sml
 *
 * Syntax trees for bare ML
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(****************************************************************************
 *            PLEASE PROPAGATE ANY MODIFICATIONS TO THIS FILE               *
 *                    INTO PERV.SML AND SYSTEM.SIG                          *
 ****************************************************************************)

structure Ast : AST =
  struct

    open Symbol Fixity

  (* to mark positions in files *)
    type srcpos = int  (* character position from beginning of stream (base 0) *)
    type region = srcpos * srcpos   (* start and end position of region *)

  (* symbolic path (Modules.spath) *)
    type path = symbol list
    type 'a fixitem = {item: 'a, fixity: symbol option, region: region}

  (* integer/word literal; the string is the literal as it appeared in the source
   * and the int is the value of the literal.
   *)
    type literal = string * IntInf.int

  (* real literal; also paired with source string *)
    type real_lit = string * RealLit.t

    datatype 'a sigConst
      = NoSig
      | Transparent of 'a
      | Opaque of 'a

    (* EXPRESSIONS *)

    datatype exp
      = VarExp of path			(* variable *)
      | FnExp of rule list		(* abstraction *)
      | FlatAppExp of exp fixitem list	(* expressions before fixity parsing *)
      | AppExp of {function:exp,argument:exp}
				    	(* application *)
      | CaseExp of{expr:exp,rules:rule list}
				    	(* case expression *)
      | LetExp of {dec:dec,expr:exp}	(* let expression *)
      | SeqExp of exp list		(* sequence of expressions *)
      | IntExp of literal		(* integer *)
      | WordExp of literal		(* word literal *)
      | RealExp of real_lit		(* floating point coded by its string *)
      | StringExp of string		(* string *)
      | CharExp of string		(* char *)
      | RecordExp of (symbol * exp) list (* record *)
      | ListExp of exp list	        (*  [list,in,square,brackets] *)
      | TupleExp of exp list		(* tuple (derived form) *)
      | SelectorExp of symbol		(* selector of a record field *)
      | ConstraintExp of {expr:exp,constraint:ty}
					(* type constraint *)
      | HandleExp of {expr:exp, rules:rule list}
				 	(* exception handler *)
      | RaiseExp of exp			(* raise an exception *)
      | IfExp of {test:exp, thenCase:exp, elseCase:exp}
					(* if expression (derived form) *)
      | AndalsoExp of exp * exp		(* andalso (derived form) *)
      | OrelseExp of exp * exp		(* orelse (derived form) *)
      | WhileExp of {test:exp,expr:exp}
					(* while (derived form) *)
      | MarkExp of exp * region		(* mark an expression *)
      | VectorExp of exp list   	(* vector *)

    (* RULE for case functions and exception handler *)
    and rule = Rule of {pat:pat,exp:exp}

    (* PATTERN *)
    and pat
      = WildPat					(* empty pattern *)
      | VarPat of path				(* variable/const-dcon pattern *)
      | IntPat of literal			(* integer *)
      | WordPat of literal			(* word literal *)
      | StringPat of string			(* string *)
      | CharPat of string			(* char *)
      | RecordPat of {def:(symbol * pat) list, flexibility:bool}
						(* record *)
      | ListPat of pat list			(* [list,in,square,brackets] *)
      | TuplePat of pat list			(* tuple *)
      | FlatAppPat of pat fixitem list		(* patterns before fixity parsing *)
      | AppPat of {constr:pat,argument:pat}	(* constructor application *)
      | ConstraintPat of {pattern:pat,constraint:ty}
						(* constraint *)
      | LayeredPat of {varPat:pat,expPat:pat} (* as patterns *)
      | MarkPat of pat * region			(* mark a pattern *)
      | VectorPat of pat list			(* vector pattern *)
      | OrPat of pat list			(* or-pattern *)

    (* STRUCTURE EXPRESSION *)
    and strexp = VarStr of path				(* variable structure *)
	       | BaseStr of dec				(* basic structure (struct...end) *)
	       | ConstrainedStr of strexp * sigexp sigConst (* signature constrained *)
	       | AppStr of path * (strexp * bool) list	(* application (external) *)
	       | AppStrI of path * (strexp * bool) list	(* application (internal) *)
	       | LetStr of dec * strexp			(* let in structure *)
	       | MarkStr of strexp * region		(* mark *)

    (* FUNCTOR EXPRESSION *)
    and fctexp = VarFct of path * fsigexp sigConst	(* functor variable *)
	       | BaseFct of {				(* definition of a functor *)
		    params	   : (symbol option * sigexp) list,
		    body	   : strexp,
		    constraint : sigexp sigConst}
	       | LetFct of dec * fctexp
	       | AppFct of path * (strexp * bool) list * fsigexp sigConst
							(* application *)
	       | MarkFct of fctexp * region     	(* mark *)

    (* WHERE SPEC *)
    and wherespec = WhType of symbol list * tyvar list * ty
		  | WhStruct of symbol list * symbol list

    (* SIGNATURE EXPRESSION *)
    and sigexp = VarSig of symbol			(* signature variable *)
	       | AugSig of sigexp * wherespec list	(* sig augmented with where specs *)
	       | BaseSig of spec list			(* basic signature (sig...end) *)
	       | MarkSig of sigexp * region 		(* mark *)

    (* FUNCTOR SIGNATURE EXPRESSION *)
    and fsigexp = VarFsig of symbol		(* funsig variable *)
		| BaseFsig of {param: (symbol option * sigexp) list, result:sigexp}
						(* basic funsig *)
		| MarkFsig of fsigexp * region	(* mark *)

    (* SPECIFICATION FOR SIGNATURE DEFINITIONS *)
    and spec = StrSpec of (symbol * sigexp * path option) list  (* structure *)
	     | TycSpec of ((symbol * tyvar list * ty option) list * bool) (* type *)
	     | FctSpec of (symbol * fsigexp) list		(* functor *)
	     | ValSpec of (symbol * ty) list	                (* value *)
	     | DataSpec of {datatycs: db list, withtycs: tb list} (* datatype *)
	     | DataReplSpec of symbol * path                    (* datatype replication *)
	     | ExceSpec of (symbol * ty option) list	        (* exception *)
	     | ShareStrSpec of path list			(* structure sharing *)
	     | ShareTycSpec of path list			(* type sharing *)
	     | IncludeSpec of sigexp			        (* include specif *)
	     | MarkSpec of spec * region		        (* mark a spec *)

    (* DECLARATIONS (let and structure) *)
    and dec = ValDec of (vb list * tyvar list)		(* values *)
	    | ValrecDec of (rvb list * tyvar list)	(* recursive values *)
	    | DoDec of exp				(* 'do' exp *)
	    | FunDec of (fb list * tyvar list)		(* recurs functions *)
	    | TypeDec of tb list			(* type dec *)
	    | DatatypeDec of {datatycs: db list, withtycs: tb list} (* datatype dec *)
	    | DataReplDec of symbol * path              (* dt replication *)
	    | AbstypeDec of {abstycs: db list, withtycs: tb list, body: dec} (* abstract type *)
	    | ExceptionDec of eb list			(* exception *)
	    | StrDec of strb list			(* structure *)
	    | FctDec of fctb list			(* functor *)
	    | SigDec of sigb list			(* signature *)
	    | FsigDec of fsigb list			(* funsig *)
	    | LocalDec of dec * dec			(* local dec *)
	    | SeqDec of dec list			(* sequence of dec *)
	    | OpenDec of path list			(* open structures *)
	    | OvldDec of symbol * exp list     	        (* overloading (internal; restricted) *)
	    | FixDec of {fixity: fixity, ops: symbol list}  (* fixity *)
	    | MarkDec of dec * region		        (* mark a dec *)

    (* VALUE BINDINGS *)
    and vb = Vb of {pat: pat, exp: exp, lazyp: bool}
	   | MarkVb of vb * region

    (* RECURSIVE VALUE BINDINGS *)
    and rvb = Rvb of {var: symbol, fixity: (symbol * region) option,
		      exp: exp, resultty: ty option, lazyp: bool}
	    | MarkRvb of rvb * region

    (* RECURSIVE FUNCTIONS BINDINGS *)
    and fb = Fb of clause list * bool (* bool indicates whether lazy *)
	   | MarkFb of fb * region

    (* CLAUSE: a definition for a single pattern in a function binding *)
    and clause = Clause of {pats: pat fixitem list, resultty: ty option, exp:exp}

    (* TYPE BINDING *)
    and tb = Tb of {tyc : symbol, def : ty, tyvars : tyvar list}
	   | MarkTb of tb * region

    (* DATATYPE BINDING *)
    and db = Db of {tyc : symbol, tyvars : tyvar list,
		    rhs : (symbol * ty option) list, lazyp : bool}
	   | MarkDb of db * region

    (* EXCEPTION BINDING *)
    and eb = EbGen of {exn: symbol, etype: ty option} (* Exception definition *)
	   | EbDef of {exn: symbol, edef: path}	  (* defined by equality *)
	   | MarkEb of eb * region

    (* STRUCTURE BINDING *)
    and strb = Strb of {name: symbol,def: strexp,constraint: sigexp sigConst}
	     | MarkStrb of strb * region

    (* FUNCTOR BINDING *)
    and fctb = Fctb of {name: symbol,def: fctexp}
	     | MarkFctb of fctb * region

    (* SIGNATURE BINDING *)
    and sigb = Sigb of {name: symbol,def: sigexp}
	     | MarkSigb of sigb * region

    (* FUNSIG BINDING *)
    and fsigb = Fsigb of {name: symbol,def: fsigexp}
	      | MarkFsigb of fsigb * region

    (* TYPE VARIABLE *)
    and tyvar = Tyv of symbol
	      | MarkTyv of tyvar * region

    (* TYPES *)
    and ty
      = VarTy of tyvar			(* type variable *)
      | ConTy of symbol list * ty list	(* type constructor application *)
      | RecordTy of (symbol * ty) list 	(* record *)
      | TupleTy of ty list		(* tuple *)
      | MarkTy of ty * region	        (* mark type *)

  end (* structure Ast *)
