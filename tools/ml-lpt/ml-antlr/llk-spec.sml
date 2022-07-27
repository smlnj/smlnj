(* llk-spec.sml
 *
 * COPYRIGHT (c) 2006
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (http://www.cs.uchicago.edu/~adrassi)
 * All rights reserved.
 *
 * Datatypes for grammar specification.
 * NOTE: individual modules are available that
 *       manipulate the datatypes described in
 *       this module.
 *)

structure LLKSpec =
  struct

    type ty = string
    type span = Err.span

    type 'a info = {
	data : 'a
      }

    datatype token = T of {
        id : Int.int,			(* globally unique ID *)
	name : Atom.atom,
	loc : span,
	ty : ty option,
	abbrev : Atom.atom option,
	keyword : bool,			(* true if marked as a %keyword *)
	default : string option		(* optional default argument for error repair *)
      }

    and nonterm = NT of {
        id : Int.int,			(* globally unique ID *)
	name : Atom.atom,
	loc : span option ref,
	binding : nt_binding,
	prods : prod list ref,
	formals : Atom.atom list ref,
	isEBNF : bool,
	ty : ty option ref
      }

    and nt_binding = TOP | WITHIN of prod

    and prod = PROD of {
        id : Int.int,			(* globally unique ID *)
	name : string,
	try : bool,
	lhs : nonterm,
	rhs : item list ref,	(* ref for tying recursive knot:
				 * subrules refer to their containing prod
				 *)
	rhsBindings : (string * bool) list,
	pred : Action.action option,
	action : Action.action option,
	loc : span
      }

    and preitem
      = TOK of token
      				(* nonterm * optional actual args *)
      | NONTERM of (nonterm * Action.action option)
      | CLOS of nonterm		(* ( ... )* *)
      | POSCLOS of nonterm	(* ( ... )+ *)
      | OPT of nonterm		(* ( ... )? *)

    and item = ITEM of {
        id : Int.int,			(* globally unique ID *)
        loc : span,
	sym : preitem
      }

    withtype sem_pred = Action.action

    datatype refcell
      = REFCELL of {
	  name : string,
	  ty : ty,
	  initCode : Action.action,
	  loc : span
        }

    datatype grammar = Grammar of {
        name : string,
	header : string option,
	defs : Action.action,		(* user definitions (code) *)
        toks : token list,
	toksImport : Action.action option,	(* optional token datatype *)
	changes : (token list * token list) list,
        nterms : nonterm list,
        prods : prod list,
	eof : token,
	sortedTops : nonterm list list,	(* topologically sorted nonterms *)
	startnt : nonterm,
	entryPoints : nonterm list,
	refcells : refcell list
      }

  end
