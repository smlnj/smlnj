(* grammar-syntax.sml
 *
 * COPYRIGHT (c) 2009 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Parse tree for grammar input.
 *)

structure GrammarSyntax =
  struct

    type span = Err.span
    type code = span * String.string
    type symbol = Atom.atom
    type name = string
    type ty = string
    type constr = (symbol * ty option * Atom.atom option)

    datatype decl
      = NAME of name
      | HEADER of code
      | START of symbol
      | ENTRY of symbol
      | KEYWORD of symbol				(* %keywords TOKEN ... *)
      | VALUE of symbol * code				(* %value TOKEN ( ... ) *)
      | PREFER of symbol				(* %prefer TOKEN ... *)
      | CHANGE of (symbol list * symbol list)		(* %change TOKEN* -> TOKEN* | ... *)
      | DEFS of code
      | TOKEN of constr
      | TOKENTYPE of ty					(* %tokentype monotype *)
      | IMPORT of {
	  filename : string,
	  dropping : (span * symbol) list
	}
      | REFCELL of name * ty * code
      | RULE of {
	  lhs : symbol,
	  formals : name list,
	  rhs : rhs
	}
      | NONTERM of symbol * ty

    and rhs = RHS of {
	  items : (string option * (span * item)) list,
	  try : bool,
	  predicate : code option,
	  action : code option,
	  loc : span
        }

    and item
      = SYMBOL of symbol * code option
      | SUBRULE of rhs list	(* ( ... ) *)
      | CLOS of span * item	(* ( ... )* *)
      | POSCLOS of span * item	(* ( ... )+ *)
      | OPT of span * item	(* ( ... )? *)

    type grammar = (span * decl) list

    local
      fun ppDecl (_, NAME n) = "%name"
	| ppDecl (_, HEADER _) = "%code"
	| ppDecl (_, START s) = "%start"
	| ppDecl (_, ENTRY s) = "%entry"
	| ppDecl (_, KEYWORD s) = "%keyword"
	| ppDecl (_, VALUE _) = "%value"
	| ppDecl (_, PREFER _) = "%prefer"
	| ppDecl (_, CHANGE _) = "%change"
	| ppDecl (_, DEFS c) = "%defs"
	| ppDecl (_, TOKENTYPE c) = "%tokentype"
	| ppDecl (_, TOKEN cstr) = "%tokens"
	| ppDecl (_, NONTERM cstr) = "%nonterm"
	| ppDecl (_, IMPORT {filename, dropping}) = "%import"
	| ppDecl (_, REFCELL (n, ty, c)) = "%refcell"
	| ppDecl (_, RULE {lhs, formals, rhs}) = "-- rule: " ^ (Atom.toString lhs)
    in
    fun ppGrammar decls = String.concatWith "\n" (map ppDecl decls)
    end

  end
