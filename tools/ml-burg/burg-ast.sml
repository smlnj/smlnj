(* 
 * burg-ast.sml
 *
 * Abstract syntax trees for BURG specifications.
 *
 * $Log$
 * Revision 1.2  2000/06/01 18:33:42  monnier
 * bring revisions from the vendor branch to the trunk
 *
 * Revision 1.1.1.8  1999/04/17 18:56:03  monnier
 * version 110.16
 *
 * Revision 1.1.1.1  1997/01/14 01:37:59  george
 *   Version 109.24
 *
 * Revision 1.1.1.2  1997/01/11  18:52:28  george
 *   ml-burg Version 109.24
 *
 * Revision 1.1.1.1  1996/01/31  16:01:24  george
 * Version 109
 * 
 *)

structure BurgAST =
  struct

    datatype decl_ast = START of string
		      | TERM of (string * string option) list
		      | TERMPREFIX of string
		      | RULEPREFIX of string
		      | SIG of string

    datatype pattern_ast = PAT of (string * pattern_ast list)

    datatype rule_ast = RULE of (string * pattern_ast * string * int list)

    datatype spec_ast = SPEC of {head : string list,
				 decls : decl_ast list, 
				 rules : rule_ast list,
				 tail : string list}
  end (* BurgAST *)

