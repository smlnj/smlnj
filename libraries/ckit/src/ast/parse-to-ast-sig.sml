(* ast/parse-to-ast-sig.sml *)

(* This is the top-level interface to the C front-end.  It is 
 * implemented by the structures Ansic, FiveESSC, and D *)

signature PARSE_TO_AST =
sig

  (* astBundle: the collection of information returned as the result of elaboration *)
  type astBundle =
    {ast: Ast.ast,  (* the abstract syntax representation of a trans. unit *)
     tidtab: Bindings.tidBinding Tidtab.uidtab, (* table of type identifiers *)
     errorCount: int,  (* count of errors occuring during parsing and elaboration *)
     warningCount: int,(* count of warnings occuring during parsing and elaboration *)
     auxiliaryInfo:  (* annotations and symbol table info *)
       {aidtab: Tables.aidtab,  (* type annotation table *)
	implicits: Tables.aidtab,
	  (* types associated with implicit argument conversions.
	   * See, e.g. "usual unary" and "usual binary" conversions
           * in Harbison & Steele *)
	env: State.symtab}} (* symbol table generated during elaboration *)

  val progToState : astBundle -> State.stateInfo
    (* extracts stateInfo from astBundle for cascading processing of multiple
     * translation units *)

  val fileToAst' : 
      TextIO.outstream (* error stream *)
      -> (Sizes.sizes * State.stateInfo) (* sizes info and initial state *)
      -> string  (* source file *)
      -> astBundle
    (* processs a source file given the state resulting from processing
     * previous files *)

  val fileToAst :
      string  (* source file *)
      -> astBundle
    (* process a file in isolation *)

  val fileToC : string -> unit
    (* process a file and pretty print the resulting ast *)

end (* signature PARSE_TO_AST *)
