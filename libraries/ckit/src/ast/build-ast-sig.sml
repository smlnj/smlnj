(* Copyright (c) 1998 by Lucent Technologies *)

signature BUILD_AST =
sig

  (* information returned by makeAst *)
  type astBundle =
    {ast: Ast.ast,
     tidtab: Bindings.tidBinding Tidtab.uidtab,
     errorCount: int,
     warningCount: int,
     auxiliaryInfo: {aidtab: Tables.aidtab,
	             implicits: Tables.aidtab,
                     env: State.symtab}}

  (* control of buildAst modes *)
  val insert_explicit_coersions : bool ref
    (* insert explicit casts at points where there are implicit type conversions? *)
  val insert_scaling : bool ref
    (* insert scaling computations at pointer arithmetic? *)
  val reduce_sizeof : bool ref
    (* replace sizeof expressions by integer constants? *)
  val reduce_assign_ops : bool ref
    (* replace assignops by simple ops and assignments? *)
  val multi_file_mode : bool ref
    (* analysis mode -- allow repeated definitions? *)
  val local_externs_ok: bool ref
    (* local declarations involving EXTERN are ok (usually false) *)
  val default_signed_char: bool ref
    (* is the type "char" implicitly regarded as signed? *)

  val multiFileMode: unit -> unit  (* was called analysis mode *)
  val compilerMode: unit -> unit
  val sourceToSourceMode: unit -> unit

  (* convert a parse tree to an ast and associated map from expression
   * adornments to types 
   *)
  val makeAst : 
    Sizes.sizes * State.stateInfo * Error.errorState
    -> ParseTree.externalDecl list
    -> astBundle

end (* signature BUILD_AST *)
