(* Admin/control/elaborator-control.sig
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Flags controlling the elaborator.
 *)

signature ELABORATOR_CONTROL =
sig

    val etdebugging : bool ref
	(* ElabType *)
    val esdebugging : bool ref
	(* ElabSig *)
    val insdebugging : bool ref
	(* Instantiate *)
    val smdebugging : bool ref
	(* Sigmatch *)
    val ecdebugging : bool ref
	(* ElabCore *)
    val emdebugging : bool ref
	(* ElabMod *)
    val tcdebugging : bool ref
	(* Typecheck *)
    val unidebugging : bool ref
	(* Unify *)
    val ovlddebugging : bool ref
	(* Overload *)
    val instantiateSigs : bool ref
	(* ElabMod, Control_MC *)
    val etopdebugging : bool ref
	(* ElabTop *)

    val markabsyn : bool ref
	(* ElabCore, ElabTop, ElabUtil ? *)

    val printAbsyn : bool ref
    val printAst : bool ref

    (***** Controls for warning messages *****)

    val unusedWarn : bool ref
    (* warning message when variables are defined but not used (disabled: default false) *)

    val multDefWarn : bool ref
    (* warning message on multiple defs (default false) *)

    val shareDefError : bool ref
    (* check share defs (default true) *)

    val valueRestrictionLocalWarn : bool ref
    (* warn on value restriction for local defs (default false) *)

    val valueRestrictionTopWarn : bool ref
    (* warn on value restriction at top level (default true) *)

    val showTypeErrorCulprits : bool ref
    (* show culprits in type error messages (default false) *)

end (* signature ELAB_CONTROL *)
