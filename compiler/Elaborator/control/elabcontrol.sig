(* elabcontrol.sig
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Flags controlling the elaborator.
 *)

signature ELAB_CONTROL =
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
	(* ElabCore, ElabTop, ElabUtil, Control_MC *)

(*    val boxedconstconreps : bool ref *)
	(* ConRep *)

    val printAbsyn : bool ref

  (***** Controls for warning messages *****)

  (* warning message when variables are defined but not used (default true) *)
    val unusedWarn : bool ref

  (* warning message on multiple defs (default false) *)
    val multDefWarn : bool ref
	(* Instantiate, Control_MC (TopLevel/main/control.sml) *)

  (* check share defs (default true) *)
    val shareDefError : bool ref
	(* Instantiate, Control_MC *)

  (* warn on value restriction for local defs (default false) *)
    val valueRestrictionLocalWarn : bool ref

  (* warn on value restriction at top level (default true) *)
    val valueRestrictionTopWarn : bool ref

  (* show culprits in type error messages (default false) *)
    val showTypeErrorCulprits : bool ref

  end (* signature ELAB_CONTROL *)
