(* Admin/control/elaborator-control.sml
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Flags controlling the elaborator.
 *)

structure ElaboratorControl : ELABORATOR_CONTROL =
struct

    val etdebugging = ref false (* "ElabType debugging" *)
    val esdebugging = ref false (* "ElabSig debugging" *)
    val insdebugging = ref false (* "Instantiate debugging" *)
    val smdebugging = ref false (* "Sigmatch debugging" *)
    val ecdebugging = ref false (* "ElabCore debugging" *)
    val emdebugging = ref false (* "ElabMod debugging" *)
    val tcdebugging = ref false (* "TypeCheck debugging" *)
    val unidebugging = ref false (* "Unify debugging" *)
    val ovlddebugging = ref false (* "Overload debugging" *)
    val instantiateSigs = ref true (* "instantiate all sigs" *)
    val etopdebugging = ref false (* "ElabTop debugging" *)

    val markabsyn = ref true (* "mark abstract syntax" *)
        (* ElabCore, ElabTop, ElabUtil *)

    val printAbsyn = ref false (* "absyn print mode" *)
    val printAst = ref false (* "ast print mode" *)
    val stats = ref false (* "match compiler timings and stats" *)

  (***** Controls for warning messages *****)

(* NOTE: we currently disable the unusedWarn check because of false positives for
 * mutually recursive functions. The false positives are caused by a
 * transformation done during type checking, which should be removed
 * at some point.  CHECK if problem solved. *)

    val unusedWarn = ref false (* "warn when variables are defined but not used" *)

    val multDefWarn = ref false (* "warn on multiple defs" *)
        (* Instantiate *)

    val shareDefError = ref true (* "check share defs" *)
        (* Instantiate *)

    val valueRestrictionLocalWarn = ref false (* "warn on value restriction for local defs" *)

    val valueRestrictionTopWarn = ref true (* "warn on value restriction at top level" *)

    val showTypeErrorCulprits = ref false (* "show culprits in type error messages" *)

end (* structure ElaboratorControl *)
