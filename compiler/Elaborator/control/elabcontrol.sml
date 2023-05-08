(* elabcontrol.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Flags controlling the elaborator.
 *)

structure ElabControl : ELAB_CONTROL =
struct

  val {newBool = new, ...} = MakeControls.make {name = "Elaborate", priority = [1]}

  val etdebugging = new ("et-debugging", "ElabType debugging", false)
      (* ElabType *)
  val esdebugging = new ("es-debugging", "ElabSig debugging", false)
      (* ElabSig *)
  val insdebugging = new ("ins-debugging", "Instantiate debugging", false)
      (* Instantiate *)
  val smdebugging = new ("sm-debugging", "Sigmatch debugging", false)
      (* Sigmatch *)
  val ecdebugging = new ("ec-debugging", "ElabCore debugging", false)
      (* ElabCore *)
  val emdebugging = new ("em-debugging", "ElabMod debugging", false)
      (* ElabMod *)
  val tcdebugging = new ("tc-debugging", "TypeCheck debugging", false)
      (* Typecheck *)
  val unidebugging = new ("uni-debugging", "Unify debugging", false)
      (* Unify *)
  val ovlddebugging = new ("ovld-debugging", "Overload debugging", false)
      (* Overload *)
  val instantiateSigs = new ("instantiate-sigs", "instantiate all sigs", true)
      (* ElabMod, Control_MC *)
  val etopdebugging = new ("etop-debugging", "ElabTop debugging", false)
      (* ElabTop *)


  val markabsyn = new ("markabsyn", "mark abstract syntax", true)
      (* ElabCore, ElabTop, ElabUtil, Control_MC *)

  val printAbsyn = new ("printAbsyn", "absyn print mode", false)
  val printAst = new ("printAst", "ast print mode", false)
  val stats = new ("stats", "match compiler timings and stats", false)


(***** Controls for warning messages *****)

  (* NOTE: we currently disable this unusedWarn check because of false positives for
   * mutually recursive functions.  The false positives are caused by a
   * transformation done during type checking, which should be removed
   * at some point.  CHECK if problem solved. *)
  val unusedWarn = new (
	"unused-warn",
	"warn when variables are defined but not used",
	false)

  val multDefWarn = new (
	"mult-def-warn",
	"warn on multiple defs",
	false)
      (* Instantiate, Control_MC (TopLevel/main/control.sml) *)

  val shareDefError = new (
	"share-def-error",
	"check share defs",
	true)
      (* Instantiate, Control_MC *)

  val valueRestrictionLocalWarn = new (
	"value-restriction-local-warn",
	"warn on value restriction for local defs",
	false)

  val valueRestrictionTopWarn = new (
	"value-restriction-top-warn",
	"warn on value restriction at top level", true)

  val showTypeErrorCulprits = new (
	"show-type-error-culprits",
	"show culprits in type error messages",
	false)

end (* structure ElabControl *)
