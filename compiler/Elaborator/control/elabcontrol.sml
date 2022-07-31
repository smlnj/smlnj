(* elabcontrol.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Flags controlling the elaborator.
 *)

structure ElabControl : ELAB_CONTROL =
  struct

    local
      val priority = [10, 10, 7]
      val clear = 2
      val obscure = 6
      val prefix = "elab"

      val registry = ControlRegistry.new { help = "elaborator flags" }

      val _ = BasicControl.nest (prefix, registry, priority)

      val nextpri = ref 0

      fun new ob (n, h, d) = let
	    val r = ref d
	    val p = !nextpri
	    val ctl = Controls.control {
		    name = n,
		    pri = [p],
		    obscurity = ob,
		    help = h,
		    ctl = r
		  }
	    in
	      nextpri := p + 1;
	      ControlRegistry.register registry {
		  ctl = Controls.stringControl ControlUtil.Cvt.bool ctl,
		  envName = SOME (ControlUtil.EnvName.toUpper "ELAB_" n)
		};
	      r
	    end

      val cnew = new clear
      val onew = new obscure
    in

    val etdebugging = onew ("et-debugging", "ElabType debugging", false)
        (* ElabType *)
    val esdebugging = onew ("es-debugging", "ElabSig debugging", false)
        (* ElabSig *)
    val insdebugging = onew ("ins-debugging", "Instantiate debugging", false)
        (* Instantiate *)
    val smdebugging = onew ("sm-debugging", "Sigmatch debugging", false)
        (* Sigmatch *)
    val ecdebugging = onew ("ec-debugging", "ElabCore debugging", false)
        (* ElabCore *)
    val emdebugging = onew ("em-debugging", "ElabMod debugging", false)
        (* ElabMod *)
    val tcdebugging = onew ("tc-debugging", "TypeCheck debugging", false)
        (* Typecheck *)
    val unidebugging = onew ("uni-debugging", "Unify debugging", false)
        (* Unify *)
    val ovlddebugging = onew ("ovld-debugging", "Overload debugging", false)
        (* Unify *)
    val instantiateSigs = onew ("instantiate-sigs", "instantiate all sigs", true)
        (* ElabMod, Control_MC *)
    val etopdebugging = onew ("etop-debugging", "ElabTop debugging", false)
        (* ElabTop *)

    val markabsyn = onew ("markabsyn", "mark abstract syntax", true)
        (* ElabCore, ElabTop, ElabUtil, Control_MC *)

    val printAbsyn = onew ("printAbsyn", "absyn print mode", false)
    val stats = onew ("stats", "match compiler timings and stats", false)


  (***** Controls for warning messages *****)

(* NOTE: we currently disable this unusedWarn check because of false positives for
 * mutually recursive functions.  The false positives are caused by a
 * transformation done during type checking, which should be removed
 * at some point.  CHECK if problem solved. *)
    val unusedWarn = cnew (
	  "unused-warn",
	  "warn when variables are defined but not used",
	  false)

    val multDefWarn = cnew (
	  "mult-def-warn",
	  "warn on multiple defs",
	  false)
        (* Instantiate, Control_MC (TopLevel/main/control.sml) *)

    val shareDefError = cnew (
	  "share-def-error",
	  "check share defs",
	  true)
        (* Instantiate, Control_MC *)

    val valueRestrictionLocalWarn = cnew (
	  "value-restriction-local-warn",
	  "warn on value restriction for local defs",
	  false)

    val valueRestrictionTopWarn = cnew (
	  "value-restriction-top-warn",
	  "warn on value restriction at top level", true)

    val showTypeErrorCulprits = cnew (
	  "show-type-error-culprits",
	  "show culprits in type error messages",
	  false)

    end (* local *)

  end (* structure ElabControl *)
