(* compiler/TopLevel/main/control.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Control : CONTROL =
struct

  local
      val priority = [10, 10, 9]
      val obscurity = 4
      val prefix = "control"

      val registry = ControlRegistry.new
                         { help = "miscellaneous control settings" }

      val _ = BasicControl.nest (prefix, registry, priority)

      val bool_cvt = ControlUtil.Cvt.bool
      val string_cvt = ControlUtil.Cvt.string

      val nextpri = ref 0

      fun register (cvtFn, name, help, defaultRef) = let
	    val p = !nextpri
	    val ctl = Controls.control {
		    name = name,
		    pri = [p],
		    obscurity = obscurity,
		    help = help,
		    ctl = defaultRef
		  }
	    in
	      nextpri := p + 1;
              ControlRegistry.register registry {
		  ctl = Controls.stringControl cvtFn ctl,
		  envName = SOME (ControlUtil.EnvName.toUpper "CONTROL_" name)
		};
	      defaultRef
	    end

    (* `new (n, h, d)` defines new control reference with default value `d`
     * and registers it with name `n` and help message `h`.
     *)
      fun new (n, h, d) = register (bool_cvt, n, h, ref d)

  in

    structure Print : PRINTCONTROL = PrintControl (* Basics/print/printcontrol.sml *)

    (* ElabData controls *)
    structure ElabData : ELABDATA_CONTROL = ElabDataControl (* ElabData/main/edcontrol.{sml,sig} *)

    (* elaborator controls *)
    structure Elab : ELAB_CONTROL = ElabControl (* Elaborator/control/elabcontrol.{sml,sig} *)

    (* Match compiler controls (used in Elatorator/matchcomp) *)
    structure MC : MC_CONTROL = MCControl (* Elaborator/control/mccontrol.{sml,sig} *)

    (* FLINT controls *)
    structure FLINT = FLINT_Control (* FLINT/main/control.{sml,sig} *)

    (* CPS controls *)
    structure CPS : CPSCONTROL = CPSControl (* CPS/main/control.{sml,sig} *)

    (* CodeGen controls *)
    structure CodeGen : CODEGENCONTROL = CodeGenControl (* CodeGen/main/control.{sml,sig} *)

    open BasicControl
    (* provides: val printWarnings = ref true *)

    open ParserControl
    (* provides: val primaryPrompt = ref "- "
		 val secondaryPrompt = ref "= "
		 val overloadKW = ref false
		 val lazysml = ref false
		 val quotation = ref false
                 val setSuccML : bool -> unit
     *)

    val debugging = new ("debugging", "?", false)
    val eldebugging = new ("eldebugging", "evalloop debugging", false)
    val pddebugging = new ("pddebugging", "PPDec debugging", false)
    val printAst = new ("printAst", "whether to print Ast representation", false)
    val printAbsyn = ElabControl.printAbsyn

    val interp = new ("interp", "?", false)

    val progressMsgs =
	new ("progressMsgs", "whether to print a message after each phase is completed", false)

(* trackExn and polyEqWarn moved to FLINT_Control, because used in FLINT/trans/translate.sml
    val trackExn =
	new ("track-exn", "whether to generate code that tracks exceptions", true)
    (* warning message when call of polyEqual compiled: *)
    val polyEqWarn =
	new ("poly-eq-warn", "whether to warn about calls of polyEqual", true)
*)

    val preserveLvarNames = new ("preserve-names", "?", false)

    (* these are really all the same ref cell: *)
    val saveit : bool ref = ElabData.saveLvarNames
    val saveAbsyn : bool ref = saveit
    val saveLambda : bool ref = saveit
    val saveConvert : bool ref = saveit
    val saveCPSopt : bool ref = saveit
    val saveClosure : bool ref = saveit

    val tdp_instrument = TDPInstrument.enabled

  end (* local *)

end (* structure Control *)
