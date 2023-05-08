(* compiler/TopLevel/main/control.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Control : CONTROL =
struct

  val {newBool, newInt, newString, newStrings} = MakeControls.make {name = "CPS", priority = [1]}

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

  structure Basics = BasicsControl
  (* provides: val printWarnings = ref true *)

  structure Parser = ParserControl
  (* provides: val primaryPrompt = ref "- "
	       val secondaryPrompt = ref "= "
	       val overloadKW = ref false
	       val lazysml = ref false
	       val quotation = ref false
	       val setSuccML : bool -> unit
   *)

  val debugging = newBool ("debugging", "general debugging flag", false)
  val eldebugging = newBool ("eldebugging", "evalloop debugging", false)
  val pddebugging = newBool ("pddebugging", "PPDec debugging", false)
  val printAst = newBool ("printAst", "whether to print Ast representation", false)
  val interp = newBool ("interp", "?", false)

  val progressMsgs =
      newBool ("progressMsgs", "whether to print a message after each phase is completed", false)

  val preserveLvarNames = newBool ("preserve-names", "?", false)

  (* these are really all the same ref cell: *)
  val saveit : bool ref = ElabData.saveLvarNames
  val saveAbsyn : bool ref = saveit
  val saveLambda : bool ref = saveit
  val saveConvert : bool ref = saveit
  val saveCPSopt : bool ref = saveit
  val saveClosure : bool ref = saveit

  val tdp_instrument = TDPInstrument.enabled

end (* structure Control *)
