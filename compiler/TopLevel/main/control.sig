(* control.sig
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* main Control structure *)
signature CONTROL =
  sig
    structure Print : PRINTCONTROL
    structure ElabData : ELABDATA_CONTROL  (* ElabData controls *)
    structure Elab : ELAB_CONTROL  (* Elaborator controls *)
    structure MC : MC_CONTROL  (* match compiler controls *)
    structure FLINT : FLINTCONTROL
    structure CPS : CPSCONTROL			  
    structure CodeGen : CODEGENCONTROL
    val debugging : bool ref
    val eldebugging : bool ref (* EvalLoopF debugging *)
    val pddebugging : bool ref (* PPDec debugging *)
    val printAst : bool ref
    val printAbsyn : bool ref

    include BASIC_CONTROL
    (* provides: val printWarnings : bool ref
     *)
    include PARSER_CONTROL
    (* provides: val primaryPrompt : string ref
		 val secondaryPrompt : string ref
		 val overloadKW : bool ref
		 val lazysml : bool ref
		 val quotation : bool ref
     *)

    val interp : bool ref
       (* turn on interpreter -- defunct *)

    val progressMsgs : bool ref
       (* turn on printing of progress messages at end of major stages in evalloop *)

    val saveLambda : bool ref
    val preserveLvarNames : bool ref

    val saveit : bool ref
    val saveAbsyn : bool ref
    val saveConvert : bool ref
    val saveCPSopt : bool ref
    val saveClosure : bool ref

    val tdp_instrument : bool ref

  end (* signature CONTROL *)

(* NOTES: trackExn and polyEqWarn moved to FLINT_Control -- used in trans/translate.sml *)
      
