(* code-generator.sig
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Generation of code from the FLINT intermediate form (after all FLINT passes
 * have completed)
 *)

signature CODE_GENERATOR =
  sig

    (* compile FLINT IR to the CFG IR *)
    val compileToCFG : { source : string, prog : FLINT.prog } -> {
            code : CFG.comp_unit,
            lits : CodeObj.literals
          }

    (* compile CFG IR to native code *)
    val compileCFG : { code : CFG.comp_unit, lits : CodeObj.literals }
          -> CodeObj.csegments

    (* compile FLINT IR to native code *)
    val compile : { source : string, prog : FLINT.prog } -> CodeObj.csegments

  (* the following are used by CM *)
    val architecture : string
    val abi_variant : string option

  end (* CODE_GENERATOR *)
