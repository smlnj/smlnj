(* code-generator.sig
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * Generation of code from the FLINT intermediate form (after all FLINT passes
 * have completed).  This process is split into two phases: first translation of
 * FLINT (via CPS) to the CFG IR, followed by translation of the CFG IR to native
 * code.
 *)

signature CODE_GENERATOR =
  sig

    type 'a csegments = {
        code : 'a,
        lits : CodeObj.literals
      }

    (* compile FLINT IR to the CFG IR *)
    val compileToCFG : { source : string, prog : FLINT.prog } -> CFG.comp_unit csegments

    (* compile CFG IR to native code *)
    val compileToNative : CFG.comp_unit -> CodeObj.native_code

  (* the following are used by CM *)
    val architecture : string
    val abi_variant : string option

  end (* CODE_GENERATOR *)
