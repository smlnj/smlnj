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

    val compile : {
	    source : string,
	    prog : FLINT.prog
	  } -> CodeObj.csegments

  (* the following are used by CM *)
    val architecture : string
    val abi_variant : string option

  end (* CODE_GENERATOR *)
