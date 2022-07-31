(* compute-execution-freqs.sig
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies.
 *)

signature COMPUTE_EXECUTION_FREQUENCIES =
  sig

    structure CFG : CONTROL_FLOW_GRAPH

    val compute : CFG.cfg -> unit

  end
