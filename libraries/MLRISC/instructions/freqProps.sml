(* freqProps.sml
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies
 *
 * Generic module for extracting the frequency information.
 *)

functor FreqProps (Props : INSN_PROPERTIES) : FREQUENCY_PROPERTIES =
  struct

    structure I = Props.I

    val fifty_fifty = Probability.prob(1, 2)
    val get = #get MLRiscAnnotations.BRANCH_PROB

  (* Branch probability *)
    fun branchProb instr = (case get(#2(Props.getAnnotations instr))
	   of SOME b => b
	    | NONE => fifty_fifty
	  (* end case *))

  end
