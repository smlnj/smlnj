(* alphaFreqProps.sml
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies
 *
 * Extract frequency information from the Alpha architecture
 * 
 * -- Allen
 *)

functor AlphaFreqProps(AlphaInstr : ALPHAINSTR) : FREQUENCY_PROPERTIES =
struct

   structure I = AlphaInstr

   val p10 = Probability.percent 10
   val p50 = Probability.percent 50
   val p90 = Probability.percent 90
   val p100 = Probability.always

   fun alphaBranchProb(I.BRANCH{b=I.BR, ...}) = p100 (* unconditional *)
     | alphaBranchProb(I.BRANCH{b=I.BEQ, ...}) = p10
     | alphaBranchProb(I.BRANCH{b=I.BNE, ...}) = p90
     | alphaBranchProb(I.FBRANCH{b=I.FBEQ, ...}) = p10 
     | alphaBranchProb(I.FBRANCH{b=I.FBNE, ...}) = p90
     | alphaBranchProb(I.BRANCH _) = p50 (* default *)
     | alphaBranchProb(I.FBRANCH _) = p50 (* default *)
     | alphaBranchProb(I.JMPL(_,[])) = p100 (* unconditional *)
     | alphaBranchProb(I.JMPL(_,labs)) =
	Probability.prob(1, length labs)  (* assume equal prob *)
     | alphaBranchProb(I.RET _) = p100
     | alphaBranchProb _ = Probability.never  (* non-branch *)

   fun branchProb(I.ANNOTATION{a, i, ...}) = 
        (case #peek MLRiscAnnotations.BRANCH_PROB a of
           SOME b => b
         | NONE => branchProb i
        )
     | branchProb(I.INSTR i) = alphaBranchProb(i)
     | branchProb _ = Probability.never  (* non-branch *)
end
