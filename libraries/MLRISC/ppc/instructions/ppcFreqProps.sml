(* ppcFreqProps.sml
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies
 *
 * Extract frequency information from the PowerPC architecture
 *
 * -- Allen
 *)

functor PPCFreqProps(PPCInstr : PPCINSTR) : FREQUENCY_PROPERTIES =
struct

   structure I = PPCInstr

   val p10 = Probability.percent 10
   val p50 = Probability.percent 50
   val p90 = Probability.percent 90
   val p100 = Probability.always

   fun ppcBranchProb(I.BC _) = p50
     | ppcBranchProb(I.BCLR{labels=[],bo=I.ALWAYS,...}) = p100
     | ppcBranchProb(I.BCLR{labels,bo=I.ALWAYS,...}) =
	Probability.prob(1, length labels)
     | ppcBranchProb(I.BCLR{labels=[],bo,...}) = p50
     | ppcBranchProb(I.BCLR{labels,bo,...}) =
	Probability.prob(1, length labels)
     | ppcBranchProb _ = Probability.never (* non-branch *)

   fun branchProb(I.ANNOTATION{a, i, ...}) =
        (case #peek MLRiscAnnotations.BRANCH_PROB a of
           SOME b => b
         | NONE => branchProb i
        )
     | branchProb(I.INSTR(i)) = ppcBranchProb(i)
     | branchProb _ = Probability.never

end
