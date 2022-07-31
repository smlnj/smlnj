(* sparcFreqProps.sml
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies
 *
 * Extract frequency information from the sparc architecture
 * 
 * -- Allen
 *)

functor SparcFreqProps(SparcInstr : SPARCINSTR) : FREQUENCY_PROPERTIES =
struct

   structure I = SparcInstr

   val p10 = Probability.percent 10
   val p50 = Probability.percent 50
   val p90 = Probability.percent 90
   val p100 = Probability.always

   fun cond I.BA  = p100
     | cond I.BE  = p10
     | cond I.BNE = p90
     | cond _     = p50

   fun fcond I.FBA  = p100
     | fcond I.FBE  = p10
     | fcond I.FBNE = p90
     | fcond _      = p50

   fun sparcBranchProb(I.Bicc{b,...}) = cond b
     | sparcBranchProb(I.FBfcc{b,...}) = fcond b
     | sparcBranchProb(I.BP{b,...}) = cond b
     | sparcBranchProb(I.BR _) = p50
     | sparcBranchProb(I.JMP _) = p100
     | sparcBranchProb(I.RET _) = p100
     | sparcBranchProb _ = Probability.never (* non-branch *)

   fun branchProb(I.ANNOTATION{a, i, ...}) =
        (case #peek MLRiscAnnotations.BRANCH_PROB a of
           SOME b => b
         | NONE => branchProb i
        )
     | branchProb(I.INSTR(i)) = sparcBranchProb(i)
     | branchProb _ = Probability.never

end
