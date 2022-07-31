(* x86FreqProps.sml
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies
 *
 * Extract frequency information from the X86 architecture
 *
 * -- Allen
 *)
functor X86FreqProps(X86Instr : X86INSTR) : FREQUENCY_PROPERTIES =
struct

   structure I = X86Instr

   val p0_001 = Probability.prob(1,1000)
   val p10 = Probability.percent 10
   val p50 = Probability.percent 50
   val p90 = Probability.percent 90
   val p100 = Probability.always

   fun x86BranchProb(I.JCC{cond=I.EQ,...}) = p10
     | x86BranchProb(I.JCC{cond=I.O,...}) = p0_001
     | x86BranchProb(I.JCC{cond=I.NE,...}) = p90
     | x86BranchProb(I.JCC{cond=I.NO,...}) = p100
     | x86BranchProb(I.JCC{cond=I.P,...}) = p0_001 (* fp unordered test *)
     | x86BranchProb(I.JCC{cond=I.NP,...}) =  p100
     | x86BranchProb(I.JCC _) = p50 (* default *)
     | x86BranchProb(I.JMP _) = p100 
     | x86BranchProb _ = Probability.never (* non-branch *)

   and branchProb(I.ANNOTATION{a, i, ...}) = 
        (case #peek MLRiscAnnotations.BRANCH_PROB a of
           SOME b => b
         | NONE => branchProb i
        )
     | branchProb (I.INSTR i) = x86BranchProb i
     | branchProb _ = Probability.never

end

