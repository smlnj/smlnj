(* hppaFreqProps.sml
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies
 *
 * Extract frequency properties from the HP architecture
 * 
 * -- Allen
 *)

functor HppaFreqProps(HppaInstr : HPPAINSTR): FREQUENCY_PROPERTIES =
struct

   structure I = HppaInstr

   val p10 = Probability.percent 10
   val p50 = Probability.percent 50
   val p90 = Probability.percent 90
   val p100 = Probability.always

   fun hppaBranchProb(I.BCOND{cmp=I.COMBT,bc=I.EQ,...}) = p10
     | hppaBranchProb(I.BCOND{cmp=I.COMBF,bc=I.EQ,...}) = p90
     | hppaBranchProb(I.BCOND{cmp=I.COMBT,bc=I.NE,...}) = p90
     | hppaBranchProb(I.BCOND{cmp=I.COMBF,bc=I.NE,...}) = p10
     | hppaBranchProb(I.BCONDI{cmpi=I.COMIBT,bc=I.EQ,...}) = p10
     | hppaBranchProb(I.BCONDI{cmpi=I.COMIBF,bc=I.EQ,...}) = p90
     | hppaBranchProb(I.BCONDI{cmpi=I.COMIBT,bc=I.NE,...}) = p90
     | hppaBranchProb(I.BCONDI{cmpi=I.COMIBF,bc=I.NE,...}) = p10
     | hppaBranchProb(I.BCOND _) = p50 (* default *)
     | hppaBranchProb(I.BCONDI _) = p50 (* default *)
     | hppaBranchProb(I.FBRANCH _) = p50 (* default *)
     (*| hppaBranchProb(I.BB{bc=I.BCLR, p=31, ...}) = 10 
     | hppaBranchProb(I.BB{bc=I.BSET, p=31, ...}) = 90 *)
     | hppaBranchProb(I.BB _) = p50 (* branch on bit *)
     | hppaBranchProb(I.B _) = p100 (* unconditional *)
     | hppaBranchProb(I.BE{labs=[], ...}) = p100 (* escapes *)
     | hppaBranchProb(I.BE{labs,...}) =
	Probability.prob(1, length labs) (* assume equal prob *)
     | hppaBranchProb(I.BV{labs=[],...}) = p100 (* escapes *)
     | hppaBranchProb(I.BV{labs,...}) =
	Probability.prob(1, length labs) (* assume equal prob *)
     | hppaBranchProb(I.BLR{labs,...}) =
	Probability.prob(1, length labs) (* assume equal prob *)
     | hppaBranchProb _ = Probability.never (* non-branch *)
   fun branchProb(I.ANNOTATION{a, i, ...}) =
         (case #peek MLRiscAnnotations.BRANCH_PROB a of
            SOME b => b 
          | NONE => branchProb i
         )
     | branchProb(I.INSTR(i)) = hppaBranchProb(i)
     | branchProb _ = Probability.never

end
