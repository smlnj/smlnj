(*
 * Extract frequency information from the MIPS architecture
 * 
 * -- Allen
 *)

functor MIPSFreqProps(MIPSInstr : MIPSINSTR) : FREQUENCY_PROPERTIES =
struct

   structure I = MIPSInstr

   fun branchProb(I.ANNOTATION{a, i, ...}) =
        (case #peek MLRiscAnnotations.BRANCH_PROB a of
           SOME b => b
         | NONE => branchProb i
        )
     | branchProb(I.BRANCH{likely,...}) = 50 (* default *)
     | branchProb(I.FBRANCH{likely,...}) = 50 (* default *)
     | branchProb(I.J _) = 100 (* unconditional *)
     | branchProb(I.JR{labels,...}) = 100 div length labels (* assume equal prob *)
     | branchProb _ = 0 (* non-branch *)

end
