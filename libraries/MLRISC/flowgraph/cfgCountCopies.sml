(*
 * This module counts the number of copies (in bytes)  
 * generated after register allocation.  Mainly useful for fine-tuning.
 *)
functor CFGCountCopies
   ( structure CFG : CONTROL_FLOW_GRAPH
     structure InsnProps : INSN_PROPERTIES
     		where I = CFG.I
     structure SdiJumps : SDI_JUMPS
     		where I = CFG.I
    ) : CFG_OPTIMIZATION =
struct
   structure CFG = CFG
   structure G   = Graph

   val name = "count copies"

   val copies = MLRiscControl.mkCounter ("copies", "copy count")

   fun run (cfg as G.GRAPH graph) = let
     val blocks = map #2 (#nodes graph ())
     fun loc _ = 0
     fun count(CFG.BLOCK{insns, ...}, n) = let
       fun scan([], n) = n
	 | scan(i::is, n) = 
	    if InsnProps.moveInstr i then
	      scan(is, n + SdiJumps.sdiSize(i, loc, 0)) 
	    else scan(is, n)
     in  scan(!insns, n) 
     end
   in
       copies := !copies + foldr count 0 blocks;
       cfg
   end
end
