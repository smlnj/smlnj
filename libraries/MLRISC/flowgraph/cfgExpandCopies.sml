(* cfgExpandCopies.sml
 *
 * COPYRIGHT (c) 2001 Bell Labs, Lucent Technologies
 *
 * This module expands all parallel copies into normal instructions
 *)

functor CFGExpandCopies
   (structure CFG    : CONTROL_FLOW_GRAPH
    structure Shuffle : SHUFFLE
    			where I = CFG.I
   ) : CFG_OPTIMIZATION =
  struct
    structure CFG = CFG
    structure I = CFG.I

    val name = "expand copies"

    fun run (cfg as Graph.GRAPH graph) = let
      fun expand(I.COPY{k,  dst, src, tmp, ...}) = let
	    val shuffle = 
		case k 
                  of CellsBasis.GP => Shuffle.shuffle 
                   | CellsBasis.FP => Shuffle.shufflefp
		   | _ =>  MLRiscErrorMsg.error ("CFGExpandCopies", "shuffle")
          in shuffle{dst=dst, src=src, tmp=tmp}
          end
	| expand(I.ANNOTATION{i,a}) = 
	    map (fn i => I.ANNOTATION{i=i, a=a}) (expand i)
	| expand i = [i]
      
      fun expandInstrs(_, CFG.BLOCK{insns, ...}) = 
	  insns := 
	    List.foldr 
	      (fn (i, rest) => List.revAppend(expand(i), rest))
	      []
	      (!insns)
    in
      #forall_nodes graph expandInstrs;
      cfg
    end
end
