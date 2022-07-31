(* cfgEmit.sml
 *
 * COPYRIGHT (c) 2001 Bell Labs, Lucent Technologies
 *
 * This module takes a flowgraph and an assembly emitter module and 
 * ties them together into one.  The output is sent to AsmStream.
 *  --Allen
 *
 * TODO: Need to check for the REORDER/NOREORDER annotation on
 * blocks and call P.Client.AsmPseudoOps.toString function to
 * print out the appropriate assembler directive. -- Lal.
 *)

functor CFGEmit
  (structure E   : INSTRUCTION_EMITTER
   structure CFG : CONTROL_FLOW_GRAPH
		   where I = E.I
		     and P = E.S.P)  : ASSEMBLY_EMITTER = 
struct
  structure CFG = CFG

  fun asmEmit (Graph.GRAPH graph, blocks) = let
	val CFG.INFO{annotations=an, data, decls, ...} = #graph_info graph
	val E.S.STREAM{pseudoOp,defineLabel,emit,annotation,comment,...} = 
             E.makeStream (!an)
	fun emitIt (id, CFG.BLOCK{labels, annotations=a, align, insns, ...}) = (
              case !align of NONE => () | SOME p => (pseudoOp p);
	      List.app defineLabel (!labels); 
	      List.app emitAn (!a);
	      List.app emit (rev (!insns)))
	and emitAn a = if Annotations.toString a = "" then () else annotation(a)
	in
	  List.app emitAn (!an);
	  List.app pseudoOp (rev (!decls));
	  pseudoOp(PseudoOpsBasisTyp.TEXT);
	  List.app emitIt blocks;
	  List.app pseudoOp (rev (!data))
	  
	end
end










