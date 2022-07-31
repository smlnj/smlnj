(*
 * This module builds a CFG from a stream of instructions.
 * We use the FLOWGRPAH_GEN interface here, which is the 
 * default interface used by the core MLRISC.
 *
 * -- Allen
 *)

functor CFGGen
  (structure CFG : CONTROL_FLOW_GRAPH
   structure InsnProps : INSN_PROPERTIES
   structure MLTree : MLTREE
     sharing CFG.I = InsnProps.I
     sharing MLTree.Constant = InsnProps.I.Constant
     sharing MLTree.PseudoOp = CFG.P 
  ) : FLOWGRAPH_GEN =
struct

   structure I = CFG.I
   structure C = I.C
   structure S = MLTree.Stream
   structure T = MLTree
   structure P = CFG.P
   structure Builder = ControlFlowGraphGen
     (structure CFG = CFG
      structure Stream = S
      structure InsnProps = InsnProps
     )

   type flowgraph = CFG.cfg

   fun newStream{compile,flowgraph} =
   let val cfg = ref(case flowgraph of
                       NONE => CFG.new()
                     | SOME cfg => cfg
                    )
       val {stream,next} = Builder.builder(!cfg)
       val S.STREAM{beginCluster,endCluster,pseudoOp,emit,exitBlock,
                    getAnnotations,comment,annotation,
                    defineLabel,entryLabel,...} 
                      = stream
       fun endCFG a = 
       let val _      = endCluster a
           val oldCFG = !cfg
           val newCFG = CFG.new()
       in  cfg := newCFG;
           next newCFG;
           compile oldCFG
       end 

   in  S.STREAM{beginCluster   = beginCluster,
                endCluster     = endCFG,
                pseudoOp       = pseudoOp,
                emit           = emit,
                exitBlock      = exitBlock,
                comment        = comment,
                annotation     = annotation,
                getAnnotations = getAnnotations,
                defineLabel    = defineLabel,
                entryLabel     = entryLabel
               }
   end

end

