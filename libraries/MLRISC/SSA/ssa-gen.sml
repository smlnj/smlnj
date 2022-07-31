(*
 * This module can be used to build a stripped down SSA graph directly from
 * an MLTREE stream.
 *)
functor SSAGen
   (structure MLTree : MLTREE
    structure CFG    : CONTROL_FLOW_GRAPH
    structure InsnProps : INSN_PROPERTIES
    structure SSA    : SSA 
      where CFG = CFG
      where I.Constant = MLTree.Constant
      sharing InsnProps.I = CFG.I
      sharing MLTree.Stream.P = CFG.P
   ) : FLOWGRAPH_GEN =
struct
   structure T = MLTree
   structure I = SSA.I
   structure C = I.C
   structure S = T.Stream
   structure Builder = ControlFlowGraphGen
      (structure CFG = CFG
       structure Stream = S
       structure InsnProps = InsnProps
      )
 
   type flowgraph = CFG.cfg

   fun error msg = MLRiscErrorMsg.error("SSAGen",msg)
   
   fun newStream{compile, flowgraph} =
   let val cfg = case flowgraph of
                   SOME cfg => cfg
                 | NONE => CFG.new(C.regmap())
       val {stream,next} = Builder.builder cfg
       val S.STREAM{beginCluster,endCluster,pseudoOp,
                    emit,exitBlock,comment,annotation,defineLabel,
                    entryLabel,alias,...} = stream

       fun endCFG a = (endCluster a;
                       next(CFG.new(C.regmap()));
                       compile cfg
                      )

       fun phi   _ = error "phi"
   in  S.STREAM{beginCluster= beginCluster,
                endCluster  = endCFG,
                pseudoOp    = pseudoOp,
                emit        = emit,
                exitBlock   = exitBlock,
                comment     = comment,
                annotation  = annotation,
                defineLabel = defineLabel,
                entryLabel  = entryLabel,
                alias       = alias,
                phi         = phi
               }
   end
end

