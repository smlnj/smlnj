(*
 * Simple module for building the IR etc.  Doesn't do any real optimizations.
 *
 * -- Allen
 *)

functor MLRISCGlue
   (structure Asm       : INSTRUCTION_EMITTER
    structure Flowgraph : FLOWGRAPH
    structure InsnProps : INSN_PROPERTIES
    structure FreqProps : FREQUENCY_PROPERTIES
       sharing InsnProps.I = Asm.I = Flowgraph.I = FreqProps.I
       sharing Flowgraph.P = Asm.P 
   ) : MLRISC_GLUE =
struct

   structure F = Flowgraph
   structure I = F.I
 
   val mlrisc  = MLRiscControl.getFlag       "mlrisc"
   val phases  = MLRiscControl.getStringList "mlrisc-phases"
   val view_IR = MLRiscControl.getFlag       "view-IR"
   val verbose = MLRiscControl.getFlag       "verbose"

   fun error msg = MLRiscErrorMsg.error("MLRISCGlue",msg)

   structure GraphViewer = GraphViewer(AllDisplays)

   structure FormatInsn = FormatInstruction(Asm)

   structure CFG = ControlFlowGraph
      (structure I         = I
       structure PseudoOps = F.P
       structure GraphImpl = DirectedGraph
       structure InsnProps = InsnProps
       structure Asm = Asm
      )

   structure Util = CFGUtil
      (structure CFG       = CFG
       structure InsnProps = InsnProps
      )

   structure CFG2Cluster = CFG2Cluster
      (structure CFG       = CFG
       structure Util      = Util
       structure Flowgraph = Flowgraph
      )

   structure Cluster2CFG = Cluster2CFG
      (structure CFG       = CFG
       structure Util      = Util
       structure Flowgraph = Flowgraph
       structure InsnProps = InsnProps
      )
       
   structure Dom = DominatorTree(DirectedGraph)

   structure CDG = ControlDependenceGraph
      (structure Dom       = Dom
       structure GraphImpl = DirectedGraph
      )

   structure Loop = LoopStructure
      (structure Dom       = Dom
       structure GraphImpl = DirectedGraph
      )

   structure IR = MLRISC_IR
      (structure CFG         = CFG
       structure CDG         = CDG
       structure Loop        = Loop
       structure GraphViewer = GraphViewer
       structure Util        = Util
      )

   structure Guess = StaticBranchPrediction
      (structure IR        = IR
       structure InsnProps = InsnProps
       structure FreqProps = FreqProps
       val loopMultiplier=10
      )
      
   structure Liveness = LivenessAnalysis(CFG)

   structure Reshape = ReshapeBranches
     (structure IR        = IR
      structure InsnProps = InsnProps
     )

   structure BranchChaining = BranchChaining
     (structure IR        = IR
      structure InsnProps = InsnProps
     )

   structure CPR = CriticalPathReduction
     (structure IR        = IR
      structure InsnProps = InsnProps
     )

   structure ClusterGraph = ClusterGraph(Flowgraph)

   structure ClusterViewer = ClusterViewer
     (structure GraphViewer = GraphViewer
      structure ClusterGraph = ClusterGraph
      structure Asm          = Asm
     )

   fun view phase ir = if !view_IR then IR.view phase ir else ()
   fun view' cluster = if !view_IR then 
      ClusterViewer.view(ClusterGraph.clusterGraph cluster) else ()

   fun optimize cluster =
   let datatype rep = IR of IR.IR
                    | CLUSTER of F.cluster
       fun doPhase "cluster->cfg" (CLUSTER c) = IR(Cluster2CFG.cluster2cfg c)
         | doPhase "cfg->cluster" (IR cfg) = 
            CLUSTER(CFG2Cluster.cfg2cluster{cfg=cfg,relayout=false})
         | doPhase "guess" (r as IR ir) = (Guess.run ir; r)
         | doPhase "reshape"   (r as IR ir) = (Reshape.run ir; r)
         | doPhase "branch-chaining" (r as IR ir) = (BranchChaining.run ir; r)
         | doPhase "cpr"   (r as IR ir) = (CPR.run ir; r)
         | doPhase "view-cfg"  (r as IR ir) = (view "cfg" ir; r)
         | doPhase "view-dom"  (r as IR ir) = (view "dom" ir; r)
         | doPhase "view-pdom" (r as IR ir) = (view "pdom" ir; r)
         | doPhase "view-doms" (r as IR ir) = (view "doms" ir; r)
         | doPhase "view-cdg"  (r as IR ir) = (view "cdg" ir; r)
         | doPhase "view-loop" (r as IR ir) = (view "loop" ir; r)
         | doPhase "view-cluster" (r as CLUSTER c) = (view' c; r)
         | doPhase phase _ = error(phase)
       fun doPhases [] (CLUSTER c) = c
         | doPhases [] _ = error "cluster needed"
         | doPhases (phase::phases) ir = 
            (if !verbose then print("["^phase^"]\n") else (); 
             doPhases phases (doPhase phase ir))
   in  doPhases (!phases) (CLUSTER cluster)
   end

   fun codegen cluster = if !mlrisc then optimize cluster else cluster

end
