(*
 * SSA optimizer for doing experiments 
 *)

functor SSAOptimizer
   (structure Asm : INSTRUCTION_EMITTER
    structure MLTreeComp : MLTREECOMP
    structure F  : FLOWGRAPH
    structure P  : INSN_PROPERTIES
    structure SP : SSA_PROPERTIES
    structure OperandTable : OPERAND_TABLE
    structure GCTypeSys : GC_TYPE_SYSTEM
    structure FreqProps : FREQUENCY_PROPERTIES
       sharing P.I = SP.I = Asm.I = F.I = OperandTable.I =
               FreqProps.I = MLTreeComp.I 
       sharing F.P = Asm.P = MLTreeComp.T.PseudoOp 
       sharing MLTreeComp.T.Constant = F.I.Constant
       sharing SP.RTL = GCTypeSys.RTL
    type sext and rext and fext and ccext
    val callgc : { id     : int,
                   msg    : string,
                   gcLabel  : Label.label,
                   returnLabel  : Label.label,
                   roots  : (P.I.C.cell * GCTypeSys.GC.gctype) list,
                   stream : (sext,rext,fext,ccext) MLTreeComp.mltreeStream
                 } -> unit
   ) : SSA_OPTIMIZER =
struct

   structure F = F
   structure I = F.I

   val view_IR    = MLRiscControl.getFlag "view-IR" 
   val verbose    = MLRiscControl.getFlag "verbose"
   val min_blocks = MLRiscControl.getInt "min-blocks"

   fun error msg = MLRiscErrorMsg.error("SSAOptimizer",msg)

   structure GraphViewer = GraphViewer(AllDisplays)

   structure FormatInsn = FormatInstruction(Asm)

   structure CFG = ControlFlowGraph
      (structure I = I
       structure P = F.P
       structure GraphImpl = DirectedGraph
       structure Asm = Asm
      )

   structure Util = CFGUtil
      (structure CFG = CFG
       structure P   = P
      )

   structure CFG2Cluster = CFG2Cluster
      (structure CFG  = CFG
       structure F    = F
      )

   structure Cluster2CFG = Cluster2CFG
      (structure CFG  = CFG
       structure Util = Util
       structure F    = F
       structure P    = P
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
     (structure IR = IR
      structure Props = P
      structure FreqProps = FreqProps
      val loopMultiplier=10
     )
      
   structure Liveness = LivenessAnalysis(CFG)

   structure SSA = SSA
      (structure CFG  = CFG 
       structure Dom  = Dom
       structure SP   = SP
       structure Props= P
       structure RTL  = SP.RTL
       structure FormatInsn = FormatInsn
       structure GraphImpl = DirectedGraph
       structure GCMap = GCTypeSys.GCMap
      )
      
   structure CFG2SSA = CFG2SSA
      (structure SSA = SSA
       structure Liveness = Liveness
      )

   structure Reshape = ReshapeBranches(structure IR = IR
                                       structure P  = P)
   structure BranchChaining = BranchChaining(structure IR = IR
                                             structure P  = P)

   structure InsertPreheaders = InsertPreheaders(structure IR = IR
                                                 structure P  = P)

   structure SSADCE = SSADeadCodeElim(SSA)

   structure CF  = SSAConstantFolding(SSA)

   structure GVN = SSAGlobalValueNumbering(CF)

   structure CCP = SSACondConstProp(CF)

   structure SSAGVN = SSAGVN(structure GVN = GVN 
                             val leaveBehindCopy = false
                             val foldGlobalConstants = true)

   structure SSAGVNL = SSAGVN(structure GVN = GVN 
                              val leaveBehindCopy = false
                              val foldGlobalConstants = false)

   structure SSAGVN' = SSAGVN(structure GVN = GVN 
                              val leaveBehindCopy = false
                              val foldGlobalConstants = true)

   structure SSACCP = SSACCP(CCP)

   structure SSAGCM = SSAGlobalCodeMotion(SSA)
   (* structure SSAGCM2 = SSAGlobalCodeMotion2(SSA) *)
   (* structure Depressurize = SSADepressurize(SSA)*)

   structure SSALiveness = SSALiveness(SSA)

   structure SSA2CFG = SSA2CFG
      (structure SSA      = SSA
       structure Liveness = SSALiveness
       structure Props    = P
       structure Util     = Util
      ) 

   structure GCInvariants = GCInvariants
      (structure IR = IR
       structure Props = P
       structure RTLProps = SP.RTLProps
       structure OperandTable = OperandTable
       structure TypeSys = GCTypeSys
      )

   structure SSAGCInvariants = SSAGCInvariants
      (structure SSA     = SSA
       structure TypeSys = GCTypeSys
      )

   structure GCGen = GCGen
      (structure MLTreeComp = MLTreeComp
       structure IR = IR
       structure GCMap = GCTypeSys.GCMap
       structure InsnProps = P
      )

   fun view phase ir = if !view_IR then IR.view phase ir else ()

   fun optimize cluster =
   let datatype rep = IR of IR.IR
                    | CLUSTER of F.cluster
                    | SSA of SSA.ssa
       fun doPhase "cluster->cfg" (CLUSTER c) = IR(Cluster2CFG.cluster2cfg c)
         | doPhase "cfg->cluster" (IR cfg) = 
            CLUSTER(CFG2Cluster.cfg2cluster{cfg=cfg,relayout=false})
         | doPhase "guess" (r as IR ir) = (Guess.run ir; r)
         | doPhase "reshape"   (r as IR ir) = (Reshape.run ir; r)
         | doPhase "branch-chaining" (r as IR ir) = (BranchChaining.run ir; r)
         | doPhase "insert-preheaders" (r as IR ir) = 
             (InsertPreheaders.run ir; r)
         | doPhase "split-critical-edges" (r as IR ir) = 
             (Util.splitAllCriticalEdges ir; r)
         | doPhase "view-cfg"  (r as IR ir) = (view "cfg" ir; r)
         | doPhase "view-dom"  (r as IR ir) = (view "dom" ir; r)
         | doPhase "view-doms" (r as IR ir) = (view "doms" ir; r)
         | doPhase "view-cdg"  (r as IR ir) = (view "cdg" ir; r)
         | doPhase "view-loop" (r as IR ir) = (view "loop" ir; r)
         | doPhase "view-ssacfg"  (r as SSA ssa) = 
            (if !view_IR then GraphViewer.view (SSA.viewAsCFG ssa) else (); r)
         | doPhase "view-ssa"  (r as SSA ssa) = 
            (if !view_IR then GraphViewer.view (SSA.viewAsSSA ssa) else (); r)
         | doPhase "cfg->ssa"  (IR ir)   = SSA(CFG2SSA.buildSSA(ir,IR.dom ir))
         | doPhase "ssa-dce"   (SSA ssa) = SSA(SSADCE.optimize ssa)
         | doPhase "ssa-gvn"   (SSA ssa) = SSA(SSAGVN.optimize ssa)
         | doPhase "ssa-gvnl"  (SSA ssa) = SSA(SSAGVNL.optimize ssa)
         | doPhase "ssa-gvn'"  (SSA ssa) = SSA(SSAGVN'.optimize ssa)
         | doPhase "ssa-gcm"   (SSA ssa) = SSA(SSAGCM.optimize ssa)
         | doPhase "ssa-ccp"   (SSA ssa) = SSA(SSACCP.optimize ssa)
         | doPhase "ssa-gc-invariants" (SSA ssa) =
              SSA(SSAGCInvariants.optimize ssa)
         (* | doPhase "ssa-gcm2"  (SSA ssa) = SSA(SSAGCM2.optimize ssa) *)
         (* | doPhase "ssa-dep"   (SSA ssa) = SSA(Depressurize.optimize ssa)*)
         | doPhase "gvn"       (r as SSA ssa) =
              (GVN.computeValueNumbers ssa; r)
         | doPhase "ssa->cfg"  (SSA ssa) = IR(SSA2CFG.buildCFG ssa)
         | doPhase "gc-invariants" (r as IR ir) = (GCInvariants.run ir; r)
         | doPhase "gc-gen"    (r as IR ir) = 
              (GCGen.gcGen{callgc=callgc} ir; r)
         | doPhase phase _ = error(phase)

       fun doPhases [] (CLUSTER c) = c
         | doPhases [] _ = error "cluster needed"
         | doPhases (phase::phases) ir = 
            let fun pr msg = TextIO.output(TextIO.stdErr,msg) 
                val _  = if !verbose then pr("[ start "^phase^"]") else (); 
                val timer = Timer.startCPUTimer()
                val ir = doPhase phase ir handle e =>
                     (print("[ "^phase^": uncaught exception: "
                            ^exnName e^" ]\n"); raise e)
                val {gc,sys,usr} = Timer.checkCPUTimer timer
                val _  = if !verbose then 
                         pr("[ end "^phase^" usr="^Time.toString usr^
                            " sys="^Time.toString sys^
                            " gc="^Time.toString gc^"]\n") else ();
            in  doPhases phases ir end
       
       val F.CLUSTER{blocks,...} = cluster
       fun isAllGC([],gc,n) = (gc,n)
         | isAllGC(F.BBLOCK{succ,pred,...}::bs,gc,n) =  
               isAllGC(bs,gc andalso (case (!succ,!pred) of
                                        ([_],[_]) => true | _ => false),n+1)
         | isAllGC(_::bs,gc,n) = isAllGC(bs,gc,n) 
   in  case isAllGC(blocks,true,0) of
         (true,_) => cluster
       | (false,n) =>
         if n >= !min_blocks then
            doPhases (!MLRiscControl.mlrisc_phases) (CLUSTER cluster)
         else
            cluster
   end

   fun codegen cluster = 
       if !MLRiscControl.mlrisc then optimize cluster else cluster

end
