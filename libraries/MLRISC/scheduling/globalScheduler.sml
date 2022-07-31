(*
 * A top level functor and ties all the modules for global scheduling 
 * together.
 *) 

functor GlobalScheduler
   (structure IR         : MLRISC_IR
    structure DDG        : SCHEDULER_DDG
    structure InsnProps  : INSN_PROPERTIES
    structure RTLProps   : RTL_PROPERTIES
    structure Viewer     : GLOBAL_SCHEDULER_DDG_VIEWER
    structure FormatInsn : FORMAT_INSTRUCTION
    (* structure Rewrite : REWRITE_INSTRUCTIONS *)
       sharing InsnProps.I = IR.I = DDG.I = (* = Rewrite.I *)
               FormatInsn.I = RTLProps.I
       sharing Viewer.IR = IR
       sharing Viewer.DDG = DDG
   ) : MLRISC_IR_OPTIMIZATION =
struct
   structure IR         = IR
   structure CFG        = IR.CFG
   structure I          = IR.I
   structure SchedProps = DDG.SchedProps
   structure SL         = SortedList
   structure G          = Graph
   structure A          = Array

   structure RegionBuilder = RegionBuilder(IR)

   structure DDGBuilder = 
     SchedulerDDGBuilder
     (structure DDG        = DDG
      structure CFG        = CFG
      structure InsnProps  = InsnProps
      structure RTLProps   = RTLProps
     )
   
   structure ListScheduler = 
     ListScheduler
       (structure IR         = IR
        structure DDG        = DDG
        structure InsnProps  = InsnProps
        structure FormatInsn = FormatInsn
        (* structure Rewrite    = Rewrite *)
       )

   structure GlobalCP = GlobalCriticalPath(DDG) 

   structure DAGScheduling = 
     DAGScheduling
       (structure ListScheduler = ListScheduler
        structure DDGBuilder    = DDGBuilder
        structure Viewer        = Viewer
        structure Ranks         = GlobalCP
       )

   structure Liveness = LivenessAnalysis(CFG)

   type flowgraph = IR.IR

   val name = "global scheduling"

   val cpu = MLRiscControl.getString "cpu"
   val _ = cpu := "default"

   fun computeLiveness(cpu_info,CFG as G.GRAPH cfg) = 
   let val SchedProps.CPU_INFO{defUse=insnDefUse, ...} = cpu_info
       val regmap = I.C.lookup(CFG.regmap CFG) 
       fun defUse(_,CFG.BLOCK{insns, ...}) = 
       let fun scan([], def, use) = (def, use)
             | scan(i::is, def, use) =
               let val (d,u) = insnDefUse i
                   val u     = SL.uniq(map regmap u)
                   val u'    = SL.difference(u, def)
                   val use'  = SL.merge(u', use)
                   val d     = SL.uniq(map (fn (r,l) => regmap r) d)
                   val d'    = SL.difference(d, use')
                   val def'  = SL.merge(d', def)
               in  scan(is, def', use')
               end
       in  scan(rev(!insns), [], [])
       end

       fun liveOut(_, block) = 
             map regmap (I.C.CellSet.toCellList (CFG.liveOut block))

       fun result{block=(_,CFG.BLOCK{annotations, ...}), liveIn, liveOut} = 
           annotations :=
              #set DDG.LIVENESS ({liveIn=liveIn, liveOut=liveOut}, !annotations)
          
   in  Liveness.liveness{cfg=CFG,defUse=defUse,liveOut=liveOut, result=result}
   end

   fun run IR = 
   let val maxBlocks=100
       val maxInstrs=1000
       val minFreqRatio=0.01
       val traceOnly=false
       val sideEntries=true
       val internalBackEdges=false
       val insertDummyBlocks=false
       val params = {maxBlocks = maxBlocks,
                     maxInstrs = maxInstrs,
                     traceOnly = traceOnly,
                     minFreqRatio = minFreqRatio,
                     sideEntries = sideEntries,
                     internalBackEdges = internalBackEdges,
                     insertDummyBlocks = insertDummyBlocks
                    }
       val cpu_info = SchedProps.info{backward=false,cpu=SchedProps.cpu(!cpu)}
   in  computeLiveness(cpu_info,IR);
       RegionBuilder.regionBuilder params IR (DAGScheduling.schedule cpu_info);
       IR
   end

end
