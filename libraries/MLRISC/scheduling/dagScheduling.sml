(*
 * A region based scheduler.
 *
 * -- Allen
 *)

functor DAGScheduling
   (structure ListScheduler : LIST_SCHEDULER
    structure DDGBuilder    : SCHEDULER_DDG_BUILDER
    structure Ranks         : SCHEDULING_RANKS
                                where type edge = DDGBuilder.DDG.edge
    structure Viewer        : GLOBAL_SCHEDULER_DDG_VIEWER
       sharing DDGBuilder.DDG = ListScheduler.DDG = Ranks.DDG
       sharing DDGBuilder.CFG = ListScheduler.CFG
       sharing Viewer.IR = ListScheduler.IR
       sharing Viewer.DDG = DDGBuilder.DDG
   ) : GLOBAL_SCHEDULING_ALGORITHM =
struct
   structure IR  = ListScheduler.IR
   structure CFG = ListScheduler.CFG
   structure DDG = ListScheduler.DDG
   structure G   = Graph

   val i2s = Int.toString

   val view_IR = MLRiscControl.getFlag "view-IR"

   fun schedule cpu_info { ir, region, numberOfInstructions, blockIdTbl } = 
   let val DDG as G.GRAPH ddg = 
           DDGBuilder.buildDDG {cpu_info=cpu_info, 
                                cfg=region, blockIdTbl=blockIdTbl,
                                numberOfInstructions=numberOfInstructions}
       val _ = print("V(ddg)="^i2s(#order ddg ())^
                     " E(ddg)="^i2s(#size ddg ())^"\n")
       val _ = if !view_IR then Viewer.view ir DDG else ()
       val ranking = Ranks.rank DDG
   in  ListScheduler.listScheduler
          {cpu_info=cpu_info, 
           blockIdTbl=blockIdTbl,
           cfg=ir, 
           region=region, 
           ddg=DDG, 
           ranking=ranking
          } 
   end

end
