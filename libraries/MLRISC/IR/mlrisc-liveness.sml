(*
 * This module performs liveness analysis.
 * It is implemented by instantiating the data flow analyzer module.
 *
 * -- Allen
 *)

signature LIVENESS_ANALYSIS =
sig
  
   structure CFG : CONTROL_FLOW_GRAPH
   structure I   : INSTRUCTIONS
       sharing CFG.I = I

   val liveness : 
       { cfg      : CFG.cfg,
         liveOut  : CFG.block Graph.node -> I.C.cell list,
         defUse   : CFG.block Graph.node -> I.C.cell list * I.C.cell list,
         result   : {block: CFG.block Graph.node, 
                     liveIn: I.C.cell list, liveOut: I.C.cell list} -> unit
       } -> unit

end

functor LivenessAnalysis(CFG : CONTROL_FLOW_GRAPH) : LIVENESS_ANALYSIS =
struct

   structure CFG = CFG
   structure I   = CFG.I
   structure C   = I.C
   structure A   = Annotations
   structure SL  = C.SortedCells
   structure G   = Graph

   structure Liveness =
      Dataflow
         (struct
              structure CFG   = CFG
              type domain     = SL.sorted_cells
              val  forward    = false
              val  bot        = SL.empty
              val  ==         = SL.eq
              val  join       = List.foldr SL.union SL.empty 
              val  op +       = SL.union
              val  op -       = SL.difference
              type dataflow_info = 
                  { liveOut : CFG.block Graph.node -> C.cell list,
                    defUse  : CFG.block Graph.node -> 
                                C.cell list * C.cell list,
                    result  : {block: CFG.block Graph.node, 
                               liveIn: I.C.cell list, 
                               liveOut: I.C.cell list} -> unit
                  }

              fun prologue(cfg,{defUse,liveOut,...}:dataflow_info) (b,b') =
                  let val (def,use) = defUse(b,b')
                      val def       = SL.uniq def
                      val use       = SL.uniq use
                      val live_out  = SL.uniq(liveOut(b,b'))
                  in  { input    = live_out,
	                output   = (live_out - def) + use,
	                transfer = fn live_out => (live_out - def) + use
                      }
                  end

              fun epilogue(cfg,{result, ...}:dataflow_info)
                          { node, input=liveOut, output=liveIn } = 
                  result{block=node, liveIn=SL.return liveIn, 
                                     liveOut=SL.return liveOut}
         end
        )

   fun liveness {cfg,liveOut,defUse,result} = 
       (Liveness.analyze(cfg, {liveOut=liveOut,defUse=defUse,result=result});
        ()
       )

end
