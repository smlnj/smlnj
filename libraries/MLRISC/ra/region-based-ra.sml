(*
 * Region-based register allocator.  
 * This register allocation takes a cluster of large size and partitions
 * it into manageable pieces to be allocated.
 *)
functor RegionBasedRA
   (RA : RA)
   (FlowgraphPartitioner : RA_FLOWGRAPH_PARTITIONER 
       where type flowgraph = RA.F.flowgraph
       where C = RA.C
   ) : RA =
struct

   structure F      = RA.F
   structure FP     = FlowgraphPartitioner
   structure I      = F.I
   structure C      = I.C
   structure Core   = RACore
   structure G      = Core.G

   open RA

   val maxBlocks = MLRiscControl.mkInt ("ra-max-blocks", "max block count for region-based RA")

   (* Main entry point.  
    * All the magic is actually done in the FlowgraphPartitioner module
    *)
   fun ra (params:raClient list) flowgraph =
       if FP.numberOfBlocks flowgraph > !maxBlocks then
          let fun ra (param as {cellkind, ...}) = 
                  FP.partition flowgraph cellkind (RA.ra [param])
          in  app ra params;
              flowgraph
          end
       else RA.ra params flowgraph

end
