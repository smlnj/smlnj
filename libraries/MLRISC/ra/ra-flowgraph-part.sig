signature RA_FLOWGRAPH_PARTITIONER =
sig

   structure C : CELLS

   type flowgraph

   (* Number of basic blocks in the flowgraph *) 
   val numberOfBlocks : flowgraph -> int

   (* Partition a flowgraph into smaller subgraphs and apply
    * allocation to them individually
    *)
   val partition : flowgraph -> CellsBasis.cellkind -> (flowgraph -> flowgraph) -> unit

end
