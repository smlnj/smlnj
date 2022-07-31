(*
 * This module partitions the IR according to some partitioning criteria
 * and frequency.  This is used mainly for global scheduling.
 *
 * -- Allen
 *)

signature REGION_BUILDER =
sig
    structure IR : MLRISC_IR

    val regionBuilder : 
        { maxBlocks         : int,
          maxInstrs         : int,
          minFreqRatio      : real,
          sideEntries       : bool,  (* can the region has side entries *)
          traceOnly         : bool,  (* no splits or merges? *)
          internalBackEdges : bool,  (* can the region has internal back edges*)
          insertDummyBlocks : bool
        } ->
        IR.IR -> 
        ({ir     : IR.IR,   (* The entire program *)
          region : IR.cfg,  (* The subregion in question *)
          numberOfInstructions : int,
          blockIdTbl : int Array.array
         } -> unit
        ) -> unit

end
