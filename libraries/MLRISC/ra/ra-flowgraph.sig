(*
 * Abstract view a flowgraph required by the new register allocator.
 * In order to allow different representation to share the same 
 * register allocator core, each representation should implement the
 * following interface to talk to the new RA.
 *
 * -- Allen
 *)

signature RA_FLOWGRAPH =
sig

   structure I     : INSTRUCTIONS
   structure C     : CELLS  
   structure G     : RA_GRAPH = RAGraph
   structure Spill : RA_SPILL
     sharing Spill.I = I
     sharing I.C = C 

   type flowgraph

   val mode : G.mode

    (* Dump the flograph to a stream *)
   val dumpFlowgraph : string * flowgraph * TextIO.outstream -> unit

    (* Dump the flograph to a stream *)
   val annotations : flowgraph -> Annotations.annotations ref

    (*
     * Interface for communicating with the new register allocator.
     * It is expected that the services will cache enough information
     * during build so that the rebuild and spill phases can be execute
     * quickly.
     *)
   val services : flowgraph ->
       { build   : G.interferenceGraph * CellsBasis.cellkind-> 
                      G.move list, (* build the graph *)
         spill   : {copyInstr    : Spill.copyInstr,
                    spill        : Spill.spill,
                    spillSrc     : Spill.spillSrc,
                    spillCopyTmp : Spill.spillCopyTmp,
                    reload       : Spill.reload,
                    reloadDst    : Spill.reloadDst,
                    renameSrc    : Spill.renameSrc,
                    graph        : G.interferenceGraph,
                    nodes        : G.node list,
                    cellkind     : CellsBasis.cellkind
                   } -> G.move list,
                     (* spill/rebuild the graph *)
         programPoint : {block:int, instr:int} -> G.programPoint,
         blockNum     : G.programPoint -> int,
         instrNum     : G.programPoint -> int
       }

end
