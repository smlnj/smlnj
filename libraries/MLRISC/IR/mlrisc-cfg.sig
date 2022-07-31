(*
 * Control flow graph data structure used by the MLRISC IR.
 * All basic optimizations are based on this representation.
 *
 * -- Allen
 *)

signature CONTROL_FLOW_GRAPH =
sig

   structure I : INSTRUCTIONS
   structure P : PSEUDO_OPS
   structure C : CELLS
   structure W : FREQ
      sharing I.C = C
   
   type weight = W.freq

   datatype block_kind = 
       START          (* entry node *)
     | STOP           (* exit node *)
     | NORMAL         (* normal node *)
     | HYPERBLOCK     (* hyperblock *)

   and data = LABEL  of Label.label
            | PSEUDO of P.pseudo_op

   (*
    * NOTE: the instructions are listed in reverse order.
    * This choice is for a few reasons:
    * i)  Clusters represent instructions in reverse order, so keeping this
    *     the same avoid having to do conversions.
    * ii) This makes it easier to add instructions at the end of the block,
    *     which is more common than adding instructions to the front.
    * iii) This also makes it easier to manipulate the branch/jump instruction
    *      at the end of the block.
    *)
   
   and block = 
      BLOCK of
      {  id          : int,                        (* block id *)
         kind        : block_kind,                 (* block kind *)
         freq        : weight ref,                 (* execution frequency *) 
         data        : data list ref,              (* data preceeding block *) 
         labels      : Label.label list ref,       (* labels on blocks *) 
         insns       : I.instruction list ref,     (* in rev order *)
         annotations : Annotations.annotations ref (* annotations *)
      }


   and edge_kind = ENTRY           (* entry edge *) 
                 | EXIT            (* exit edge *)
                 | JUMP            (* unconditional jump *)
                 | FALLSTHRU       (* falls through to next block *)  
                 | BRANCH of bool  (* branch *) 
                 | SWITCH of int   (* computed goto *)   
                 | SIDEEXIT of int (* the ith side exit in a hyperblock *)

   and edge_info = EDGE of { k : edge_kind,                  (* edge kind *)
                             w : weight ref,                 (* edge freq *)
                             a : Annotations.annotations ref (* annotations *)
                           }

   type edge = edge_info Graph.edge
   type node = block Graph.node

   datatype info = 
       INFO of { annotations : Annotations.annotations ref,
                 firstBlock  : int ref, (* id of first block *)
                 reorder     : bool ref (* has the CFG been reordered? *)
               }

   type cfg = (block,edge_info,info) Graph.graph

  (*========================================================================
   *
   *  Various kinds of annotations on basic blocks
   *
   *========================================================================*)
   val LIVEOUT : C.cellset Annotations.property
                  (* escaping live out information *)
   val CHANGED : (string * (unit -> unit)) Annotations.property

  (*========================================================================
   *
   *  Methods for manipulating basic blocks
   *
   *========================================================================*)
   val newBlock          : int * W.freq ref -> block (* empty *)
   val newStart          : int * W.freq ref -> block          (* start node *)
   val newStop           : int * W.freq ref -> block          (* stop node *)
   val copyBlock         : int * block -> block       (* copy a block *)
   val defineLabel       : block -> Label.label       (* define a label *)
   val insns             : block -> I.instruction list ref
   val freq              : block -> W.freq ref
   val branchOf          : edge_info -> bool option

               (* emit assembly *)
   val emit       : Annotations.annotations -> block -> unit  
   val show_block : Annotations.annotations -> block -> string 

  (*========================================================================
   *
   *  Methods for manipulating CFG
   *
   *========================================================================*)
   val cfg      : info -> cfg      (* create a new cfg *) 
   val new      : unit -> cfg      (* create a new cfg *)
   val subgraph : cfg -> cfg       (* mark as subgraph *)
   val init     : cfg -> unit      (* add start/stop nodes *)
   val changed  : cfg -> unit      (* mark cfg as changed *)  

   val annotations    : cfg -> Annotations.annotations ref
   val liveOut        : block -> C.cellset
   val fallsThruFrom  : cfg * Graph.node_id -> Graph.node_id option
   val fallsThruTo    : cfg * Graph.node_id -> Graph.node_id option
   val removeEdge     : cfg -> edge -> unit
   val setBranch      : cfg * Graph.node_id * bool -> I.instruction
   val edgeDir        : edge_info Graph.edge -> bool option

  (*========================================================================
   *
   *  For viewing
   *
   *========================================================================*)
   val viewStyle      : cfg -> (block,edge_info,info) GraphLayout.style
   val viewLayout     : cfg -> GraphLayout.layout
   val headerText     : block -> string
   val footerText     : block -> string
   val subgraphLayout : { cfg : cfg, subgraph : cfg } -> GraphLayout.layout

  (*========================================================================
   *
   *  Miscellaneous stuff
   *
   *========================================================================*)
   val cdgEdge : edge_info -> bool (* for building a CDG *)

end

