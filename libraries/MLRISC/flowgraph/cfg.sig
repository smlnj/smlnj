(* cfg.sig
 *
 * COPYRIGHT (c) 2001 Bell Labs, Lucent Technologies
 *
 * Control flow graph data structure used by the MLRISC IR.
 * All basic optimizations are based on this representation.
 *
 * -- Allen
 *)

signature CONTROL_FLOW_GRAPH =
sig

   structure P : PSEUDO_OPS 
   structure I : INSTRUCTIONS 

  (* used to represent frequency of execution; we use reals because some
   * static prediction methods produce such.
   *)
   type weight = real

   datatype block_kind = 
       START          (* entry node *)
     | STOP           (* exit node *)
     | NORMAL         (* normal node *)

   (*
    * NOTE 1: the instructions are listed in reverse order.
    * This choice is for a few reasons:
    *
    * i)  Clusters represent instructions in reverse order, so keeping this
    *     the same avoid having to do conversions.
    *
    * ii) This makes it easier to add instructions at the end of the block,
    *     which is more common than adding instructions to the front.
    *
    * iii) This also makes it easier to manipulate the branch/jump instruction
    *      at the end of the block.
    *
    * NOTE 2: 
    *  Alignment always appear before labels in a block.
    *)
   
   and block = 
      BLOCK of
      {  id          : int,                        (* block id *)
         kind        : block_kind,                 (* block kind *)
         freq        : weight ref,                 (* execution frequency *) 
         labels      : Label.label list ref,       (* labels on blocks *) 
         insns       : I.instruction list ref,     (* in rev order *)
	 align	     : P.pseudo_op option ref,	   (* alignment only *)
         annotations : Annotations.annotations ref (* annotations *)
      }


  (* We have the following invariant on blocks and out-edge kinds:
   *
   *	If the last instruction of the block is an unconditional jump, then
   *	there is one out edge labeled with JUMP.
   *
   *	If the last instruction of the block is a conditional jump, then
   *	there are two out edges.  The one corresponding to the jump is
   *	labeled BRANCH(true) and the other is labeled BRANCH(false).
   *
   *    If the last instruction of the block is not a jump, then there is
   *	one out edge labeled with FALLSTHRU.
   *
   *	If the block ends with a switch, then the out edges are labeled with
   *	SWITCH.
   *
   *	If the block ends with a call that has been wrapped with a FLOW_TO,
   *	then there will be one FALLSTHRU out edges and one or more FLOWSTO
   *	out edges.
   *
   *	Control-flow to outside the CFG is represented by edges to the unique
   *	STOP node.  When such edges are to labels that are defined outside
   *	the CFG, then JUMP, BRANCH, or SWITCH edges are used (as appropriate).
   *	When such edges are to unkonwn places (e.g., traps, returns, and
   *	indirect jumps), then an EXIT edge is used.  There should never be
   *	a FALLSTHRU or ENTRY edge to the STOP node.
   *)
    and edge_kind	    (* edge kinds *)
      = ENTRY			(* edge from START node *) 
      | EXIT            	(* unlabeled edge to STOP node *)
      | JUMP			(* unconditional jump *)
      | FALLSTHRU		(* falls through to next block *)  
      | BRANCH of bool		(* branch *) 
      | SWITCH of int		(* computed goto *)
      | FLOWSTO			(* FLOW_TO edge *)
   
    and edge_info = EDGE of {
	k : edge_kind,                  (* edge kind *)
	w : weight ref,                 (* edge freq *)
	a : Annotations.annotations ref (* annotations *)
      }

   type edge = edge_info Graph.edge
   type node = block Graph.node

   datatype info = 
       INFO of 
       { annotations : Annotations.annotations ref,
         firstBlock  : int ref, (* id of first block (UNUSED?) *)
         reorder     : bool ref, (* has the CFG been reordered? *)
         data        : P.pseudo_op list ref, (* reverse order of generation *)
	 decls       : P.pseudo_op list ref (* pseudo-ops before first section *)
       }

   type cfg = (block,edge_info,info) Graph.graph

  (*========================================================================
   *
   *  Various kinds of annotations on basic blocks
   *
   *========================================================================*)
   val LIVEOUT : I.C.cellset Annotations.property
                  (* escaping live out information *)
   val CHANGED : (string * (unit -> unit)) Annotations.property

  (*========================================================================
   *
   *  Methods for manipulating basic blocks
   *
   *========================================================================*)
   val newBlock          : int * weight ref -> block	(* new empty block *)
   val newNode		 : cfg -> weight -> node	(* new empty block hooked *)
							(* into the cfg *)
   val newStart          : int * weight ref -> block	(* start node *)
   val newStop           : int * weight ref -> block	(* stop node *)
   val copyBlock         : int * block -> block		(* copy a block *)
   val defineLabel       : block -> Label.label		(* define a label *)
   val insns             : block -> I.instruction list ref
   val freq              : block -> weight ref
   val edgeFreq          : edge -> weight ref
   val sumEdgeFreqs      : edge list -> weight
   val branchOf          : edge_info -> bool option

               (* emit assembly *)
   val emit       : Annotations.annotations -> block -> unit

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
       (* IMPORTANT note: you MUST call this function after
        * changing the topology of the CFG.  
        *)

   val annotations    : cfg -> Annotations.annotations ref
   val liveOut        : block -> I.C.cellset
   val fallsThruFrom  : cfg * Graph.node_id -> Graph.node_id option
   val fallsThruTo    : cfg * Graph.node_id -> Graph.node_id option
   val removeEdge     : cfg -> edge -> unit
   val setBranch      : cfg * Graph.node_id * bool -> I.instruction
   val edgeDir        : edge_info Graph.edge -> bool option

   val entryId : cfg -> Graph.node_id	(* the unique entry node ID *)
   val exitId : cfg -> Graph.node_id	(* the unique exit node ID *)
   val entry : cfg -> node		(* the unique entry node *)
   val exit : cfg -> node		(* the unique exit node *)

   (*=======================================================================
    *
    *  More complex methods for manipulating CFG.
    *  These methods will guarantee all CFG invariants, like frequencies,
    *  are preserved.
    * 
    *=======================================================================*)

       (* get a label from block; generate one if necessary *)
   val labelOf : cfg -> Graph.node_id -> Label.label

      (*
       *  Update the label of the branch instruction in a block
       *  to be consistent with the control flow edges.  
       *  This is an NOP if the CFG is already consistent.
       *  This is used internally after changing CFG edges, 
       *  but it could also be useful for others.
       *)
   val updateJumpLabel : cfg -> Graph.node_id -> unit

      (*
       *  Deep copy an edge info
       *)
   val copyEdge : edge_info -> edge_info

      (*
       *  Merge a control flow edge.  
       *  [See also the mustPreceed test below]
       *  Return false if merging is unsuccessful.
       *)
   val mergeEdge : cfg -> edge -> bool

      (*
       *  Eliminate the jump/insert a jump
       *     at the end of the current block if it is feasible.
       *  Return true iff it is successful.
       *)
   val eliminateJump : cfg -> Graph.node_id -> bool
   val insertJump    : cfg -> Graph.node_id -> bool

      (*
       *  Edge splitting:
       *
       *  Split n groups of control flow edges, all initially entering block j,
       *
       *     i_11 -> j,  i_12 -> j, ...         group 1
       *     i_21 -> j,  i_22 -> j, ...         group 2
       *             ....
       *     i_n1 -> j,  i_n2 -> j, ...         group n
       *  
       *  into 
       *
       *     i_11 -> k_1 
       *     i_12 -> k_1
       *        ...
       *     i_21 -> k_2
       *     i_22 -> k_2
       *        ...
       *     i_n1 -> k_n
       *     i_n2 -> k_n
       *        ...
       * 
       *  and the chain
       *      k_1 -> k_2
       *      k_2 -> k_3
       *        ...
       *      k_n -> j
       *
       *  where k_1, ..., k_n are new basic blocks.
       * 
       *  Return the new edges 
       *       k_1-> k_2, ..., k_n -> j 
       *
       *  and the new blocks 
       *       k_1, ..., k_n.
       *
       *  Each block k_1, ..., k_n can have instructions placed in them.
       *
       *  If the jump flag is true, then a jump is always placed in the 
       *  new block k_n; otherwise, we try to eliminate the jump when feasible.
       *)
   val splitEdges : cfg ->
                    { groups : (edge list * 
                                I.instruction list (* reverse order *) 
                               ) list,  
                      jump : bool
                    } -> (node * edge) list  (* k_i and k_i -> k_{i+1} *)

       (*
        *  Test if an edge is critical.  An edge i->j is critical iff 
        *  there are multiple entries into j and multiple exits out of i,
        *  i.e. it is both a merge and a split node.
        *
        *  Test if a node is a merge or split node.
        *)
    val isCriticalEdge : cfg -> edge -> bool
    val isMerge        : cfg -> Graph.node_id -> bool
    val isSplit        : cfg -> Graph.node_id -> bool


       (*
        *  Split all critical edges in the CFG.
        *  This may introduce extra jumps into the program.
        *)
    val splitAllCriticalEdges : cfg -> unit

       (*
        *  Check whether two blocks are necessary connected.
        *  Blocks i and j are connected iff i must be placed before j
        *  in all feasible layouts..
        *)
    val mustPreceed : cfg -> Graph.node_id * Graph.node_id -> bool

       (*
        *  Try to merge all edges
        *)
    val mergeAllEdges : cfg -> unit

   (*========================================================================
   *
   *  For viewing
   *
   *========================================================================*)
(*****
   val viewStyle      : cfg -> (block,edge_info,info) GraphLayout.style
   val viewLayout     : cfg -> GraphLayout.layout
   val headerText     : block -> string
   val footerText     : block -> string
   val subgraphLayout : { cfg : cfg, subgraph : cfg } -> GraphLayout.layout
*****)

  (*========================================================================
   *
   *  Miscellaneous stuff
   *
   *========================================================================*)
   val cdgEdge : edge_info -> bool (* for building a CDG *)

  (*========================================================================
   *
   *  Methods for printing CFGs
   *
   *========================================================================*)
   val kindName : block_kind -> string
   val show_block : Annotations.annotations -> block -> string 
   val show_edge  : edge_info -> string 
   val dumpBlock : (TextIO.outstream * cfg) -> node -> unit
   val dump : (TextIO.outstream * string * cfg) -> unit

end

