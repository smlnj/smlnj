(* 
 * Machine SSA representation.
 *
 * Some conventions
 * ----------------
 *  1.  Each machine instruction is mapped into an ssa_op.  Some exceptions:
 *      a.  live-out and live-in for each entry and exit are represented as 
 *          SINK and SOURCE nodes.
 *      b.  PHI functions may be inserted
 *      c.  COPYs may be propagated during construction of the ssa graph. 
 *  2.  Each instruction set must provide the pseudo instructions SINK, SOURCE
 *      and PHI.
 *  3.  Each ssa_op is numbered with its own ssa_id, starting from 0.
 *  4.  Each definition is given a unique (non-negative) value id
 *      IMPORTANT: ssa_id <> value.  Each instructions may define multiple
 *      values, even zero.   But each instruction is given its own unique id.
 *  5.  Negative value ids are immediate constants.   These are value numbered
 *      so that the each distinct constant is given unique (negative) id.
 *      These constants are entered into the operandTbl.
 *  6.  In-edges of the graph are use-def chains.  These are computed on the
 *      fly.
 *      Out-edges are def-use chains.  Both type of edges have the following
 *      form:
 *              (source id, dst id, register)
 *      Of course, immedidate operands have no edges associated with them.
 *  7.  The graph interface has high overhead.  So in order to allow faster
 *      implementation, we allow direct access to the internal data structures.
 *      These are:
 *         Name          Mapping             Description
 *         ---------------------------------------------------------------
 *         defSiteTbl   value -> ssa_id      Definition site of a value
 *                                           (i.e. use/def chains)
 *         blockTbl     ssa_id -> block      Block id of an instruction
 *         defsTbl      ssa_id -> value list Definitions of an ssa_op
 *         usesTbl      ssa_id -> value list Uses of an ssa_op
 *         rtlTbl       ssa_id -> rtl        RTL of an ssa_op
 *         succTbl      ssa_id -> outedges   Out edges of an ssa_op
 *                                           (i.e. def/use chains)
 *         ssaOpTbl     ssa_id -> ssa_op     ssa_op table
 *         cellKindTbl  value -> ssa_id      cellkind of a value
 *         operandTbl   value -> operand     operand table
 * 
 *     But in general, you should use the graph interface for traversal
 *     if are not sure how the internal tables work.
 * 
 * -- Allen (leunga@cs.nyu.edu)
 * 
 *)
signature SSA =
sig

   structure I          : INSTRUCTIONS
   structure C          : CELLS
   structure SP         : SSA_PROPERTIES
   structure RTL        : MLTREE_RTL
   structure CFG        : SSA_FLOWGRAPH
   structure Dom        : DOMINATOR_TREE
   structure DJ         : DJ_GRAPH
   structure GCMap      : GC_MAP
   structure MLTreeComp : MLTREECOMP
   structure W          : FREQ
      sharing SP.I = CFG.I = MLTreeComp.I = I
      sharing SP.RTL = RTL
      sharing MLTreeComp.T = RTL.T
      sharing I.C = SP.C = C 
      sharing Dom = DJ.Dom
      sharing CFG.W = W

   (*------------------------------------------------------------------------
    * Basic type definitions used in the SSA form
    *------------------------------------------------------------------------*)
   type value  = int             (* value id *)
   type pos    = int             (* position within a block *)
   type block  = Graph.node_id   (* block id *)
   type ssa_id = Graph.node_id   (* ssa id *)
   type rtl    = RTL.rtl         (* RTL *)
   type const  = SP.OT.const     (* constants *)
   type cfg    = CFG.cfg         (* control flow graph *)
                  (* dominator tree *)
   type dom    = (CFG.block,CFG.edge_info,CFG.info) Dom.dominator_tree 
   type nameTbl = {oldName:C.cell, index:int} IntHashTable.hash_table 

   (*------------------------------------------------------------------------
    * An SSA op is an instruction 
    *------------------------------------------------------------------------*)
   type ssa_op = I.instruction

   (*------------------------------------------------------------------------
    * Information about the SSA graph 
    *------------------------------------------------------------------------*)
   type ssa_info 

   (*------------------------------------------------------------------------
    * The graph structure
    *------------------------------------------------------------------------*)
   type ssa = (ssa_op,value,ssa_info) Graph.graph

   (*------------------------------------------------------------------------
    * How to create a new SSA graph
    *------------------------------------------------------------------------*)
                  (* create an empty SSA graph *)
   val newSSA : {cfg:     cfg, 
                 dom:     cfg -> dom, 
                 gcmap:   GCMap.gcmap option,
                 nameTbl: nameTbl option
                } -> ssa  
   val newRenamedVar : ssa -> C.cell -> value   (* generate renamed variable *)
   val newVar    : ssa -> C.cell -> C.cell      

   (* create a new op; but does not add edges *)
   val newOp     : ssa -> {id   : ssa_id,        
                           instr: I.instruction, 
                           rtl  : rtl, 
                           defs : value list,
                           uses : value list,
                           block: block,
                           pos  : pos
                          } -> unit
   val reserve   : ssa -> int -> unit           (* reserve n nodes *)
   val immed     : ssa -> int -> value          (* lookup immed value *)
   val operand   : ssa -> I.operand -> value    (* lookup perand *)
   val computeDefUseChains : ssa -> unit

   (*------------------------------------------------------------------------
    * Extract info from the SSA graph
    *------------------------------------------------------------------------*)
   val dom       : ssa -> dom                 (* extracts the dominator *)
   val cfg       : ssa -> cfg                 (* extracts the CFG *)
   val maxVar    : ssa -> int                 (* maximum number of ssa names *)
   val numberOfOperands : ssa -> int          (* number of operands *)
   val const     : ssa -> value -> const      (* lookup const values *)

   (*------------------------------------------------------------------------
    * Extract the raw tables.  
    * These should only be used when the optimization guarantees that
    * no new ssa ops are added to the graph, since that may involve resizing
    * these tables, rendering them obsolete.  
    *------------------------------------------------------------------------*)
   val defSiteTbl : ssa -> ssa_id Array.array    
   val blockTbl   : ssa -> block Array.array
   val posTbl     : ssa -> pos Array.array
   val defsTbl    : ssa -> value list Array.array 
   val usesTbl    : ssa -> value list Array.array 
   val rtlTbl     : ssa -> rtl Array.array
   val succTbl    : ssa -> value Graph.edge list Array.array (* out edges *)
   val ssaOpTbl   : ssa -> ssa_op Array.array                (* node table *) 
   val cellKindTbl: ssa -> C.cellkind IntHashTable.hash_table
                              (* cellkind table *)
   val operandTbl : ssa -> SP.OT.operandTable       
   val minPos     : ssa -> int ref
   val maxPos     : ssa -> int ref

   (*------------------------------------------------------------------------
    * Zero registers, pinned registers
    *------------------------------------------------------------------------*)
   val zeroRegs     : Word8Array.array
   val pinnedUseTbl : Word8Array.array
   val pinnedDefTbl : Word8Array.array

   (*------------------------------------------------------------------------
    * Lookup information (the safe way)
    *------------------------------------------------------------------------*)
   val defSite   : ssa -> value -> ssa_id          (* lookup definition site *)
   val block     : ssa -> ssa_id -> block          (* lookup block id *)
   val rtl       : ssa -> ssa_id -> rtl            (* lookup rtl *)
   val uses      : ssa -> ssa_id -> value list
   val defs      : ssa -> ssa_id -> value list

       (* nodes linearized and indexed by block id *)
   val nodes     : ssa -> {sources : ssa_id list Array.array,
                           phis    : ssa_id list Array.array,
                           ops     : ssa_id list Array.array,
                           sinks   : ssa_id list Array.array
                          }
   val freqTbl   : ssa -> W.freq Array.array  (* frequency indexed by block *)
   val noResize  : ssa -> ('a -> 'b) -> 'a -> 'b
   (*------------------------------------------------------------------------
    *  Iterators
    *------------------------------------------------------------------------*)
   val forallNodes : ssa -> (ssa_id -> unit) -> unit
   val foldNodes   : ssa -> (ssa_id * 'a -> 'a) -> 'a -> 'a 

   (*------------------------------------------------------------------------
    * Remove all useless phi-functions from the graph.  
    * Useless phi-functions are self-loops of the form
    *    t <- phi(t, t, ..., t, s, t, ..., t)
    * This transformation removes this phi-function and replace all uses
    * of t by s.  This process is worklist driven; removing a useless 
    * phi-function can introduce other useless phi-functions. 
    *------------------------------------------------------------------------*)
   val removeUselessPhiFunctions : ssa -> unit
 
   (*------------------------------------------------------------------------
    * Remove all nodes from the graph.  Note that no uses should be
    * present after this transformation.
    *------------------------------------------------------------------------*)
   val removeAllNodes : ssa -> ssa_id list -> unit

   (*------------------------------------------------------------------------
    * Replace all use of one value with another.  Return true iff
    * all uses of "from" has been replaced by "to".
    * Note: The definition of "from" must dominate all uses of "to", as
    * required by the SSA form.
    *------------------------------------------------------------------------*)
   val replaceAllUses : ssa -> {from:value, to:value, vn:value} -> bool

   (*------------------------------------------------------------------------
    * Replace the definition of value by const.  Return true iff
    * this operation is successful.
    *------------------------------------------------------------------------*)
   val foldConstant : ssa -> {value:value, const:value} -> bool

   (*------------------------------------------------------------------------
    * Move an instruction from one block to another
    *------------------------------------------------------------------------*)
   val moveOp : ssa -> {id:ssa_id, block:block} -> unit

   (*------------------------------------------------------------------------
    * Set the target of a conditional branch as true or false.
    * This removes the branch and eliminates all unreachable code.
    *------------------------------------------------------------------------*)
   val setBranch : ssa -> {id:ssa_id, cond:bool} -> unit

   (*------------------------------------------------------------------------
    * Signal that an SSA has been changed.   This uncaches all data structures.
    *------------------------------------------------------------------------*)
   val changed : ssa -> unit

   (*------------------------------------------------------------------------
    *  Pretty printing 
    *------------------------------------------------------------------------*)
   val showOp  : ssa -> ssa_id -> string
   val showVal : ssa -> value -> string
   val showRTL : ssa -> rtl -> string

   (*------------------------------------------------------------------------
    *  Graphical viewing 
    *------------------------------------------------------------------------*)
   val viewAsCFG : ssa -> GraphLayout.layout
   val viewAsSSA : ssa -> GraphLayout.layout

   (*------------------------------------------------------------------------
    *  Check whether the ssa graph is consistent
    *------------------------------------------------------------------------*)
   val consistencyCheck : ssa -> unit
end

