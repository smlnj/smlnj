(* mlriscAnnotations.sig
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies
 *
 * These are some basic annotations understood by the MLRISC system.
 * The MLRISC client can create its own annotations and propagate them
 * to MLRISC.  Client-defined annotations are ignored by MLRISC. 
 *
 * -- Allen
 * 
 * TODO: There should be comments to say that the annotations are block
 * or instruction annotations. -- Lal.
 *)

signature MLRISC_ANNOTATIONS =
sig

    structure C : CELLS_BASIS = CellsBasis

    (* 
     * The branch probability of conditional branches. 
     * The client can attach this with conditional branches.
     * nnThis has no effect otherwise. 
     *
     * Currently, the annotation is recognized by the static branch prediction
     * mondule. 
     *)
   exception BRANCHPROB of Probability.prob
   val BRANCH_PROB : Probability.prob Annotations.property

    (* The execution frequency of a basic block 
     * You can attach this at a basic block.
     *)
   exception EXECUTIONFREQ of int
   val EXECUTION_FREQ : int Annotations.property

    (* No effect at all; this just allows you to insert comments *)
   val COMMENT : string Annotations.property

    (* Instructions in the block should not be reordered *)
   val NOREORDER : unit Annotations.property

    (* 
     * Control dependence definition and use.
     *
     * To use these, the client should generate
     * control dependence virtual registers via Cells.newCell Cells.CTRL
     * and attach these annotations to instructions and basic blocks.
     *
     * These annotations are currently recognized by the SSA optimization
     * modules.
     *)
   exception CTRLDEF of C.cell
   exception CTRLUSE of C.cell
   val CTRL_DEF : C.cell Annotations.property
   val CTRL_USE : C.cell Annotations.property

    (* 
     * Attach this annotation to assemblers for pretty printing
     * client defined cell informations.
     *)
   val PRINT_CELLINFO : (C.cell -> string) Annotations.property

    (*
     * Does a compilation unit has GC information? 
     *)
   val GC_INFO : unit Annotations.property

    (*
     * Disable all optimizations in the cluster
     *)
   val NO_OPTIMIZATION : unit Annotations.property

    (*
     * Mark basic block that is used for calling the GC
     *)
   val CALLGC : unit Annotations.property
   val GCSAFEPOINT : string Annotations.property

    (*
     * Insert block names
     *)
   exception BLOCKNAMES of Annotations.annotations
   val BLOCK_NAMES : Annotations.annotations Annotations.property

    (*
     * This annotation inserts an empty basic block
     *)
   exception EMPTYBLOCK 
   val EMPTY_BLOCK : unit Annotations.property

    (* 
     * Enter information for a register.
     *)
   exception MARKREG of C.cell -> unit
   val MARK_REG : (C.cell -> unit) Annotations.property

    (*
     * Disable branch chaining optimization on a jump
     *)
   val NO_BRANCH_CHAINING : unit Annotations.property

    (*
     * Code has reference to a virtual (dedicated) frame pointer.
     *)
   val USES_VIRTUAL_FRAME_POINTER : unit Annotations.property

    (* 
     * Define return arguments of a call (hack for x86)
     *)
   val RETURN_ARG : C.cell Annotations.property
end
