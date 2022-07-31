(*
 * Basic Instruction properties that must be supported on all architectures.
 *
 * -- Allen
 *)
signature INSN_PROPERTIES = 
sig
   structure I : INSTRUCTIONS
   structure C : CELLS
     sharing I.C = C

      (* classify instructions *)
   datatype kind = IK_JUMP   (* branches, including returns *)
                 | IK_NOP    (* no ops *)
                 | IK_INSTR  (* normal instructions *)
                 | IK_COPY   (* parallel copy *)
                 | IK_CALL   (* call instructions *)
                 | IK_CALL_WITH_CUTS (* call with cut edges *)
                 | IK_PHI    (* A phi node (SSA) *)
                 | IK_SINK   (* A sink node (SSA) *)
                 | IK_SOURCE (* A source node (SSA) *)

   val instrKind  : I.instruction -> kind

      (* parallel moves *) 
   val moveInstr  : I.instruction -> bool
   val moveTmpR   : I.instruction -> CellsBasis.cell option
   val moveDstSrc : I.instruction -> CellsBasis.cell list * CellsBasis.cell list

      (* no op *)
   val nop 	  : unit -> I.instruction

      (* jump instruction *)
   val jump       : Label.label -> I.instruction

      (* load immediate; must be within immedRange *)
   val immedRange  : {lo:int, hi:int}
   val loadImmed   : {immed:int, t:CellsBasis.cell} -> I.instruction
   val loadOperand : {opn:I.operand, t:CellsBasis.cell} -> I.instruction

     (* 
      * Targets of a branch instruction 
      * precondition: instruction must be of type IK_JUMP.
      *)
   datatype target = LABELLED of Label.label | FALLTHROUGH | ESCAPES
   val branchTargets : I.instruction -> target list

  (* Set the jump target; error if not a jump instruction.  *)
   val setJumpTarget : I.instruction * Label.label -> I.instruction

  (* Set the branch target; error if not a branch instruction, t=true, f=false case *)
   val setBranchTargets : {i:I.instruction, t:Label.label, f:Label.label} -> I.instruction
 
      (* equality and hashing on operands *)
   val eqOpn      : I.operand * I.operand -> bool
   val hashOpn    : I.operand -> word

  (* Given a conditional jump instruction and label, return a conditional
   * jump that has the complimentary condition and that targets the given
   * label.  If the given instruction is not a conditional jump, then
   * the NegateConditional exception is raised.
   *)
    exception NegateConditional
    val negateConditional : (I.instruction * Label.label) -> I.instruction

     (* definition/use for the RA *)
   val defUse     : CellsBasis.cellkind -> 
                      I.instruction -> (CellsBasis.cell list * CellsBasis.cell list)

     (* annotations *)
   val getAnnotations : I.instruction ->
                           I.instruction * Annotations.annotation list
   val annotate       : I.instruction * Annotations.annotation -> I.instruction

   val replicate : I.instruction -> I.instruction
end

