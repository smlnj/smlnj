structure AMD64MLTree =
   MLTreeF (structure Constant  = UserConst
    structure Region    = UserRegion
    structure Extension = UserExtension)

structure AMD64MLTreeEval =
   MLTreeEval (structure T = AMD64MLTree
    fun eq _ _ = false
    val eqRext = eq val eqFext = eq
    val eqCCext = eq val eqSext = eq)

structure AMD64PseudoOps  =
  struct
    structure Client =
      struct
        datatype pseudo_op_ext = COMM of (Label.label * int)
        structure AsmPseudoOps = AMD64GasPseudoOps (
			     structure T = AMD64MLTree
			     structure MLTreeEval = AMD64MLTreeEval)
        type pseudo_op = pseudo_op_ext        
        fun toString (COMM(lab, sz)) = concat[         
            "\t.comm\t"(*, P.lexpToString(P.T.LABEL lab)*),
            ",", Int.toString sz]
        fun emitValue {pOp, loc, emit} = raise Fail "emitValue"
        fun sizeOf _ = 0
        fun adjustLabels _ = false
      end (* Client *)

    structure T = AMD64MLTree
    type pseudo_op = (T.labexp, Client.pseudo_op) PseudoOpsBasisTyp.pseudo_op
    fun toString _ = ""
    fun emitValue _ = ()
    fun sizeOf _ = 0
    fun adjustLabels _ = false
end (* AMD64PseudoOps *)

structure PS = PseudoOps (structure Client = AMD64PseudoOps.Client)

structure Stream = InstructionStream(PS)


structure AMD64Instr = AMD64Instr (AMD64MLTree)

structure AMD64Shuffle = AMD64Shuffle(AMD64Instr)


structure AMD64MLTreeHash =
   MLTreeHash (structure T = AMD64MLTree
    fun h _ _ = 0w0
    val hashRext = h val hashFext = h
    val hashCCext = h val hashSext = h)

structure AMD64Asm = AMD64AsmEmitter
   (structure Instr = AMD64Instr
    structure S = Stream
    structure MLTreeEval = AMD64MLTreeEval
    structure Shuffle = AMD64Shuffle
   )

structure AMD64InsnProps = AMD64Props 
			  (structure Instr = AMD64Instr
                           structure MLTreeHash = AMD64MLTreeHash
			   structure MLTreeEval = AMD64MLTreeEval)

structure AMD64CFG = ControlFlowGraph (
            structure I = AMD64Asm.I
	    structure GraphImpl = DirectedGraph
	    structure InsnProps = AMD64InsnProps
	    structure Asm = AMD64Asm)

structure AMD64Stream = InstructionStream(AMD64PseudoOps)
structure AMD64MLTStream = MLTreeStream (
		      structure T = AMD64MLTree
		      structure S = AMD64Stream)

structure AMD64MTC = struct
  structure T = AMD64MLTree
  structure TS = AMD64MLTStream
  structure I = AMD64Instr
  structure CFG = AMD64CFG
  structure C = I.C
   type reducer =
     (I.instruction,C.cellset,I.operand,I.addressing_mode,AMD64CFG.cfg) TS.reducer
   fun unimplemented _ = MLRiscErrorMsg.impossible "UserMLTreeExtComp"
   val compileSext  = unimplemented
   val compileRext  = unimplemented
   val compileFext  = unimplemented
   val compileCCext = unimplemented
		      
   structure AMD64MLTreeUtils : MLTREE_UTILS =
     struct
       structure T = AMD64MLTree
       structure IX = AMD64InstrExt
       structure U = MLTreeUtils (
       structure T = T
       fun hashSext _ _ = 0w0
       fun hashRext _ _ = 0w0
       fun hashFext _ _ = 0w0
       fun hashCCext _ _ = 0w0
       fun eqSext _ _ = raise Fail "eqSext"
       fun eqRext _ _ = raise Fail "eqRext"
       fun eqFext _ _ = raise Fail "eqFext"
       fun eqCCext _ _ = raise Fail "eqCCext"
       fun showSext (prt : T.printer) ext = raise Fail "todo"
       fun showRext _ _ = raise Fail "showRext"
       fun showFext _ _ = raise Fail "showFext"
       fun showCCext _ _ = raise Fail "showCCext")
       open U
     end
end

structure AMD64 = AMD64Gen (
		  structure I = AMD64Instr
		  structure MLTreeUtils = AMD64MTC.AMD64MLTreeUtils
		  structure ExtensionComp = AMD64MTC
		  fun signBit _ = raise Fail "todo"
		  fun negateSignBit _ = raise Fail "todo"
		  val floats16ByteAligned = true
		  )

structure AMD64Emit = CFGEmit (
             structure CFG = AMD64CFG
             structure E = AMD64Asm) 


structure AMD64FlowGraph = BuildFlowgraph 
	    (structure Props = AMD64InsnProps
             structure Stream = AMD64Stream
	     structure CFG = AMD64CFG)

structure AMD64Expand = CFGExpandCopies (structure CFG=AMD64CFG
                                         structure Shuffle = AMD64Shuffle)
structure AMD64BlockPlacement = DefaultBlockPlacement(AMD64CFG)

structure RASpill = RASpillWithRenaming (
    structure Asm = AMD64Asm
    structure InsnProps = AMD64InsnProps
    val max_dist = ref 4
    val keep_multiple_values = ref false)

structure C = AMD64Cells

datatype spill_operand_kind = SPILL_LOC 
                            | CONST_VAL

datatype ra_phase = SPILL_PROPAGATION 
                  | SPILL_COLORING

structure IntRA = 
  struct
    val dedicated = [C.rsp, C.rbp]
    val allRegs = C.Regs CellsBasis.GP {from=0, to=15, step=1}
    val allRegsSet = foldl C.addReg C.empty allRegs
    val avail = let
        val availSet = foldl C.rmvReg allRegsSet dedicated
        in
          C.getReg availSet
        end
    fun spillInit _ = ()
    fun spillLoc {info=frame, an, cell, id=loc} = 
        {opnd = AMD64Instr.Immed 0, kind = SPILL_LOC}
    val phases = [SPILL_PROPAGATION, SPILL_COLORING]
  end (* IntRA *)

structure FloatRA =
  struct
    val avail = C.Regs CellsBasis.FP {from=0, to=15, step=1}
    val dedicated = []
    fun spillInit _ = ()
    fun spillLoc (info, ans, id) = AMD64Instr.Immed 0
    val phases = [SPILL_PROPAGATION, SPILL_COLORING]
  end (* FloatRA *)

(* register allocation *)
structure AMD64RA = AMD64RegAlloc (
         structure I = AMD64Instr
         structure CFG = AMD64CFG
         structure Asm = AMD64Asm
         structure SpillHeur = ChowHennessySpillHeur
         structure Spill = RASpill
         structure Props = AMD64InsnProps
         type spill_info = unit
         fun beforeRA (Graph.GRAPH graph) = ()
         datatype spill_operand_kind = datatype spill_operand_kind
         datatype ra_phase = datatype ra_phase
         structure Int = IntRA
         structure Float = FloatRA
	 val floats16ByteAligned = true)

structure CCalls = X86_64SVIDFn (
        structure T = AMD64MLTree
        val frameAlign = 8)

structure AMD64Expand = CFGExpandCopies (
    structure CFG=AMD64CFG
    structure Shuffle = AMD64Shuffle)
