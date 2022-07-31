local 

val fast_floating_point = ref true

(*
 * User defined constant type.  Dummy for now.
 * In practice, you'll want to use this type to implement constants with
 * values that cannot be determined until final code generation, e.g.
 * stack frame offset.
 *)
structure UserConst =
struct
   type const = unit
   fun toString() = ""  
   fun hash() = 0w0  
   fun valueOf _ = 0
   fun == _ = true  
end

(*
 * User defined datatype for representing aliasing.   Dummy for now.
 * You'll need this to represent aliasing information. 
 *)
structure UserRegion =
struct
   type region = unit
   fun toString () = "" 
   val memory = ()
   val stack = ()
   val readonly = ()
   val spill = ()
end

(*
 * User defined datatype for representing pseudo assembly operators.
 * Dummy for now.
 *
 * You'll need this to represent assembler directives. 
 *)
structure UserPseudoOps =
struct
   type pseudo_op = unit  
   fun toString () = ""
   fun emitValue _ = ()
   fun sizeOf _ = 0
   fun adjustLabels _ = true
end



(*
 * Client defined extensions.  None for now.
 * You'll need this only if you need to extend the set of MLTREE operators
 *)
structure UserExtension =
struct

   type ('s,'r,'f,'c) sx = ('s,'r,'f,'c) X86InstrExt.sext
   type ('s,'r,'f,'c) rx = unit
   type ('s,'r,'f,'c) fx = unit
   type ('s,'r,'f,'c) ccx = unit

end

(*
 * This module controls how we handle user extensions.  Since we don't
 * have any yet.  This is just a bunch of dummy routines.
 *)
functor UserMLTreeExtComp
	    (    structure I : X86INSTR where T.Extension = UserExtension
    structure TS : MLTREE_STREAM where T = I.T
    structure CFG : CONTROL_FLOW_GRAPH where I = I and P = TS.S.P
   ) : MLTREE_EXTENSION_COMP =
struct
    structure T = TS.T
    structure TS = TS
    structure I = I
    structure CFG = CFG
    structure C = I.C

    structure CompInstrExt = X86CompInstrExt (
      structure I = I
      structure TS = TS
      structure CFG = CFG)

    type reducer =
	  (I.instruction,C.cellset,I.operand,I.addressing_mode,CFG.cfg) TS.reducer

    val compileSext = CompInstrExt.compileSext

    fun compileRext _ = raise Fail "CompExtFn.compileRext"
    fun compileFext _ = raise Fail "CompExtFn.compileFext"
    fun compileCCext _ = raise Fail "CompExtFn.compileCCext"

end

structure X86MLTree =
   MLTreeF (structure Constant  = UserConst
    structure Region    = UserRegion
    structure Extension = UserExtension)

structure X86MLTreeEval =
   MLTreeEval (structure T = X86MLTree
    fun eq _ _ = false
    val eqRext = eq val eqFext = eq
    val eqCCext = eq val eqSext = eq)

structure X86GasPseudoOps = 
   X86GasPseudoOps(structure T=X86MLTree
		   structure MLTreeEval=X86MLTreeEval)
functor X86PseudoOpsFn (
    structure T : MLTREE
    structure MLTreeEval : MLTREE_EVAL where T = T
  ) : PSEUDO_OPS_BASIS = X86GasPseudoOps (
    structure T = T
    structure MLTreeEval = MLTreeEval)
(*
functor X86PseudoOpsFn (
    structure T : MLTREE
    structure MLTreeEval : MLTREE_EVAL where T = T
  ) : PSEUDO_OPS_BASIS = X86DarwinPseudoOps (
    structure T = T
    structure MLTreeEval = MLTreeEval)
*)
structure X86PseudoOps = X86PseudoOpsFn(
            structure T = X86MLTree
            structure MLTreeEval = X86MLTreeEval)

structure PseudoOps =
  struct

    structure Client =
      struct
	structure AsmPseudoOps = X86PseudoOps
	type pseudo_op = unit
			 
	fun toString () = ""
  
	fun emitValue _ = raise Fail "todo"
	fun sizeOf _ = raise Fail "todo"
	fun adjustLabels _ = raise Fail "todo"
      end (* Client *)
  
    structure PseudoOps = PseudoOps (structure Client = Client)
  end

structure X86Stream = InstructionStream(PseudoOps.PseudoOps)
structure X86Instr = X86Instr (X86MLTree)
structure X86Shuffle = X86Shuffle(X86Instr)

structure X86MLTreeHash =
   MLTreeHash (structure T = X86MLTree
    fun h _ _ = 0w0
    val hashRext = h val hashFext = h
    val hashCCext = h val hashSext = h)


functor X86MemRegs(X86Instr:X86INSTR) = struct
  structure I = X86Instr
  structure CB = CellsBasis

  fun memReg{reg, base} = raise Fail ""
end

structure X86MemRegs = X86MemRegs(X86Instr)

structure X86Asm = X86AsmEmitter
   (structure Instr = X86Instr
    structure S = X86Stream
    val memRegBase = NONE
    structure MemRegs = X86MemRegs
    structure MLTreeEval = X86MLTreeEval
    structure Shuffle = X86Shuffle
   )

structure X86InsnProps = X86Props 
			  (structure Instr = X86Instr
                           structure MLTreeHash = X86MLTreeHash
			   structure MLTreeEval = X86MLTreeEval)

structure X86CFG = ControlFlowGraph (
            structure I = X86Asm.I
	    structure GraphImpl = DirectedGraph
	    structure InsnProps = X86InsnProps
	    structure Asm = X86Asm)

structure X86MLTStream = MLTreeStream (
		      structure T = X86MLTree
		      structure S = X86Stream)

structure CompInstrExt = X86CompInstrExt (
      structure I = X86Instr
      structure TS = X86MLTStream
      structure CFG = X86CFG)

structure X86MTC = struct
  structure T = X86MLTree
  structure TS = X86MLTStream
  structure I = X86Instr
  structure CFG = X86CFG
  structure C = I.C
   type reducer =
     (I.instruction,C.cellset,I.operand,I.addressing_mode,X86CFG.cfg) TS.reducer
   fun unimplemented _ = MLRiscErrorMsg.impossible "UserMLTreeExtComp"
   val compileSext  = CompInstrExt.compileSext
   val compileRext  = unimplemented
   val compileFext  = unimplemented
   val compileCCext = unimplemented
		      
   structure X86MLTreeUtils : MLTREE_UTILS =
     struct
       structure T = X86MLTree
       structure IX = X86InstrExt
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

structure X86 = X86 (
		  structure X86Instr = X86Instr
		  structure MLTreeUtils = X86MTC.X86MLTreeUtils
		  structure ExtensionComp = X86MTC
		  structure MLTreeStream = X86MLTStream
           datatype arch = Pentium | PentiumPro | PentiumII | PentiumIII
           val arch = ref Pentium (* Lowest common denominator *)

		  fun cvti2f _ = raise Fail ""
		  val fast_floating_point = fast_floating_point
		  )

structure X86Emit = CFGEmit (
             structure CFG = X86CFG
             structure E = X86Asm) 


structure X86FlowGraph = BuildFlowgraph 
	    (structure Props = X86InsnProps
             structure Stream = X86Stream
	     structure CFG = X86CFG)

structure X86Expand = CFGExpandCopies (structure CFG=X86CFG
                                         structure Shuffle = X86Shuffle)
structure X86BlockPlacement = DefaultBlockPlacement(X86CFG)

structure RASpill = RASpillWithRenaming (
    structure Asm = X86Asm
    structure InsnProps = X86InsnProps
    val max_dist = ref 4
    val keep_multiple_values = ref false)

datatype spill_operand_kind = SPILL_LOC 
                            | CONST_VAL

datatype ra_phase = SPILL_PROPAGATION 
                  | SPILL_COLORING

  fun upto(from, to) = if from>to then [] else from::(upto (from+1,to))
  infix upto 

structure CB = CellsBasis
structure I = X86Instr
structure C = X86Cells

structure IntRA = 
  struct
    val dedicated = [C.esp, C.ebp]
    val allRegs = C.Regs CellsBasis.GP {from=0, to=7, step=1}
    val allRegsSet = foldl C.addReg C.empty allRegs
    val avail = let
        val availSet = List.foldl C.rmvReg allRegsSet dedicated
        in
          C.getReg availSet
        end
    fun spillInit _ = ()
    val memRegs = C.Regs CB.GP {from=8,to=31,step=1}
    fun spillLoc {info=frame, an, cell, id=loc} = let
	    val spillLoc = ~(loc*4)
	    val opnd = I.Displace {
		  base = C.ebp,
		  disp = I.Immed (Int32.fromInt spillLoc),
		  mem = ()
		}
            in
              {opnd = opnd, kind = SPILL_LOC}
            end
    val phases = [SPILL_PROPAGATION, SPILL_COLORING]
  end (* IntRA *)

structure FloatRA =
  struct
    val avail = List.map C.FPReg (0 upto 31)
    val dedicated = []
    fun spillInit _ = ()
    val fastMemRegs = C.Regs CB.FP {from=8, to=31, step=1}
    val fastPhases = [SPILL_PROPAGATION,SPILL_COLORING]
    val memRegs = []
    fun spillLoc (info, ans, id) = raise Fail ""
    val phases = [SPILL_PROPAGATION]
  end (* FloatRA *)

(* register allocation *)
structure X86RA = X86RA (
         structure I = X86Instr
	 structure InsnProps = X86InsnProps
         structure CFG = X86CFG
         structure Asm = X86Asm
         structure SpillHeur = ChowHennessySpillHeur
         structure Spill = RASpill
         structure Props = X86InsnProps
         type spill_info = unit
         fun beforeRA (Graph.GRAPH graph) = ()
         datatype spillOperandKind = datatype spill_operand_kind
	 datatype raPhase = datatype ra_phase
	 val fast_floating_point = fast_floating_point
         structure Int = IntRA
         structure Float = FloatRA)

structure X86Expand = CFGExpandCopies (
    structure CFG=X86CFG
    structure Shuffle = X86Shuffle)


structure CCall = X86SVIDFn (
           structure T = X86MLTree
           fun ix x = x
	   val fast_floating_point = fast_floating_point
           val abi = "Darwin")


    structure C = X86Instr.C
    structure T = X86MLTree
    structure CFG = X86CFG
    structure FlowGraph = X86FlowGraph
    val wordTy = 32

    fun lit i = T.LI (T.I.fromInt (32, i))
    val param0 = T.LOAD(wordTy, T.ADD(32, lit 8, T.REG(32,C.ebp)), ())

in

structure X86MLTree = X86MLTree

structure X86MLRISCGen =
  struct

    fun codegen' (functionName, stms, result) = let
        val insnStrm = FlowGraph.build()
        val stream as X86Stream.STREAM
           { beginCluster,  (* start a cluster *)
             endCluster,    (* end a cluster *)
             emit,          (* emit MLTREE stm *)
             defineLabel,   (* define a local label *)
             entryLabel,    (* define an external entry *)
             exitBlock,     (* mark the end of a procedure *)
             pseudoOp,      (* emit a pseudo op *)
             annotation,    (* add an annotation *)
             ... } =
             X86.selectInstructions insnStrm
   	fun doit () = (
	    beginCluster 0;      (* start a new cluster *)
            pseudoOp PseudoOpsBasisTyp.TEXT;
	    pseudoOp (PseudoOpsBasisTyp.EXPORT [functionName]);    
            entryLabel functionName; (* define the entry label *)
            List.app emit stms; (* emit all the statements *)
            exitBlock result;
            endCluster [])
	val cfg = doit ()
	val cfg = X86RA.run cfg
	val cfg = X86Expand.run cfg
       in
	(cfg, stream)
       end (* codegen' *)

    fun dumpOutput (cfg, stream) = let
	val (cfg as Graph.GRAPH graph, blocks) = 
		X86BlockPlacement.blockPlacement cfg
	val CFG.INFO{annotations=an, data, decls, ...} = #graph_info graph
	in
	  X86Emit.asmEmit (cfg, blocks)
	end (* dumpOutput *)

    fun codegen (functionName, target, proto, initStms, args) = let 
	val [functionName, target] = List.map Label.global [functionName, target]
	(* construct the C call *)
	val {result, callseq} = CCall.genCall {
	           name=T.LABEL target,
	           paramAlloc=fn _ => false,
	           structRet=fn _ => T.REG (32, C.eax),
	           saveRestoreDedicated=fn _ => {save=[], restore=[]},
	           callComment=NONE,
	           proto=proto,
	           args=args}
	fun wordLit i = T.LI (T.I.fromInt (wordTy, i))
	val stms = List.concat [
		   [T.EXT(X86InstrExt.PUSHL(T.REG(32, C.ebp))),
		    T.COPY (wordTy, [C.ebp], [C.esp])],		   
		   initStms,
		   callseq, 
		   [T.EXT(X86InstrExt.LEAVE)],
		   [T.RET []]]
        in  
           dumpOutput(codegen'(functionName, stms, result))
        end (* codegen *)

  end

structure X86CCall = CCall
structure X86Test = GenTestFn (
		 structure T = X86MLTree
		 structure CCall = CCall
		 structure Cells = X86Cells
		 val codegen = X86MLRISCGen.codegen
		 val param0 = param0
		 val wordTy = 32)

end (* local *)
