local 

(*---------------------------------------------------------------------------
 * First, some front-end dependent stuff.  Typically, you only need
 * one instance of these things for each source language.
 *---------------------------------------------------------------------------*)

(*
 * User defined constant type.  Dummy for now.
 * In practice, you'll want to use this type to implement constants with
 * values that cannot be determined until final code generation, e.g.
 * stack frame offset.
 *)
structure UserConst =
struct
   type const = int
   val toString = Int.toString
   fun hash _ = 0w0  
   fun valueOf _ = 0
   fun == _ = true  
end

(*
 * Instantiate label expressions with respect to user defined constants.
 * This type is somewhat misnamed; it is used to represent constant 
 * expressions.
 *)
(* structure LabelExp = LabelExp(UserConst) *)

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
 * Instruction stream datatype.
 * This is just a simple record type used by MLRISC to represent 
 * instruction streams.
 *)
(*structure Stream = InstructionStream(UserPseudoOps)*)

(*
 * Client defined extensions.  None for now.
 * You'll need this only if you need to extend the set of MLTREE operators
 *)
structure UserExtension =
struct

   type ('s,'r,'f,'c) sx = ('s,'r,'f,'c) SparcInstrExt.sext
   type ('s,'r,'f,'c) rx = unit
   type ('s,'r,'f,'c) fx = unit
   type ('s,'r,'f,'c) ccx = unit

end

structure SparcMLTree =
   MLTreeF (structure Constant  = UserConst
    structure Region    = UserRegion
    structure Extension = UserExtension)


(*---------------------------------------------------------------------------
 * Backend specific stuff.  You'll need one instance of these things 
 * for each architecture.  
 *---------------------------------------------------------------------------*)

(*
 * The Sparc instruction set, specialized with respect to the
 * user constant and region types.  
 *)
structure SparcInstr = SparcInstr
   (SparcMLTree
   )

(*
 * How to serialize parallel copies
 *)
structure SparcShuffle = SparcShuffle(SparcInstr)

structure SparcMLTreeEval =
   MLTreeEval (structure T = SparcMLTree
    fun eq _ _ = false
    val eqRext = eq val eqFext = eq
    val eqCCext = eq val eqSext = eq)

functor SparcPseudoOpsFn (
    structure T : MLTREE
    structure MLTreeEval : MLTREE_EVAL where T = T
  ) : PSEUDO_OPS_BASIS = SparcGasPseudoOps (
    structure T = SparcMLTree
    structure MLTreeEval = SparcMLTreeEval)

structure SparcPseudoOps = SparcPseudoOpsFn(
            structure T = SparcMLTree
            structure MLTreeEval = SparcMLTreeEval)

structure PseudoOps =
  struct

    structure Client =
      struct
	structure AsmPseudoOps = SparcPseudoOps
	type pseudo_op = unit
			 
	fun toString () = ""
  
	fun emitValue _ = raise Fail "todo"
	fun sizeOf _ = raise Fail "todo"
	fun adjustLabels _ = raise Fail "todo"
      end (* Client *)
  
    structure PseudoOps = PseudoOps (structure Client = Client)
  end

structure SparcStream = InstructionStream(PseudoOps.PseudoOps)
structure SparcMLTreeStream = 
    MLTreeStream
      (structure T = SparcMLTree
       structure S = SparcStream)

(*
 * The assembler 
 *) 
structure SparcAsm = SparcAsmEmitter
   (structure Instr = SparcInstr
    structure Stream = SparcStream
    structure Shuffle = SparcShuffle
    structure S = SparcStream
    structure MLTreeEval = SparcMLTreeEval
    val V9 = false  (* we'll generate V8 instructions for now *)
   )

structure SparcPseudoInstrs = 
struct
  structure I = SparcInstr
  structure C = I.C

  type format1 =
       {r:CellsBasis.cell, i:I.operand, d:CellsBasis.cell} *
       (I.operand -> CellsBasis.cell) -> I.instruction list

  type format2 =
       {i:I.operand, d:CellsBasis.cell} *
       (I.operand -> CellsBasis.cell) -> I.instruction list

  fun error msg = MLRiscErrorMsg.impossible ("SparcPseudoInstrs."^msg)

  val delta = 0 (*SparcSpec.framesize*)	(* initial value of %fp - %sp *)

  (* runtime system dependent; the numbers are relative to %sp but
   * we need offsets relative to %fp, hence the adjustment by delta *)
  val floatTmpOffset = I.IMMED (88 - delta)
  val umulOffset = I.IMMED (80 - delta)
  val smulOffset = I.IMMED (72 - delta)
  val udivOffset = I.IMMED (84 - delta)
  val sdivOffset = I.IMMED (76 - delta)

  val stack = () (*CPSRegions.stack*)

  val native = true  (* use native versions of the instructions? *)

  fun umul_native({r, i, d}, reduceOpnd) =
      [I.arith{a=I.UMUL,r=r,i=i,d=d}]

  val TNE = I.ticc{t=I.BNE,cc=I.ICC,r=C.r0,i=I.IMMED 7}
  val TVS = I.ticc{t=I.BVS,cc=I.ICC,r=C.r0,i=I.IMMED 7}

      (* overflows iff Y != (d ~>> 31) *)
  fun smult_native({r, i, d}, reduceOpnd) =
      let val t1 = C.newReg()
          val t2 = C.newReg()
      in  [I.arith{a=I.SMUL,r=r,i=i,d=d},
           I.shift{s=I.SRA,r=d,i=I.IMMED 31,d=t1},
           I.rdy{d=t2},
           I.arith{a=I.SUBCC,r=t1,i=I.REG t2,d=C.r0},
           TNE
          ] 
      end

  fun smul_native({r, i, d}, reduceOpnd) =
      [I.arith{a=I.SMUL,r=r,i=i,d=d}]

  fun udiv_native({r,i,d},reduceOpnd) = 
      [I.wry{r=C.r0,i=I.REG C.r0},
       I.arith{a=I.UDIV,r=r,i=i,d=d}]

   (* May overflow if MININT div -1 *)
  fun sdivt_native({r,i,d},reduceOpnd) = 
      let val t1 = C.newReg()
      in  [I.shift{s=I.SRA,r=r,i=I.IMMED 31,d=t1},
           I.wry{r=t1,i=I.REG C.r0},
           I.arith{a=I.SDIVCC,r=r,i=i,d=d},
           TVS
          ]
      end

  fun sdiv_native({r,i,d},reduceOpnd) =
      let val t1 = C.newReg()
      in  [I.shift{s=I.SRA,r=r,i=I.IMMED 31,d=t1},
           I.wry{r=t1,i=I.REG C.r0},
           I.arith{a=I.SDIV,r=r,i=i,d=d}
          ]
      end

  (* 
   * Registers %o2, %o3 are used to pass arguments to ml_mul and ml_div 
   * Result is returned in %o2.
   *)
  val r10 = C.GPReg 10
  val r11 = C.GPReg 11

  fun callRoutine(offset,reduceOpnd,r,i,d) =   
  let val addr = C.newReg()
      val defs = C.addReg(r10,C.empty) 
      val uses = C.addReg(r10,C.addReg(r11,C.empty))
      fun copy{dst, src, tmp} = 
	  I.COPY{k=CellsBasis.GP, sz=32, dst=dst, src=src, tmp=tmp}
  in
      [copy{src=[r,reduceOpnd i],dst=[r10,r11],tmp=SOME(I.Direct(C.newReg()))},
       I.load{l=I.LD,r=C.frameptrR,i=offset,d=addr,mem=stack},
       I.jmpl{r=addr,i=I.IMMED 0,d=C.linkReg,defs=defs,uses=uses,
              cutsTo=[],nop=true,mem=stack},
       copy{src=[r10],dst=[d],tmp=NONE}
      ]
  end

  fun umul({r, i, d}, reduceOpnd) = callRoutine(umulOffset,reduceOpnd,r,i,d)
  fun smultrap({r, i, d}, reduceOpnd) = callRoutine(smulOffset,reduceOpnd,r,i,d)
  fun udiv({r, i, d}, reduceOpnd) = callRoutine(udivOffset,reduceOpnd,r,i,d)
  fun sdivtrap({r, i, d}, reduceOpnd) = callRoutine(sdivOffset,reduceOpnd,r,i,d)

  fun cvti2d({i, d}, reduceOpnd) = 
      [I.store{s=I.ST,r=C.frameptrR,i=floatTmpOffset,d=reduceOpnd i,mem=stack},
       I.fload{l=I.LDF,r=C.frameptrR,i=floatTmpOffset,d=d,mem=stack},
       I.fpop1{a=I.FiTOd,r=d,d=d}
      ]
  fun cvti2s _ = error "cvti2s"
  fun cvti2q _ = error "cvti2q"

     (* Generate native versions of the instructions *)
  val umul32 = if native then umul_native else umul
  val smul32 : format1 =
      if native then smul_native else (fn _ => error "smul32")
  val smul32trap = if native then smult_native else smultrap
  val udiv32 = if native then udiv_native else udiv
  val sdiv32 : format1 =
      if native then sdiv_native else (fn _ => error "sdiv32")
  val sdiv32trap = if native then sdivt_native else sdivtrap

  val overflowtrap32 = (* tvs 0x7 *)
                       [I.ticc{t=I.BVS,cc=I.ICC,r=C.r0,i=I.IMMED 7}]
  val overflowtrap64 = [] (* not needed *)

  fun save (r, opnd :SparcInstr.operand, d) = [I.save{r=r, i=opnd, d=d}]
  fun restore (r, opnd :SparcInstr.operand, d) = [I.restore{r=r, i=opnd, d=d}]

end

structure SparcMLTreeHash = 
    MLTreeHash
       (structure T = SparcMLTree
        fun h _ _ = 0w0
        val hashRext = h	val hashFext = h
        val hashCCext = h       val hashSext = h)

structure SparcProps = 
  SparcProps
    (structure SparcInstr = SparcInstr
     structure MLTreeEval = SparcMLTreeEval
     structure MLTreeHash = SparcMLTreeHash)

structure SparcAsmEmitter = 
  SparcAsmEmitter(structure Instr=SparcInstr
		  structure Shuffle=SparcShuffle
                  structure S = SparcStream
		  structure MLTreeEval=SparcMLTreeEval
                  val V9 = false)


structure SparcCFG = 
  ControlFlowGraph
     (structure I = SparcInstr
      structure PseudoOps = SparcPseudoOps
      structure GraphImpl = DirectedGraph
      structure InsnProps = SparcProps
      structure Asm = SparcAsmEmitter)

structure SparcFlowGraph = BuildFlowgraph 
	    (structure Props = SparcProps
             structure Stream = SparcStream
	     structure CFG = SparcCFG)

structure SparcExpand = CFGExpandCopies (structure CFG=SparcCFG
                                         structure Shuffle = SparcShuffle)
structure SparcBlockPlacement = DefaultBlockPlacement(SparcCFG)

structure SparcEmit = CFGEmit (
             structure CFG = SparcCFG
             structure E = SparcAsmEmitter) 

structure SparcCCall = SparcCCallFn (
		         structure T = SparcMLTree
			 fun ix x = raise Fail "")

(*
 * This module controls how we handle user extensions.  Since we don't
 * have any yet.  This is just a bunch of dummy routines.
 *)
structure SparcMLTreeExtComp : MLTREE_EXTENSION_COMP =
struct
   structure TS = SparcMLTreeStream
   structure I = SparcInstr
   structure T = SparcMLTree
   structure C = I.C
   structure Ext = UserExtension
   structure CFG = SparcCFG
   structure SparcCompInstrExt = 
     SparcCompInstrExt(structure I = I structure CFG = CFG structure TS=SparcMLTreeStream)

   type reducer = 
     (I.instruction,C.cellset,I.operand,I.addressing_mode, CFG.cfg) TS.reducer
   fun unimplemented _ = MLRiscErrorMsg.impossible "SparcMLTreeExtComp" 

   val compileSext  = SparcCompInstrExt.compileSext
   val compileRext  = unimplemented
   val compileCCext = unimplemented
   val compileFext  = unimplemented
end

    structure MLTreeComp=
       Sparc(structure SparcInstr = SparcInstr
             structure SparcMLTree = SparcMLTree
             structure PseudoInstrs = SparcPseudoInstrs
             structure ExtensionComp = SparcMLTreeExtComp
             val V9 = false
             val muluCost = ref 5
             val multCost = ref 3
             val divuCost = ref 5
             val divtCost = ref 5
             val registerwindow = ref false
             val useBR = ref false
            )


    structure InsnProps = SparcProps

    structure RA = 
       RISC_RA
         (structure I         = SparcInstr
       structure C         = CellsBasis
       structure T = SparcMLTree
          structure CFG       = SparcCFG
          structure InsnProps = InsnProps 
          structure Rewrite   = SparcRewrite(SparcInstr)
	  structure SpillInstr= SparcSpillInstr(SparcInstr)
          structure Asm       = SparcAsmEmitter
          structure SpillHeur = ChaitinSpillHeur
          structure Spill     = RASpill(structure InsnProps = InsnProps
                                        structure Asm = SparcAsmEmitter)

          structure SpillTable = SpillTable(val initialSpillOffset = 0 (* This is probably wrong!!!!! *)
            val spillAreaSz = 4000
            val architecture = "Sparc" )
          val fp = I.C.frameptrR
          val spill = UserRegion.spill
	  datatype spillOperandKind = SPILL_LOC | CONST_VAL
	  type spill_info = unit
          fun beforeRA _ = SpillTable.beginRA()

          val architecture = "Sparc"
         
          fun pure(I.ANNOTATION{i,...}) = pure i
            | pure(I.INSTR(I.LOAD _)) = true
            | pure(I.INSTR(I.FLOAD _)) = true
            | pure(I.INSTR(I.SETHI _)) = true
            | pure(I.INSTR(I.SHIFT _)) = true
            | pure(I.INSTR(I.FPop1 _)) = true
            | pure(I.INSTR(I.FPop2 _)) = true
            | pure _ = false

          (* make copy *) 
          structure Int = 
          struct
	               val dedicated = [I.C.stackptrR, I.C.GPReg 0]
             val avail     = 
		 C.SortedCells.return
              (C.SortedCells.difference(
                C.SortedCells.uniq(
                   SparcCells.Regs C.GP {from=0, to=31, step=1}),
                C.SortedCells.uniq dedicated)
              )

	     fun mkDisp loc = T.LI(T.I.fromInt(32, SpillTable.get loc))
             fun spillLoc{info, an, cell, id} = 
		  {opnd=I.Displace{base=fp, disp=mkDisp(RAGraph.FRAME id), mem=spill},
		   kind=SPILL_LOC}

             val mode = RACore.NO_OPTIMIZATION
          end

          structure Float = 
          struct
      fun fromto(n, m, inc) = if n>m then [] else n :: fromto(n+inc, m, inc)
	  val avail =  SparcCells.Regs C.FP {from=0, to=30, step=2}
	  val dedicated = []

	      fun mkDisp loc = T.LI(T.I.fromInt(32, SpillTable.getF loc))

             fun spillLoc(S, an, loc) = 
		I.Displace{base=fp, disp=mkDisp(RAGraph.FRAME loc), mem=spill}

             val mode = RACore.NO_OPTIMIZATION
          end
         )

structure Cells = SparcInstr.C
structure T = SparcMLTree
structure CFG = SparcCFG
structure FlowGraph = SparcFlowGraph
    val wordTy = 32


    val GP = SparcCells.GPReg
    val FP = SparcCells.FPReg

    fun greg r = GP r
    fun oreg r = GP (r + 8)
    fun ireg r = GP (r + 24)
    fun freg r = FP r
    fun reg32 r = T.REG (32, r)
    fun freg64 r = T.FREG (64, r)
    fun LI i = T.LI (T.I.fromInt (32, i))



in

structure SparcMLTree = SparcMLTree
structure SparcCCall = SparcCCall

structure SparcMLRISCGen =
  struct

    fun gen (functionName, stms, result) = let
           val insnStrm = FlowGraph.build()
	   val stream as SparcStream.STREAM
           { beginCluster,  (* start a cluster *)
             endCluster,    (* end a cluster *)
             emit,          (* emit MLTREE stm *)
             defineLabel,   (* define a local label *)
             entryLabel,    (* define an external entry *)
             exitBlock,     (* mark the end of a procedure *)
             pseudoOp,      (* emit a pseudo op *)
             annotation,    (* add an annotation *)
             ... } =
             MLTreeComp.selectInstructions insnStrm
	fun doit () = (
	    beginCluster 0;      (* start a new cluster *)
            pseudoOp PseudoOpsBasisTyp.TEXT;		  
	    pseudoOp (PseudoOpsBasisTyp.EXPORT [functionName]);    
            entryLabel functionName; (* define the entry label *)
            List.app emit stms; (* emit all the statements *)
            exitBlock result;
            endCluster [])
	val cfg = doit ()
	val cfg = RA.run cfg
	val cfg = SparcExpand.run cfg
        in  
         (cfg, stream)        (* end the cluster *)
       end

    fun dumpOutput (cfg, stream) = let
	val (cfg as Graph.GRAPH graph, blocks) = 
		SparcBlockPlacement.blockPlacement cfg
	val CFG.INFO{annotations=an, data, decls, ...} = #graph_info graph
	in
	  SparcEmit.asmEmit (cfg, blocks)
	end (* dumpOutput *)

    val GP = SparcCells.GPReg
    val FP = SparcCells.FPReg
    fun greg r = GP r
    fun oreg r = GP (r + 8)
    fun ireg r = GP (r + 24)
    fun freg r = FP r
    fun reg32 r = T.REG (32, r)
    fun freg64 r = T.FREG (64, r)
    fun LI i = T.LI (T.I.fromInt (32, i))
    val sp = oreg 6
   
    fun codegen (functionName, target, proto, initStms, args) = let 
        val _ = Label.reset()

	val [functionName, target] = List.map Label.global [functionName, target]

	(* construct the C call *)
	val {result, callseq} = SparcCCall.genCall {
	           name=T.LABEL target,
	           paramAlloc=fn _ => false,
(* FIXME *)
	           structRet=fn _ => T.REG(32, SparcCells.GPReg 0),
	           saveRestoreDedicated=fn _ => {save=[], restore=[]},
	           callComment=NONE,
	           proto=proto,
	           args=args}

	fun wordLit i = T.LI (T.I.fromInt (wordTy, i))

	fun offp i = T.ADD(32, T.REG (32, ireg 6), LI i)

	val stms = List.concat [
		   [T.EXT(SparcInstrExt.SAVE(T.REG(32, sp), LI(~112), T.REG(32, sp)))],
		   initStms,
		   callseq, 
		   [T.EXT(SparcInstrExt.RESTORE(T.REG(32, greg 0), T.REG(32, greg 0), T.REG(32, greg 0)))],
		   [T.JMP(T.ADD(32, T.REG(32, oreg 7), LI 8), [])]
		   ]

(*	val _ = List.all (fn stm => ChkTy.check stm 
				    orelse raise Fail ("typechecking error: "^SparcMTC.SparcMLTreeUtils.stmToString stm))
		stms
*)

        in
	   dumpOutput(gen (functionName, stms, result))
	end

  end

structure SparcTest = GenTestFn (
		  structure T = SparcMLTree
		  structure CCall = SparcCCall
		  structure Cells = SparcCells
		  val codegen = SparcMLRISCGen.codegen
		  val param0 = reg32(ireg 0)
		  val wordTy = 32)
end
