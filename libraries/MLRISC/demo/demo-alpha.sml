(*
 * The Alpha instruction set, specialized with respect to the
 * user constant and region types.  
 *)

structure AlphaMLTree =
   MLTreeF
   (structure Constant  = UserConstant
    structure Region    = UserRegion
    structure Extension = UserExtension
   )

structure AlphaInstr = AlphaInstr(AlphaMLTree)

structure AlphaMLTreeEval =
   MLTreeEval
   (structure T = AlphaMLTree
    fun eq _ _ = false
    val eqRext = eq val eqFext = eq
    val eqCCext = eq val eqSext = eq
   )

structure AlphaMLTreeHash =
   MLTreeHash
   (structure T = AlphaMLTree
    fun h _ _ = 0w0
    val hashRext = eq val hashFext = eq
    val hashCCext = eq val hashSext = eq
   )

(*
 * How to serialize parallel copies
 *)
structure AlphaShuffle = AlphaShuffle(AlphaInstr)

(*
 * The assembler 
 *) 
structure AlphaAsm = AlphaAsmEmitter
   (structure Instr = AlphaInstr
    structure Stream = Stream
    structure Shuffle = AlphaShuffle
    val V9 = false  (* we'll generate V8 instructions for now *)
   )

(*
 * The flowgraph (cluster) representation specialized to the sparc instruction 
 * set.
 *)
structure AlphaFlowGraph = 
   FlowGraph(structure I = AlphaInstr 
             structure P = UserPseudoOps
            )
(*
 * Alpha has no integer division.  So they have to be handled specially.
 * The following is stolen from Fermin's C-- source code.
 *)
structure AlphaPseudoInstrs : ALPHA_PSEUDO_INSTR =
struct

  fun error msg = MLRiscErrorMsg.error ("AlphaPseudoInstrs", msg)

  structure I = AlphaInstr
  structure T = MLTree
  structure C = I.C

  type reduceOpnd = I.operand -> C.cell

  (* reduceOpnd moves the operand to a register if it's not in one 
     already (handy).
     div*, rem* are assembler macros. The alpha/osf assembler accepts 
        divl $1, 7, $1
     but the alpha/linux assembler insists that the operand be a register
     Sigh ...
   *)

  val temps = foldr C.addReg C.empty (map C.GPReg [23, 24, 25, 26, 28])

  fun pseudoArith instr ({ra, rb, rc}, reduceOpnd) =
      [I.PSEUDOARITH{oper=instr, ra=ra, rb=I.REGop(reduceOpnd rb), rc=rc, tmps=temps}]

  fun divl  operands = pseudoArith I.DIVL operands
  fun divlu operands = pseudoArith I.DIVLU operands
  fun divq  operands = pseudoArith I.DIVQ  operands
  fun divqu operands = pseudoArith I.DIVQU operands
  fun divlv _ = error "divlv"
  fun divqv _ = error "divqv"

  fun reml  operands = pseudoArith I.REML  operands
  fun remlu operands = pseudoArith I.REMLU operands
  fun remq  operands = pseudoArith I.REMQ  operands
  fun remqu operands = pseudoArith I.REMQU operands
  fun remlv _ = error "remlv"
  fun remqv _ = error "remqv"

  val stack = I.Region.stack
  val sp = C.stackptrR

  val push16 = I.LDA{r=sp, b=sp, d=I.IMMop (~16)}
  val pop16  = I.LDA{r=sp, b=sp, d=I.IMMop 16}

  (**** int to float ****)

  (* i32 -> f32 *)
  fun cvtls({opnd, fd}, reduceOpnd) =
  let val ra = reduceOpnd opnd
  in
      [push16,
       I.STORE{stOp=I.STQ, r=ra, b=sp, d=I.IMMop 0, mem=stack},
       I.FLOAD{ldOp=I.LDT, r=fd, b=sp, d=I.IMMop 0, mem=stack},
       pop16,
       I.FUNARY{oper=I.CVTQS, fb=fd, fc=fd}]
  end

  (* i32 -> f64 *)
  fun cvtlt({opnd, fd}, reduceOpnd) =
  let val ra = reduceOpnd opnd
  in
      [push16,
       I.STORE{stOp=I.STQ, r=ra, b=sp, d=I.IMMop 0, mem=stack},
       I.FLOAD{ldOp=I.LDT, r=fd, b=sp, d=I.IMMop 0, mem=stack},
       pop16,
       I.FUNARY{oper=I.CVTQT, fb=fd, fc=fd}]
  end

  (* i64 -> f32 *)
  val  cvtqs = cvtls

  (* i64 -> f64 *)
  val cvtqt = cvtlt

  (**** float to int ****)

  (* TODO: These should really look at the rounding mode, and not generate
           CVTTQ_C blindly *)

  (* f32 -> i32 *)
  fun cvtsl({mode, fs, rd}) = let
      val ftmp = AlphaCells.newFreg()
      in
      [I.FUNARY{oper=I.CVTTQC, fb=fs, fc=ftmp},
       push16,
       I.FSTORE{stOp=I.STT, r=ftmp, b=sp, d=I.IMMop 0, mem=stack},
       I.LOAD  {ldOp=I.LDL, r=rd,   b=sp, d=I.IMMop 0, mem=stack},
       pop16
      ]
      end

  (* f64 -> i32 *)
  val cvttl= cvtsl


  (* f32 -> i64 *)
  fun cvtsq({mode, fs, rd}) = let
      val ftmp = AlphaCells.newFreg()
      in
      [I.FUNARY{oper=I.CVTTQC, fb=fs, fc=ftmp},
       push16,
       I.FSTORE{stOp=I.STT, r=ftmp, b=sp, d=I.IMMop 0, mem=stack},
       I.LOAD  {ldOp=I.LDQ, r=rd,   b=sp, d=I.IMMop 0, mem=stack},
       pop16
      ]
      end

  (* f64 -> i64 *)
  val cvttq = cvtsq


end (* AlphaPseudoInstrs *)

(*
 * Instruction selection module for Alpha.  
 *)
structure AlphaMLTreeComp = 
   Alpha(structure AlphaInstr = AlphaInstr
         structure AlphaMLTree = MLTree
         structure PseudoInstrs = AlphaPseudoInstrs
         structure ExtensionComp = UserMLTreeExtComp
           (structure I = AlphaInstr
            structure T = AlphaMLTree
           )
         (* Some alpha specific parameters *)
         val mode32bit = false (* simulate 32 bit mode *)
         val multCost = ref 8 (* just guessing *)
         val useMultByConst = ref false (* just guessing *)
         val byteWordLoadStores = ref false
         val SMLNJfloatingPoint = false (* must be true for SML/NJ *)
        )


(*
 * Alpha specific backend
 *)
structure AlphaBackEnd =
   BackEnd
   (structure I          = AlphaInstr
    structure Flowgraph  = AlphaFlowGraph
    structure InsnProps  = AlphaProps(AlphaInstr)
    structure Asm        = AlphaAsm
    structure MLTreeComp = AlphaMLTreeComp

    val sp = I.C.stackptrR
    val spill = UserRegion.spill

    (* I'm assuming only r31 and the stack pointer is dedicated *)
    structure RA =
      RISC_RA
      (structure I          = I
       structure C          = I.C
       structure Flowgraph  = Flowgraph
       structure Asm        = Asm
       structure Rewrite    = AlphaRewrite(AlphaInstr)
       structure InsnProps  = InsnProps
       structure Spill      = RASpill(structure Asm = Asm
                                      structure InsnProps = InsnProps)
       structure SpillHeur  = ChaitinSpillHeur
       structure SpillTable = 
         SpillTable  
         (val initialSpillOffset = 0 (* This is probably wrong!!!!! *)
          val spillAreaSz = 4000
          val architecture = "Alpha"
         )

       open SpillTable
   
       fun pure _ = false
   
       (* make copies *)
       structure Int =
       struct
          val dedicated  = [I.C.stackptrR, I.C.GPReg 31]
          val avail  = 
               C.SortedCells.return(
                 C.SortedCells.difference(
                    C.SortedCells.uniq(
                      C.Regs C.GP {from=0, to=31, step=1}),
                    C.SortedCells.uniq dedicated))

          fun copy((rds as [_], rss as [_]), _) =
              I.COPY{dst=rds, src=rss, impl=ref NONE, tmp=NONE}
            | copy((rds, rss), I.COPY{tmp, ...}) =
              I.COPY{dst=rds, src=rss, impl=ref NONE, tmp=tmp}
          (* spill register *)
          fun spillInstr{an,src,spilledCell,spillLoc} =
              [I.STORE{stOp=I.STL, b=sp, d=I.IMMop(get spillLoc), 
                       r=src, mem=spill}]

          (* spill copy temp *)
          fun spillCopyTmp(_,I.COPY{k,tmp,dst,src,impl},loc) =
              I.COPY{k=k,tmp=SOME(I.Displace{base=sp, disp=get loc}),
                     dst=dst,src=src,impl=impl}
      
          (* reload register *)
           fun reloadInstr{an,dst,spilledCell,spillLoc} =
               [I.LOAD{ldOp=I.LDL, b=sp, d=I.IMMop(get spillLoc), r=dst, 
                     mem=spill}]
       end

       structure Float = 
       struct
          val dedicated = [I.C.FPReg 31]
          val avail = C.Regs C.FP {from=0, to=30, step=1}

          fun copy((fds as [_], fss as [_]), _) =
              I.FCOPY{dst=fds, src=fss, impl=ref NONE, tmp=NONE}
            | copy((fds, fss), I.FCOPY{tmp, ...}) =
              I.FCOPY{dst=fds, src=fss, impl=ref NONE, tmp=tmp}
      
          fun spillCopyTmp(_,I.FCOPY{tmp,dst,src,impl},loc) =
              I.FCOPY{tmp=SOME(I.Displace{base=sp, disp=getF loc}),
                      dst=dst,src=src,impl=impl}
          fun spillInstr(_,r,loc) =
              [I.FSTORE{stOp=I.STT, b=sp, d=I.IMMop(getF loc), r=r, mem=spill}]
      
          fun reloadInstr(_,r,loc) =
              [I.FLOAD{ldOp=I.LDT, b=sp, d=I.IMMop(getF loc), r=r, mem=spill}]
       end
      )
   )

