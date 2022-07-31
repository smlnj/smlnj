(*---------------------------------------------------------------------------
 * Backend specific stuff.  You'll need one instance of these things 
 * for each architecture.  
 *---------------------------------------------------------------------------*)

(*
 * The Sparc instruction set, specialized with respect to the
 * user constant and region types.  
 *)
structure SparcInstr = SparcInstr
   (structure LabelExp = LabelExp
    structure Region = UserRegion
   )

(*
 * How to serialize parallel copies
 *)
structure SparcShuffle = SparcShuffle(SparcInstr)

(*
 * The assembler 
 *) 
structure SparcAsm = SparcAsmEmitter
   (structure Instr = SparcInstr
    structure Stream = Stream
    structure Shuffle = SparcShuffle
    val V9 = false  (* we'll generate V8 instructions for now *)
   )

(*
 * The flowgraph (cluster) representation specialized to the sparc instruction 
 * set.
 *)
structure SparcFlowGraph = 
   FlowGraph(structure I = SparcInstr 
             structure P = UserPseudoOps
            )
(*
 * Because of various Sparc related ugliness.  Pseudo instructions 
 * related to integer multiplication/division are handled via callbacks.  
 * Here we can decide what actual code to generate.  Here we only
 * handle a subset of of the pseudo instructions.
 *)
structure SparcPseudoInstrs =
struct
  structure I = SparcInstr
  structure C = SparcInstr.C

  type format1 =
       {r:C.cell, i:I.operand, d:C.cell} *
       (I.operand -> I.C.cell) -> I.instruction list

  type format2 =
       {i:I.operand, d:C.cell} *
       (I.operand -> I.C.cell) -> I.instruction list

  fun error msg = MLRiscErrorMsg.impossible ("SparcPseudoInstrs."^msg)

  fun umul32({r, i, d}, reduceOpnd) = [I.ARITH{a=I.UMUL,r=r,i=i,d=d}]
  fun smul32({r, i, d}, reduceOpnd) = [I.ARITH{a=I.SMUL,r=r,i=i,d=d}]
  fun udiv32({r,i,d},reduceOpnd) = 
      [I.WRY{r=C.r0,i=I.REG(C.r0)},I.ARITH{a=I.UDIV,r=r,i=i,d=d}]

  fun sdiv32({r,i,d},reduceOpnd) =
  let val t1 = C.newReg()
  in  [I.SHIFT{s=I.SRA,r=r,i=I.IMMED 31,d=t1},
       I.WRY{r=t1,i=I.REG(C.r0)},
       I.ARITH{a=I.SDIV,r=r,i=i,d=d}
      ]
  end

  fun cvti2d({i,d},reduceOpnd) = error "cvti2d"
    (* There is no data path between integer and floating point registers.
       So we actually have to use some memory location for temporary
       This is commented out for now.
     *)
    (* 
      [I.STORE{s=I.ST,r=C.stackptrR,i=floatTmpOffset,d=reduceOpnd i,mem=stack},
       I.FLOAD{l=I.LDF,r=C.stackptrR,i=floatTmpOffset,d=d,mem=stack},
       I.FPop1{a=I.FiTOd,r=d,d=d}
      ]
    *)
  fun cvti2s _ = error "cvti2s"
  fun cvti2q _ = error "cvti2q"

  fun smul32trap _ = error "smul32trap"
  fun sdiv32trap _ = error "sdiv32trap"

  val overflowtrap32 = [] (* not needed *)
  val overflowtrap64 = [] (* not needed *)
end


(*
 * Instruction selection module for Sparc.  
 *)
structure SparcMLTreeComp = 
   Sparc(structure SparcInstr = SparcInstr
         structure SparcMLTree = MLTree
         structure PseudoInstrs = SparcPseudoInstrs
         structure ExtensionComp = UserMLTreeExtComp
           (structure I = SparcInstr
            structure T = SparcMLTree
           )
         (* Some sparc specific parameters *)
         val V9 = false
         val muluCost = ref 5
         val multCost = ref 3
         val divuCost = ref 5
         val divtCost = ref 5
         val registerwindow = ref false
         val useBR = ref false
        )


(*---------------------------------------------------------------------------
 * Okay.  Finally, we can tie the front-end and back-end together.
 *---------------------------------------------------------------------------*)
structure SparcBackEnd = 
   BackEnd
   (structure Flowgraph  = SparcFlowGraph
    structure MLTreeComp = SparcMLTreeComp
    structure InsnProps  = SparcProps(SparcInstr)
    structure Asm        = SparcAsm

    structure RA =
      RISC_RA 
      (structure I         = SparcInstr
       structure Flowgraph = Flowgraph
       structure Asm       = Asm
       structure InsnProps = InsnProps
       structure Spill     = RASpill(structure Asm = Asm
                                     structure InsnProps = InsnProps)
       structure Rewrite   = SparcRewrite(SparcInstr)
       structure SpillHeur = ChaitinSpillHeur
       structure C         = I.C
 
       val sp = C.stackptrR
       val spill = UserRegion.spill 

       structure SpillTable = SpillTable
           (val initialSpillOffset = 0 (* This is probably wrong!!!!! *)
            val spillAreaSz = 4000
            val architecture = "Sparc" 
           )
       open SpillTable
   
       fun pure(I.ANNOTATION{i,...}) = pure i
         | pure(I.LOAD _) = true
         | pure(I.FLOAD _) = true
         | pure(I.SETHI _) = true
         | pure(I.SHIFT _) = true
         | pure(I.FPop1 _) = true
         | pure(I.FPop2 _) = true
         | pure _ = false
   
       (* I'm assuming only r0 and the stack pointer is dedicated *)
       structure Int =
       struct
           val dedicated  = [I.C.stackptrR, I.C.GPReg 0]
           val avail = 
             C.SortedCells.return
              (C.SortedCells.difference(
                C.SortedCells.uniq(
                  C.Regs C.GP {from=0, to=31, step=1}),
                C.SortedCells.uniq dedicated)
              )

          fun copy((rds as [_], rss as [_]), _) =
              I.COPY{dst=rds, src=rss, impl=ref NONE, tmp=NONE}
            | copy((rds, rss), I.COPY{tmp, ...}) =
              I.COPY{dst=rds, src=rss, impl=ref NONE, tmp=tmp}

          (* spill copy temp *)
          fun spillCopyTmp(_,I.COPY{dst,src,tmp,impl},loc) =
              I.COPY{dst=dst, src=src, impl=impl,
                     tmp=SOME(I.Displace{base=sp, disp=get loc})}
       
          (* spill register *)
           fun spillInstr{an,src,spilledCell,spillLoc} =
               [I.STORE{s=I.ST, r=sp, i=I.IMMED(get spillLoc), d=src, 
                      mem=spill}]
           
          (* reload register *)
           fun reloadInstr{an,dst,spilledCell,spillLoc} =
                [I.LOAD{l=I.LD, r=sp, i=I.IMMED(get spillLoc), d=dst, 
                      mem=spill}]
       end

       structure Float = 
       struct
          val dedicated = []
          val avail     = C.Regs C.FP {from=0, to=31, step=2}
   
          fun copy((fds as [_], fss as [_]), _) =
              I.FCOPY{dst=fds, src=fss, impl=ref NONE, tmp=NONE}
            | copy((fds, fss), I.FCOPY{tmp, ...}) =
              I.FCOPY{dst=fds, src=fss, impl=ref NONE, tmp=tmp}
   
          fun spillCopyTmp(_,I.FCOPY{dst,src,tmp,impl},loc) =
              I.FCOPY{dst=dst, src=src, impl=impl,
                      tmp=SOME(I.Displace{base=sp, disp=getF loc})}
   
          fun spillInstr(_, d,loc) =
              [I.FSTORE{s=I.STDF, r=sp, i=I.IMMED(getF loc), d=d, mem=spill}]
   
          fun reloadInstr(_,d,loc) =
              [I.FLOAD{l=I.LDDF, r=sp, i=I.IMMED(getF loc), d=d, mem=spill}]
       end
      )
   )
