(*
 * X86 specific register allocator.
 * This module abstracts out all the nasty RA business on the x86.  
 * So you should only have to write the callbacks.
 *)

functor X86RA 
  ( structure I          : X86INSTR
    structure InsnProps  : INSN_PROPERTIES where I = I
    structure F          : FLOWGRAPH where I = I
    structure Asm        : INSTRUCTION_EMITTER where I = I

      (* Spilling heuristics determines which node should be spilled. 
       * You can use Chaitin, ChowHenessey, or one of your own.
       *)
    structure SpillHeur : RA_SPILL_HEURISTICS 

      (* The Spill module figures out the strategies for inserting 
       * spill code.  You can use RASpill, or RASpillWithRenaming,
       * or write your own if you are feeling adventurous.
       *)
    structure Spill : RA_SPILL where I = I 

    sharing F.P = Asm.P

       (* Should we use allocate register on the floating point stack? 
        * Note that this flag must match the one passed to the code generator 
        * module.
        *)
    val fast_floating_point : bool ref

    datatype raPhase = SPILL_PROPAGATION 
                     | SPILL_COLORING

    (* Called before register allocation; perform your initialization here. *)
    val beforeRA : unit -> unit

    (* Integer register allocation parameters *)
    structure Int :
    sig
       val avail     : I.C.cell list
       val dedicated : I.C.cell list
       val memRegs   : I.C.cell list
       val phases    : raPhase list

       val spillLoc  : Annotations.annotations ref * 
                       RAGraph.logical_spill_id -> I.operand

       (* This function is called once before spilling begins *)
       val spillInit :  RAGraph.interferenceGraph -> unit

    end   

    (* Floating point register allocation parameters *)
    structure Float :
    sig
       (* Sethi-Ullman mode *)
       val avail     : I.C.cell list
       val dedicated : I.C.cell list
       val memRegs   : I.C.cell list
       val phases    : raPhase list

       val spillLoc  : Annotations.annotations ref * 
                       RAGraph.logical_spill_id -> I.operand

       (* This function is called once before spilling begins *)
       val spillInit : RAGraph.interferenceGraph -> unit

       (* When fast_floating_point is on, use these instead: *)
       val fastMemRegs : I.C.cell list
       val fastPhases  : raPhase list
    end

  ) : CLUSTER_OPTIMIZATION =
struct

    structure F = F
    structure I = I
    structure C = I.C

    val name = "X86RA"

    type flowgraph = F.cluster 

    val intSpillCnt = MLRiscControl.getCounter "ra-int-spills"
    val floatSpillCnt = MLRiscControl.getCounter "ra-float-spills"
    val intReloadCnt = MLRiscControl.getCounter "ra-int-reloads"
    val floatReloadCnt = MLRiscControl.getCounter "ra-float-reloads"
    val intRenameCnt = MLRiscControl.getCounter "ra-int-renames"
    val floatRenameCnt = MLRiscControl.getCounter "ra-float-renames"
    val x86CfgDebugFlg = MLRiscControl.getFlag "x86-cfg-debug"

(*
    val deadcode = MLRiscControl.getCounter "x86-dead-code"
    val deadblocks = MLRiscControl.getCounter "x86-dead-blocks"
 *)

    structure PrintFlowGraph=
       PrintCluster(structure Flowgraph=F
                    structure Asm = Asm)

    structure X86FP = 
       X86FP(structure X86Instr = I
             structure X86Props = InsnProps
             structure Flowgraph = F
             structure Liveness = Liveness(F)
             structure Asm = Asm
            )

    structure X86Spill = X86Spill(structure Instr=I structure Props=InsnProps)

    (* 
     * Dead code elimination 
     *)
    exception X86DeadCode
    val affectedBlocks =
	  IntHashTable.mkTable(32,X86DeadCode) : bool IntHashTable.hash_table
    val deadRegs       =
	  IntHashTable.mkTable(32,X86DeadCode) : bool IntHashTable.hash_table
    fun removeDeadCode(F.CLUSTER{blocks, ...}) =
    let val find = IntHashTable.find deadRegs
        fun isDead r = 
            case find (C.registerId r) of
               SOME _ => true
            |  NONE   => false
        fun isAffected i = getOpt (IntHashTable.find affectedBlocks i, false)
        fun isDeadInstr(I.ANNOTATION{i, ...}) = isDeadInstr i 
          | isDeadInstr(I.MOVE{dst=I.Direct rd, ...}) = isDead rd
          | isDeadInstr(I.MOVE{dst=I.MemReg rd, ...}) = isDead rd
          | isDeadInstr(I.COPY{dst=[rd], ...}) = isDead rd
          | isDeadInstr _ = false
        fun scan [] = ()
          | scan(F.BBLOCK{blknum, insns, ...}::rest) =
            (if isAffected blknum then 
                ((* deadblocks := !deadblocks + 1; *)
                 insns := elim(!insns, [])
                ) else ();
             scan rest)
          | scan(_::rest) = scan rest
       and elim([], code) = rev code
         | elim(i::instrs, code) = 
          if isDeadInstr i then 
             ((* deadcode := !deadcode + 1; *) elim(instrs, code))
          else elim(instrs, i::code)
    in if IntHashTable.numItems affectedBlocks > 0 then 
          (scan blocks;
	     IntHashTable.clear deadRegs;
	     IntHashTable.clear affectedBlocks)
       else ()
    end

    (* This function finds out which pseudo memory registers are unused.
     * Those that are unused are made available for spilling.
     * The register allocator calls this function right before spilling 
     * a set of nodes.
     *)
    val firstSpill = ref true
    val firstFPSpill = ref true

    fun spillInit(graph, I.C.GP) = 
        if !firstSpill then (* only do this once! *)
            (Int.spillInit graph;
             firstSpill := false
            )
         else ()
      | spillInit(graph, I.C.FP) = 
        if !firstFPSpill then
            (Float.spillInit graph;
             firstFPSpill := false
            )
        else ()
 
    (* This is the generic register allocator *)
    structure Ra = 
      RegisterAllocator
       (SpillHeur)
       (MemoryRA             (* for memory coalescing *)
         (RADeadCodeElim     (* do the funky dead code elimination stuff *)
            (ClusterRA
               (structure Flowgraph = F
                structure Asm = Asm
                structure InsnProps = InsnProps
                structure Spill = Spill
               )
            )
            (fun cellkind I.C.GP = true | cellkind _ = false
             val deadRegs = deadRegs
             val affectedBlocks = affectedBlocks
             val spillInit = spillInit
            )
         )
      )


    (* -------------------------------------------------------------------
     * Floating point stuff 
     * -------------------------------------------------------------------*)
    val KF32 = length Float.avail
    structure FR32 = GetReg(val nRegs=KF32 
                            val available=map C.registerId Float.avail
                            val first=C.registerId(I.C.ST 8))

    val availF8 = C.Regs C.FP {from=0, to=6, step=1}
    val KF8  = length availF8
    structure FR8  = GetReg(val nRegs=KF8
                            val available=map C.registerId availF8
                            val first=C.registerId(I.C.ST 0))
 
    (* -------------------------------------------------------------------
     * Callbacks for floating point K=32 
     * -------------------------------------------------------------------*)
    fun copyInstrF((rds as [_], rss as [_]), _) =
          I.FCOPY{dst=rds, src=rss, tmp=NONE}
      | copyInstrF((rds, rss), I.FCOPY{tmp, ...}) = 
          I.FCOPY{dst=rds, src=rss, tmp=tmp}
      | copyInstrF(x, I.ANNOTATION{i,a}) = 
          I.ANNOTATION{i=copyInstrF(x, i), a=a}

    val copyInstrF = fn x => [copyInstrF x]
 
    fun getFregLoc(an, Ra.FRAME loc) = Float.spillLoc(an, loc)
      | getFregLoc(an, Ra.MEM_REG r) = I.FDirect r

    (* spill floating point *)
    fun spillF{instr, reg, spillLoc, kill, annotations=an} = 
        (floatSpillCnt := !floatSpillCnt + 1;
         X86Spill.fspill(instr, reg, getFregLoc(an, spillLoc)) 
        )

    fun spillFreg{src, reg, spillLoc, annotations=an} = 
       (floatSpillCnt := !floatSpillCnt + 1;
        [I.FLDL(I.FDirect(src)), I.FSTPL(getFregLoc(an, spillLoc))]
       )

   fun spillFcopyTmp{copy=I.FCOPY{dst, src, ...}, spillLoc, 
                     annotations=an} =
        (floatSpillCnt := !floatSpillCnt + 1;
         I.FCOPY{dst=dst, src=src, tmp=SOME(getFregLoc(an, spillLoc))}
        )
     | spillFcopyTmp{copy=I.ANNOTATION{i,a}, spillLoc, annotations} =
        let val i = spillFcopyTmp{copy=i, spillLoc=spillLoc, 
                                  annotations=annotations}
        in  I.ANNOTATION{i=i, a=a} end

    (* rename floating point *)
    fun renameF{instr, fromSrc, toSrc} =
        (floatRenameCnt := !floatRenameCnt + 1;
         X86Spill.freload(instr, fromSrc, I.FDirect toSrc)
        )

    (* reload floating point *)
    fun reloadF{instr, reg, spillLoc, annotations=an} = 
        (floatReloadCnt := !floatReloadCnt + 1;
         X86Spill.freload(instr, reg, getFregLoc(an, spillLoc))
        )

    fun reloadFreg{dst, reg, spillLoc, annotations=an} = 
        (floatReloadCnt := !floatReloadCnt + 1;
         [I.FLDL(getFregLoc(an, spillLoc)), I.FSTPL(I.FDirect dst)]
        )

    (* -------------------------------------------------------------------
     * Callbacks for floating point K=7 
     * -------------------------------------------------------------------*)
    fun FMemReg f = let val fx = C.registerNum f
                    in  if fx >= 8 andalso fx < 32
                        then I.FDirect f else I.FPR f
                    end

    fun copyInstrF'((rds as [d], rss as [s]), _) =
         I.FMOVE{fsize=I.FP64,src=FMemReg s,dst=FMemReg d}
      | copyInstrF'((rds, rss), I.FCOPY{tmp, ...}) = 
         I.FCOPY{dst=rds, src=rss, tmp=tmp}
      | copyInstrF'(x, I.ANNOTATION{i, a}) =
         I.ANNOTATION{i=copyInstrF'(x,i), a=a}

    val copyInstrF' = fn x => [copyInstrF' x]

    fun spillFreg'{src, reg, spillLoc, annotations=an} = 
        (floatSpillCnt := !floatSpillCnt + 1;
         [I.FMOVE{fsize=I.FP64, src=FMemReg src, dst=getFregLoc(an,spillLoc)}]
        )

    fun renameF'{instr, fromSrc, toSrc} =
        (floatRenameCnt := !floatRenameCnt + 1;
         X86Spill.freload(instr, fromSrc, I.FPR toSrc)
        )

    fun reloadFreg'{dst, reg, spillLoc, annotations=an} = 
        (floatReloadCnt := !floatReloadCnt + 1;
         [I.FMOVE{fsize=I.FP64, dst=FMemReg dst, src=getFregLoc(an,spillLoc)}]
        )
 
    (* -------------------------------------------------------------------
     * Integer 8 stuff 
     * -------------------------------------------------------------------*)
    fun memToMemMove{dst, src} =
        let val tmp = I.C.newReg() 
        in  [I.MOVE{mvOp=I.MOVL,src=src,dst=I.Direct tmp},
             I.MOVE{mvOp=I.MOVL,src=I.Direct tmp,dst=dst}
            ]
        end

    fun copyInstrR((rds as [d], rss as [s]), _) =
        if C.sameColor(d,s) then [] else 
        let val dx = C.registerNum d and sx = C.registerNum s
        in  case (dx >= 8 andalso dx < 32, sx >= 8 andalso sx < 32) of
             (false, false) => [I.COPY{dst=rds, src=rss, tmp=NONE}]
           | (true, false) => [I.MOVE{mvOp=I.MOVL,src=I.Direct s,
                                      dst=I.MemReg d}]
           | (false, true) => [I.MOVE{mvOp=I.MOVL,src=I.MemReg s,
                                      dst=I.Direct d}]
           | (true, true) => memToMemMove{src=I.MemReg s, dst=I.MemReg d}
        end
      | copyInstrR((rds, rss), I.COPY{tmp, ...}) = 
         [I.COPY{dst=rds, src=rss, tmp=tmp}]
      | copyInstrR(x, I.ANNOTATION{i, a}) = 
          copyInstrR(x, i) (* XXX *)
      

    fun getRegLoc(an, Ra.FRAME loc) = Int.spillLoc(an, loc)
      | getRegLoc(an, Ra.MEM_REG r) = I.MemReg r

        (* No, logical spill locations... *)

    structure GR8 = GetReg(val nRegs=8 
                           val available=map C.registerId Int.avail
                           val first=0)
 
    val K8 = length Int.avail

     (* register allocation for general purpose registers *)
    fun spillR8{instr, reg, spillLoc, kill, annotations=an} = 
        (intSpillCnt := !intSpillCnt + 1;
         X86Spill.spill(instr, reg, getRegLoc(an, spillLoc))
        ) 

    fun isMemReg r = let val x = C.registerNum r
                     in  x >= 8 andalso x < 32 end
 
    fun spillReg{src, reg, spillLoc, annotations=an} = 
        let val _ = intSpillCnt := !intSpillCnt + 1;
            val dstLoc = getRegLoc(an,spillLoc)
            val isMemReg = isMemReg src
            val srcLoc = if isMemReg then I.MemReg src else I.Direct src
        in  if InsnProps.eqOpn(srcLoc, dstLoc) then []
            else if isMemReg then memToMemMove{dst=dstLoc, src=srcLoc}
            else [I.MOVE{mvOp=I.MOVL, src=srcLoc, dst=dstLoc}]
        end

    fun spillCopyTmp{copy=I.COPY{src, dst,...}, spillLoc, annotations=an} = 
        (intSpillCnt := !intSpillCnt + 1;
         I.COPY{dst=dst, src=src, tmp=SOME(getRegLoc(an, spillLoc))}
        )
   
    fun renameR8{instr, fromSrc, toSrc} = 
        (intRenameCnt := !intRenameCnt + 1;
         X86Spill.reload(instr, fromSrc, I.Direct toSrc)
        )

    fun reloadR8{instr, reg, spillLoc, annotations=an} = 
        (intReloadCnt := !intReloadCnt + 1;
         X86Spill.reload(instr, reg, getRegLoc(an,spillLoc))
        ) 

    fun reloadReg{dst, reg, spillLoc, annotations=an} = 
        let val _ = intReloadCnt := !intReloadCnt + 1
            val srcLoc = getRegLoc(an, spillLoc)
            val isMemReg = isMemReg dst
            val dstLoc = if isMemReg then I.MemReg dst else I.Direct dst
        in  if InsnProps.eqOpn(srcLoc,dstLoc) then []
            else if isMemReg then memToMemMove{dst=dstLoc, src=srcLoc}
            else [I.MOVE{mvOp=I.MOVL, src=srcLoc, dst=dstLoc}]
        end

    fun resetRA() = 
      (firstSpill := true;
       firstFPSpill := true;
       IntHashTable.clear affectedBlocks; 
       IntHashTable.clear deadRegs;
       if !fast_floating_point then FR8.reset() else FR32.reset(); 
       GR8.reset()
      )

    (* Dedicated + available registers *)
    fun mark(a, l) = app (fn r => Array.update(a, C.registerId r, true)) l

    val dedicatedR   = Array.array(32,false)
    val dedicatedF32 = Array.array(64,false)
    val dedicatedF8  = Array.array(64,false)
    val _ = mark(dedicatedR, Int.dedicated)
    val _ = mark(dedicatedF32, Float.dedicated)


    fun phases ps =
    let fun f([], m) = m
          | f(SPILL_PROPAGATION::ps, m) = f(ps, Ra.SPILL_PROPAGATION+m)
          | f(SPILL_COLORING::ps, m) = f(ps, Ra.SPILL_COLORING+m)
    in  f(ps, Ra.NO_OPTIMIZATION)
    end

    (* RA parameters *)

    (* How to allocate integer registers:    
     * Perform register alocation + memory allocation
     *)
    val RAInt = {spill     = spillR8,
                 spillSrc  = spillReg,
                 spillCopyTmp= spillCopyTmp,
                 reload    = reloadR8,
                 reloadDst = reloadReg,
                 renameSrc = renameR8,
                 copyInstr = copyInstrR,
                 K         = K8,
                 getreg    = GR8.getreg,
                 cellkind  = I.C.GP,   
                 dedicated = dedicatedR,
                 spillProh = [],
                 memRegs   = Int.memRegs,
                 mode      = phases(Int.phases)
                } : Ra.raClient

    (* How to allocate floating point registers:    
     * Allocate all fp registers on the stack.  This is the easy way.
     *)
    val RAFP32 ={spill     = spillF,
                 spillSrc  = spillFreg,
                 spillCopyTmp= spillFcopyTmp,
                 reload    = reloadF,
                 reloadDst = reloadFreg,
                 renameSrc = renameF,
                 copyInstr = copyInstrF,
                 K         = KF32,
                 getreg    = FR32.getreg,
                 cellkind  = I.C.FP,   
                 dedicated = dedicatedF32,
                 spillProh = [],
                 memRegs   = Float.memRegs,
                 mode      = phases(Float.phases)
                } : Ra.raClient

    (* How to allocate floating point registers:    
     * Allocate fp registers on the %st stack.  Also perform
     * memory allcoation.
     *)
     val RAFP8 ={spill     = spillF,
                 spillSrc  = spillFreg',
                 spillCopyTmp= spillFcopyTmp,
                 reload    = reloadF,
                 reloadDst = reloadFreg',
                 renameSrc = renameF',
                 copyInstr = copyInstrF',
                 K         = KF8,
                 getreg    = FR8.getreg,
                 cellkind  = I.C.FP,   
                 dedicated = dedicatedF8,
                 spillProh = [],
                 memRegs   = Float.fastMemRegs,
                 mode      = phases(Float.fastPhases) 
                } : Ra.raClient

    (* Two RA modes, fast and normal *) 
    val fast_fp = [RAInt, RAFP8]
    val normal_fp = [RAInt, RAFP32]
 
    (* The main ra routine *)
    fun run cluster =
    let val printGraph = 
            if !x86CfgDebugFlg then 
               PrintFlowGraph.printCluster(!MLRiscControl.debug_stream)
            else fn msg => fn _ => () 

        val _ = beforeRA() 
        val _ = resetRA()

        (* generic register allocator *)

        val cluster = Ra.ra
                      (if !fast_floating_point then fast_fp else normal_fp)
                      cluster

        val _ = removeDeadCode cluster

        val _ = printGraph "\t---After register allocation K=8---\n" cluster

        (* Run the FP translation phase when fast floating point has
         * been enabled
         *)
        val cluster = 
             if !fast_floating_point andalso I.C.numCell I.C.FP () > 0 then 
             let val cluster = X86FP.run cluster
             in  printGraph "\t---After X86 FP translation ---\n" cluster;
                 cluster
             end
             else cluster
    in  cluster
    end

end
