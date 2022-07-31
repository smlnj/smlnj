(*
 * X86 specific register allocator.
 * This module abstracts out all the nasty RA business on the x86.  
 * So you should only have to write the callbacks.
 *
 *   Here's more some info on the x86 functor.
 *Basically the new functor encapsulates all the features in the
 *x86 register allocator, including things like memory pseudo registers,
 *and the new floating point allocator that maps things onto the %st registers.
 *For floating point, we can also switch between the sethi-ullman mode and 
 *the %st register mode.
 *
 *   Notes on the parameters of the functor: 
 *
 *>   structure SpillHeur : RA_SPILL_HEURISTICS
 *
 *   This should be one of the spill heuristic module like ChaitinSpillHeur or
 * Command ('i' to return to index):  you can also roll your own.
 *
 *>   structure Spill : RA_SPILL 
 *
 *   This should be either RASpill or RASpillWithRenaming.
 *
 *>   val fast_floating_point : bool ref
 *
 *    This flag is used to turn on the new x86 fp mode.  The same flag
 *    is also passed to the x86 instruction selection module.
 *
 *>   datatype raPhase = SPILL_PROPAGATION | SPILL_COLORING
 *
 *    This datatype specifies which additional phases we should run.
 *
 *>   val beforeRA : flowgraph -> spill_info
 *
 *    This callback is invoked before each call to RA.  The RA may have
 *    to perform both integer and floating point RA.  This is called before
 *    integer RA.   
 *
 *    The callbacks for integer and floating point are separated into
 *    the substructures Int and Float.
 *
 *>   structure Int :
 *>   sig
 *>      val avail     : I.C.cell list
 *>      val dedicated : I.C.cell list
 *>      val memRegs   : I.C.cell list
 *>      val phases    : raPhase list
 *>      val spillLoc  : spill_info * Annotations.annotations ref *
 *>                      RAGraph.logical_spill_id -> I.operand
 *>      val spillInit :  RAGraph.interferenceGraph -> unit
 *>   end                 
 *
 *    avail is the list of registers available for allocation
 *    memRegs is the list of memory registers that may appear in the program
 *    phases is a list of additional RA phases.  I recommend turning on 
 *    everything:
 *
 *         [SPILL_PROPAGATION, SPILL_COLORING]
 *
 *    spillInit is called once before spilling occurs.
 *
 *    spillLoc is a callback that maps logical_spill_ids into an x86
 *    effective address.  The list of allocations is from the block in which
 *    the spilled instruction occurs.  The client should keep track of 
 *    existing ids, and allocate a new effective address when a new id occurs.
 *    In general, the client should keep track of a single table of free
 *    spill space for both integer and floating point registers.
 *
 *    Previously, the spill/reload routines have to do special things in the
 *    presence of memory registers, but that stuff is taken care of in the
 *    new module, so all spillLoc has to do is map logical_spill_ids into
 *    effective address.
 *
 *>   structure Float :
 *>   sig
 *>      val avail     : I.C.cell list
 *>      val dedicated : I.C.cell list
 *>      val memRegs   : I.C.cell list
 *>      val phases    : raPhase list
 *>      val spillLoc  : spill_info * Annotations.annotations ref *
 *>                      RAGraph.logical_spill_id -> I.operand
 *>      val spillInit : RAGraph.interferenceGraph -> unit
 *>   end   
 *
 *    For floating point, it is similar.
 *
 *>   
 *>      val fastMemRegs : I.C.cell list
 *>      val fastPhases  : raPhase list
 *
 *    When fast_floating_point is turned on, we use different parameters:  
 *
 *    avail is set to [%st(0), ..., %st(6)]  
 *    dedicated is set to []
 *    memRegs is set to fastMemRegs
 *
 *    In general, the flow of the module is like this:
 *
 *    ra:
 *         call beforeRA()
 *         integer RA --- call Int.spillInit() once if spilling is needed
 *         floating fp RA --- call Real.spillInit() once if spilling is needed
 *         if !fast_floating_point then
 *            invoke the module X86FP to convert fake %fp registers 
 *            into real %st registers
 *         endif
 *
 *)

functor X86RA 
  ( structure I          : X86INSTR
    structure InsnProps  : INSN_PROPERTIES 
			       where I = I
    structure CFG        : CONTROL_FLOW_GRAPH 
			       where I = I
    structure Asm        : INSTRUCTION_EMITTER 
			       where I = I 
				 and S.P = CFG.P

      (* Spilling heuristics determines which node should be spilled 
       * You can use Chaitin, ChowHenessey, or one of your own.
       *)
    structure SpillHeur : RA_SPILL_HEURISTICS 

      (* The Spill module figures out the strategies for inserting 
       * spill code.  You can use RASpill, or RASpillWithRenaming,
       * or write your own if you are feeling adventurous.
       *)
    structure Spill : RA_SPILL where I = I 


    type spill_info (* user-defined abstract type *)

       (* Should we use allocate register on the floating point stack? 
        * Note that this flag must match the one passed to the code generator 
        * module.
        *)
    val fast_floating_point : bool ref

    datatype raPhase = SPILL_PROPAGATION 
                     | SPILL_COLORING

    datatype spillOperandKind = SPILL_LOC | CONST_VAL

    (* Called before register allocation; perform your initialization here. *)
    val beforeRA : CFG.cfg -> spill_info

    (* Integer register allocation parameters *)
    structure Int :
    sig
       val avail     : CellsBasis.cell list
       val dedicated : CellsBasis.cell list
       val memRegs   : CellsBasis.cell list
       val phases    : raPhase list

       val spillLoc  : {info:spill_info,
                        an  :Annotations.annotations ref,
                        cell:CellsBasis.cell, (* spilled cell *)
                        id  :RAGraph.logical_spill_id
                       } -> 
                       { opnd: I.ea,
                         kind: spillOperandKind
                       }

       (* This function is called once before spilling begins *)
       val spillInit :  RAGraph.interferenceGraph -> unit

    end   

    (* Floating point register allocation parameters *)
    structure Float :
    sig
       (* Sethi-Ullman mode *)
       val avail     : CellsBasis.cell list
       val dedicated : CellsBasis.cell list
       val memRegs   : CellsBasis.cell list
       val phases    : raPhase list

       val spillLoc  : spill_info * Annotations.annotations ref * RAGraph.logical_spill_id 
		           -> I.ea

       (* This function is called once before spilling begins *)
       val spillInit : RAGraph.interferenceGraph -> unit

       (* When fast_floating_point is on, use these instead: *)
       val fastMemRegs : CellsBasis.cell list
       val fastPhases  : raPhase list
    end

  ) : CFG_OPTIMIZATION =
struct

    structure CFG = CFG
    structure I = I
    structure C = I.C
    structure CB = CellsBasis

    val name = "X86RA"

    type flowgraph = CFG.cfg

    val intSpillCnt = MLRiscControl.mkCounter ("ra-int-spills", "RA int spill count")
    val intReloadCnt = MLRiscControl.mkCounter ("ra-int-reloads", "RA int reload count")
    val intRenameCnt = MLRiscControl.mkCounter ("ra-int-renames", "RA int rename count")
    val floatSpillCnt = MLRiscControl.mkCounter ("ra-float-spills", "RA float spill count")
    val floatReloadCnt = MLRiscControl.mkCounter ("ra-float-reloads", "RA float reload count")
    val floatRenameCnt = MLRiscControl.mkCounter ("ra-float-renames", "RA float rename count")

    fun inc c = c := !c + 1

    val x86CfgDebugFlg = MLRiscControl.mkFlag ("x86-cfg-debug", "x86 CFG debug mode")

    fun error msg = MLRiscErrorMsg.error("X86RA",msg)

(*
    val deadcode = MLRiscControl.getCounter "x86-dead-code"
    val deadblocks = MLRiscControl.getCounter "x86-dead-blocks"
 *)

    structure PrintFlowgraph=
       PrintFlowgraph(structure CFG=CFG
                      structure Asm = Asm)

    structure X86FP = 
       X86FP(structure X86Instr = I
             structure X86Props = InsnProps
             structure Flowgraph = CFG
             structure Liveness = Liveness(CFG)
             structure Asm = Asm
            )

   structure X86SpillInstr = X86SpillInstr(structure Instr=I structure Props=InsnProps)
   val spillFInstr = X86SpillInstr.spill CB.FP
   val reloadFInstr = X86SpillInstr.reload CB.FP
   val spillInstr = X86SpillInstr.spill CB.GP
   val reloadInstr = X86SpillInstr.reload CB.GP

   fun annotate([], i) = i
     | annotate(a::an, i) = annotate(an, I.ANNOTATION{a=a, i=i})

    (* 
     * Dead code elimination 
     *)
    exception X86DeadCode
    val affectedBlocks =
	  IntHashTable.mkTable(32,X86DeadCode) : bool IntHashTable.hash_table
    val deadRegs       =
	  IntHashTable.mkTable(32,X86DeadCode) : bool IntHashTable.hash_table

    fun removeDeadCode(cfg as Graph.GRAPH graph) = let
        val blocks = #nodes graph ()
        val find = IntHashTable.find deadRegs
        fun isDead r = 
            case find (CB.cellId r) of
               SOME _ => true
            |  NONE   => false
        fun isAffected i = getOpt (IntHashTable.find affectedBlocks i, false)
        fun isDeadInstr(I.ANNOTATION{i, ...}) = isDeadInstr i 
          | isDeadInstr(I.INSTR(I.MOVE{dst=I.Direct rd, ...})) = isDead rd
          | isDeadInstr(I.INSTR(I.MOVE{dst=I.MemReg rd, ...})) = isDead rd
          | isDeadInstr(I.COPY{k=CB.GP, dst=[rd], ...}) = isDead rd
          | isDeadInstr _ = false
        fun scan [] = ()
          | scan((blknum, CFG.BLOCK{insns, ...})::rest) =
            (if isAffected blknum then 
                ((* deadblocks := !deadblocks + 1; *)
                 insns := elim(!insns, [])
                ) else ();
             scan rest)
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

    fun spillInit(graph, CB.GP) = 
        if !firstSpill then (* only do this once! *)
            (Int.spillInit graph;
             firstSpill := false
            )
         else ()
      | spillInit(graph, CB.FP) = 
        if !firstFPSpill then
            (Float.spillInit graph;
             firstFPSpill := false
            )
        else ()
      | spillInit _ = error "spillInit"
 
    (* This is the generic register allocator *)
    structure Ra = 
      RegisterAllocator
       (SpillHeur)
       (MemoryRA             (* for memory coalescing *)
         (RADeadCodeElim     (* do the funky dead code elimination stuff *)
            (ClusterRA
               (structure Flowgraph = CFG
                structure Asm = Asm
                structure InsnProps = InsnProps
                structure Spill = Spill
               )
            )
            (fun cellkind CB.GP = true | cellkind _ = false
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
                            val available=map CB.registerId Float.avail
                            val first=CB.registerId(I.C.ST 8))

    val availF8 = C.Regs CB.FP {from=0, to=6, step=1}
    val KF8  = length availF8
    structure FR8  = GetReg(val nRegs=KF8
                            val available=map CB.registerId availF8
                            val first=CB.registerId(I.C.ST 0))
 
    (* -------------------------------------------------------------------
     * Callbacks for floating point K=32 
     * -------------------------------------------------------------------*)
    fun fcopy{dst, src, tmp} = 
	I.COPY{k=CB.FP, sz=64, dst=dst, src=src, tmp=tmp}

    fun copyInstrF((rds as [_], rss as [_]), _) =
          fcopy{dst=rds, src=rss, tmp=NONE}
      | copyInstrF((rds, rss), I.COPY{k=CB.FP, tmp, ...}) = 
          fcopy{dst=rds, src=rss, tmp=tmp}
      | copyInstrF(x, I.ANNOTATION{i,a}) = 
          I.ANNOTATION{i=copyInstrF(x, i), a=a}
      | copyInstrF _ = error "copyInstrF"

    val copyInstrF = fn x => [copyInstrF x]
 
    fun getFregLoc(S, an, Ra.FRAME loc) = Float.spillLoc(S, an, loc)
      | getFregLoc(S, an, Ra.MEM_REG r) = I.FDirect r

    (* spill floating point *)
    fun spillF S {annotations=an, kill, reg, spillLoc, instr} = let
      (* preserve annotation on instruction *)
      fun spill(instrAn, I.ANNOTATION{a, i}) = spill(a::instrAn, i)
	| spill(instrAn, I.KILL{regs, spilled}) = 
	   {code=
	      [annotate
		(instrAn, 
		 I.KILL {regs=C.rmvFreg(reg, regs), 
			 spilled=C.addFreg(reg, spilled)})],
	     proh = [], 
	     newReg=NONE}
	| spill(instrAn, I.LIVE _) = error "spillF: LIVE"
	| spill(_, I.COPY _) = error "spillF: COPY"
	| spill(instrAn, I.INSTR _) = 
	  (inc floatSpillCnt;
	   spillFInstr(instr, reg, getFregLoc(S, an, spillLoc)))
    in spill([], instr)
    end

    fun spillFreg S {src, reg, spillLoc, annotations=an} = 
       (inc floatSpillCnt;
        let val fstp = [I.fstpl(getFregLoc(S, an, spillLoc))]
        in  if CB.sameColor(src,C.ST0) then fstp
            else I.fldl(I.FDirect(src))::fstp
        end
       )

   fun spillFcopyTmp S {copy=I.COPY{k=CB.FP, dst, src, ...}, spillLoc, reg,
                        annotations=an} =
        (inc floatSpillCnt;
         fcopy{dst=dst, src=src, tmp=SOME(getFregLoc(S, an, spillLoc))}
        )
     | spillFcopyTmp S {copy=I.ANNOTATION{i,a}, spillLoc, reg, annotations} =
        let val i = spillFcopyTmp S {copy=i, spillLoc=spillLoc, reg=reg,
                                     annotations=annotations}
        in  I.ANNOTATION{i=i, a=a} end
     | spillFcopyTmp _ _ = error "spillFcopyTmp"

    (* rename floating point *)
    fun renameF{instr, fromSrc, toSrc} =
        (inc floatRenameCnt;
         reloadFInstr(instr, fromSrc, I.FDirect toSrc)
        )

    (* reload floating point *)
    fun reloadF S {annotations=an,reg,spillLoc,instr} = let
      fun reload(instrAn, I.ANNOTATION{a,i}) = reload(a::instrAn, i)
	| reload(instrAn, I.LIVE{regs, spilled}) = 
	   {code=[I.LIVE{regs=C.rmvFreg(reg, regs), spilled=C.addFreg(reg, spilled)}],
	    proh=[],
	    newReg=NONE}
	| reload(_, I.KILL _) = error "reloadF: KILL"
	| reload (_, I.COPY _) = error "reloadF: COPY"
	| reload(instrAn, instr as I.INSTR _) = 
  	   (inc floatReloadCnt;
	    reloadFInstr(instr, reg, getFregLoc(S, an, spillLoc)))
    in reload([], instr)
    end

    fun reloadFreg S {dst, reg, spillLoc, annotations=an} = 
        (inc floatReloadCnt;
         if CB.sameColor(dst,C.ST0) then 
            [I.fldl(getFregLoc(S, an, spillLoc))]
         else  
            [I.fldl(getFregLoc(S, an, spillLoc)), I.fstpl(I.FDirect dst)]
        )

    (* -------------------------------------------------------------------
     * Callbacks for floating point K=7 
     * -------------------------------------------------------------------*)
    fun FMemReg f = let val fx = CB.registerNum f
                    in  if fx >= 8 andalso fx < 32
                        then I.FDirect f else I.FPR f
                    end

    fun copyInstrF'((rds as [d], rss as [s]), _) =
         I.fmove{fsize=I.FP64,src=FMemReg s,dst=FMemReg d}
      | copyInstrF'((rds, rss), I.COPY{k=CB.FP, tmp, ...}) = 
         fcopy{dst=rds, src=rss, tmp=tmp}
      | copyInstrF'(x, I.ANNOTATION{i, a}) =
         I.ANNOTATION{i=copyInstrF'(x,i), a=a}
      | copyInstrF' _ = error "copyInstrF'"

    val copyInstrF' = fn x => [copyInstrF' x]

    fun spillFreg' S {src, reg, spillLoc, annotations=an} = 
        (inc floatSpillCnt;
         [I.fmove{fsize=I.FP64, src=FMemReg src, 
                  dst=getFregLoc(S, an,spillLoc)}]
        )

    fun renameF'{instr, fromSrc, toSrc} =
        (inc floatRenameCnt;
         reloadFInstr(instr, fromSrc, I.FPR toSrc)
        )

    fun reloadFreg' S {dst, reg, spillLoc, annotations=an} = 
        (inc floatReloadCnt;
         [I.fmove{fsize=I.FP64, dst=FMemReg dst, 
                  src=getFregLoc(S,an,spillLoc)}]
        )
 
    (* -------------------------------------------------------------------
     * Integer 8 stuff 
     * -------------------------------------------------------------------*)
    fun copy{dst, src, tmp} = I.COPY{k=CB.GP, sz=32, dst=dst, src=src, tmp=tmp}
    fun memToMemMove{dst, src} =
        let val tmp = I.C.newReg() 
        in  [I.move{mvOp=I.MOVL,src=src,dst=I.Direct tmp},
             I.move{mvOp=I.MOVL,src=I.Direct tmp,dst=dst}
            ]
        end

    fun copyInstrR((rds as [d], rss as [s]), _) =
        if CB.sameColor(d,s) then [] else 
        let val dx = CB.registerNum d and sx = CB.registerNum s
        in  case (dx >= 8 andalso dx < 32, sx >= 8 andalso sx < 32) of
             (false, false) => [copy{dst=rds, src=rss, tmp=NONE}]
           | (true, false) => [I.move{mvOp=I.MOVL,src=I.Direct s,
                                      dst=I.MemReg d}]
           | (false, true) => [I.move{mvOp=I.MOVL,src=I.MemReg s,
                                      dst=I.Direct d}]
           | (true, true) => memToMemMove{src=I.MemReg s, dst=I.MemReg d}
        end
      | copyInstrR((rds, rss), I.COPY{k=CB.GP, tmp, ...}) = 
         [copy{dst=rds, src=rss, tmp=tmp}]
      | copyInstrR(x, I.ANNOTATION{i, a}) = 
          copyInstrR(x, i) (* XXX *)
      | copyInstrR _ = error "copyInstrR"
      

    fun getRegLoc(S, an, cell, Ra.FRAME loc) = 
         Int.spillLoc{info=S, an=an, cell=cell, id=loc}
      | getRegLoc(S, an, cell, Ra.MEM_REG r) = {opnd=I.MemReg r,kind=SPILL_LOC}

        (* No, logical spill locations... *)

    structure GR8 = GetReg(val nRegs=8 
                           val available=map CB.registerId Int.avail
                           val first=0)
 
    val K8 = length Int.avail

     (* register allocation for general purpose registers *)
    fun spillR8 S {annotations=an, kill, reg, spillLoc, instr} = let
      fun annotate([], i) = i
	| annotate(a::an, i) = annotate(an, I.ANNOTATION{a=a, i=i})

      (* preserve annotation on instruction *)
      fun spill(instrAn, I.ANNOTATION{a,i}) = spill(a::instrAn, i)
	| spill(instrAn, I.KILL{regs, spilled}) = 
	   {code=
	      [annotate
		(instrAn, 
		 I.KILL {regs=C.rmvReg(reg, regs), 
			 spilled=C.addReg(reg, spilled)})],
	     proh = [], 
	     newReg=NONE}
	| spill(instrAn, I.LIVE _) = error "spill: LIVE"
	| spill(_, I.COPY _) = error "spill: COPY"
	| spill(instrAn, I.INSTR _) = 
	  (case getRegLoc(S, an, reg, spillLoc) 
	    of {opnd=spillLoc, kind=SPILL_LOC} => 
		   ( inc intSpillCnt;
		     spillInstr(annotate(instrAn, instr), reg, spillLoc)
		    ) 
	     | _ => (* don't have to spill a constant *)
		   {code=[], newReg=NONE, proh=[]} 
	  (*esac*))
    in spill([], instr)
    end

    fun isMemReg r = let val x = CB.registerNum r
                     in  x >= 8 andalso x < 32 end
 
    fun spillReg S {src, reg, spillLoc, annotations=an} = 
        let val _ = inc intSpillCnt
            val {opnd=dstLoc,kind} = getRegLoc(S,an,reg,spillLoc)
            val isMemReg = isMemReg src
            val srcLoc = if isMemReg then I.MemReg src else I.Direct src
        in  if kind=CONST_VAL orelse InsnProps.eqOpn(srcLoc, dstLoc) then []
            else if isMemReg then memToMemMove{dst=dstLoc, src=srcLoc}
            else [I.move{mvOp=I.MOVL, src=srcLoc, dst=dstLoc}]
        end

    fun spillCopyTmp S {copy=I.COPY{k=CB.GP, src, dst,...}, 
                        reg, spillLoc, annotations=an} = 
        (case getRegLoc(S, an, reg, spillLoc) of
           {opnd=tmp, kind=SPILL_LOC} =>
            (inc intSpillCnt;
             copy{dst=dst, src=src, tmp=SOME tmp}
            )
         | _ => error "spillCopyTmp"
        )
      | spillCopyTmp S {copy=I.ANNOTATION{i, a}, reg, spillLoc, annotations} =
        I.ANNOTATION{i=spillCopyTmp S {copy=i, reg=reg, spillLoc=spillLoc,
                                       annotations=annotations}, a=a}
      | spillCopyTmp _ _ = error "spillCopyTmp(2)"
   
    fun renameR8{instr, fromSrc, toSrc} = 
        (inc intRenameCnt;
         reloadInstr(instr, fromSrc, I.Direct toSrc)
        )


    fun reloadR8 S {annotations=an, reg, spillLoc, instr} = let
      fun reload(instrAn, I.ANNOTATION{a,i}) = reload(a::instrAn, i)
	| reload(instrAn, I.LIVE{regs, spilled}) = 
	   {code=[I.LIVE{regs=C.rmvReg(reg, regs), spilled=C.addReg(reg, spilled)}],
	    proh=[],
	    newReg=NONE}
	| reload(_, I.KILL _) = error "reload: KILL"
	| reload (_, I.COPY _) = error "reload: COPY"
	| reload(instrAn, instr as I.INSTR _)  = 
  	 ( inc intReloadCnt;
	   reloadInstr(annotate(instrAn, instr), reg, #opnd(getRegLoc(S,an,reg,spillLoc)))
  	  ) 
    in reload([], instr)
    end 

    fun reloadReg S {dst, reg, spillLoc, annotations=an} = 
        let val _ = inc intReloadCnt
            val srcLoc = #opnd(getRegLoc(S, an, reg, spillLoc))
            val isMemReg = isMemReg dst
            val dstLoc = if isMemReg then I.MemReg dst else I.Direct dst
        in  if InsnProps.eqOpn(srcLoc,dstLoc) then []
            else if isMemReg then memToMemMove{dst=dstLoc, src=srcLoc}
            else [I.move{mvOp=I.MOVL, src=srcLoc, dst=dstLoc}]
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
    local 
      fun mark(arr, _, [], others) = others
	| mark(arr, len, r::rs, others) = let
	    val r = CB.registerId r
          in
	    if r >= len then mark(arr, len, rs, r::others)
	    else (Array.update(arr, r, true); mark(arr, len, rs, others))
          end
      val dedicatedR   = Array.array(32,false)
      val dedicatedF32 = Array.array(64,false)
      val otherR = mark(dedicatedR, 32, Int.dedicated, [])
      val otherF32 = mark(dedicatedF32, 64, Float.dedicated, [])
      fun isDedicated (len, arr, other) r = 
	(r < len andalso Array.sub(arr, r)) orelse List.exists (fn d => r = d) other
    in
      val isDedicatedR : int -> bool = isDedicated (32, dedicatedR, otherR)
      val isDedicatedF32 : int -> bool = isDedicated (64, dedicatedF32, otherF32)
      val isDedicatedF8 : int -> bool = fn _ => false
    end

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
    fun RAInt S = 
                {spill     = spillR8 S,
                 spillSrc  = spillReg S,
                 spillCopyTmp= spillCopyTmp S,
                 reload    = reloadR8 S,
                 reloadDst = reloadReg S,
                 renameSrc = renameR8,
                 copyInstr = copyInstrR,
                 K         = K8,
                 getreg    = GR8.getreg,
                 cellkind  = CB.GP,   
                 dedicated = isDedicatedR,
                 spillProh = [],
                 memRegs   = Int.memRegs,
                 mode      = phases(Int.phases)
                } : Ra.raClient

    (* How to allocate floating point registers:    
     * Allocate all fp registers on the stack.  This is the easy way.
     *)
    fun RAFP32 S =
                {spill     = spillF S,
                 spillSrc  = spillFreg S,
                 spillCopyTmp= spillFcopyTmp S,
                 reload    = reloadF S,
                 reloadDst = reloadFreg S,
                 renameSrc = renameF,
                 copyInstr = copyInstrF,
                 K         = KF32,
                 getreg    = FR32.getreg,
                 cellkind  = CB.FP,   
                 dedicated = isDedicatedF32,
                 spillProh = [],
                 memRegs   = Float.memRegs,
                 mode      = phases(Float.phases)
                } : Ra.raClient

    (* How to allocate floating point registers:    
     * Allocate fp registers on the %st stack.  Also perform
     * memory allcoation.
     *)
     fun RAFP8 S =
                {spill     = spillF S,
                 spillSrc  = spillFreg' S,
                 spillCopyTmp= spillFcopyTmp S,
                 reload    = reloadF S,
                 reloadDst = reloadFreg' S,
                 renameSrc = renameF',
                 copyInstr = copyInstrF',
                 K         = KF8,
                 getreg    = FR8.getreg,
                 cellkind  = CB.FP,   
                 dedicated = isDedicatedF8,
                 spillProh = [],
                 memRegs   = Float.fastMemRegs,
                 mode      = phases(Float.fastPhases) 
                } : Ra.raClient

    (* Two RA modes, fast and normal *) 
    fun fast_fp S = [RAInt S, RAFP8 S]
    fun normal_fp S = [RAInt S, RAFP32 S]
 
    (* The main ra routine *)
    fun run cluster =
    let val printGraph = 
            if !x86CfgDebugFlg then 
               PrintFlowgraph.printCFG(!MLRiscControl.debug_stream)
            else fn msg => fn _ => () 

        val S = beforeRA cluster 
        val _ = resetRA()

        (* generic register allocator *)

        val cluster = Ra.ra
                      (if !fast_floating_point then fast_fp S else normal_fp S)
                      cluster

        val _ = removeDeadCode cluster

        val _ = printGraph "\t---After register allocation K=8---\n" cluster

        (* Run the FP translation phase when fast floating point has
         * been enabled
         *)
        val cluster = 
             if !fast_floating_point andalso I.C.numCell CB.FP () > 0 then 
             let val cluster = X86FP.run cluster
             in  printGraph "\t---After X86 FP translation ---\n" cluster;
                 cluster
             end
             else cluster
    in  cluster
    end

end
