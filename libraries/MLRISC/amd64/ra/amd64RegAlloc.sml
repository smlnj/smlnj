(* amd64RegAlloc.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Ties together the GP and FP register allocators.
 *)

functor AMD64RegAlloc (
      structure I : AMD64INSTR
      structure SpillHeur : RA_SPILL_HEURISTICS
      structure Props : AMD64INSN_PROPERTIES
          where I = I
      structure CFG : CONTROL_FLOW_GRAPH
          where I = I
      structure Spill : RA_SPILL
          where I = I
      structure Asm : INSTRUCTION_EMITTER
          where I = I
          and   S.P = CFG.P

      type spill_info
      datatype spill_operand_kind = SPILL_LOC
                                  | CONST_VAL

      val beforeRA : CFG.cfg -> spill_info

      datatype ra_phase = SPILL_PROPAGATION
                        | SPILL_COLORING

      structure Int : sig
          val avail : CellsBasis.cell list
          val dedicated : CellsBasis.cell list
          val spillLoc  : {info:spill_info,
                           an  :Annotations.annotations ref,
                           cell:CellsBasis.cell, (* spilled cell *)
                           id  :RAGraph.logical_spill_id
                          } ->
                          {opnd: I.ea,
                           kind: spill_operand_kind
                          }
          val spillInit : RAGraph.interferenceGraph -> unit
          val phases    : ra_phase list
      end

      structure Float : sig
          val avail : CellsBasis.cell list
          val dedicated : CellsBasis.cell list
          val spillLoc  : spill_info * Annotations.annotations ref *
                        RAGraph.logical_spill_id -> I.ea
          val spillInit : RAGraph.interferenceGraph -> unit
          val phases    : ra_phase list
      end

     (* guaranteeing that floats are stored at 16-byte aligned addresses reduces the number of instructions *)
      val floats16ByteAligned : bool

    ) : CFG_OPTIMIZATION =
  struct

    datatype ra_phase = datatype ra_phase

    fun error msg = MLRiscErrorMsg.error ("AMD64RA", msg)
    fun inc c = c := !c + 1

    val intSpillCnt =
        MLRiscControl.mkCounter ("ra-int-spills", "RA int spill count")
    val intReloadCnt =
        MLRiscControl.mkCounter ("ra-int-reloads", "RA int reload count")
    val intRenameCnt =
        MLRiscControl.mkCounter ("ra-int-renames", "RA int rename count")
    val floatSpillCnt =
        MLRiscControl.mkCounter ("ra-float-spills", "RA float spill count")
    val floatReloadCnt =
        MLRiscControl.mkCounter ("ra-float-reloads", "RA float reload count")
    val floatRenameCnt =
        MLRiscControl.mkCounter ("ra-float-renames", "RA float rename count")

    structure CB = CellsBasis
    structure C = I.C

    val firstSpill = ref true
    val firstFPSpill = ref true

    fun spillInit (graph, CB.GP) =
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

    (*
     * Dead code elimination
     *)
    exception AMD64DeadCode
    val affectedBlocks =
	  IntHashTable.mkTable(32, AMD64DeadCode) : bool IntHashTable.hash_table
    val deadRegs       =
	  IntHashTable.mkTable(32, AMD64DeadCode) : bool IntHashTable.hash_table

    fun removeDeadCode(cfg as Graph.GRAPH graph) = let
        val blocks = #nodes graph ()
        fun isDead r = Option.isSome (IntHashTable.find deadRegs (CB.cellId r))
        fun isAffected i = Option.getOpt (IntHashTable.find affectedBlocks i, false)
        fun isDeadInstr(I.ANNOTATION{i, ...}) = isDeadInstr i
          | isDeadInstr(I.INSTR(I.MOVE{dst=I.Direct (_,rd), ...})) = isDead rd
          | isDeadInstr(I.INSTR(I.FMOVE{dst=I.FDirect rd, ...})) = isDead rd
          | isDeadInstr(I.COPY{k=CB.GP, dst=[rd], ...}) = isDead rd
          | isDeadInstr(I.COPY{k=CB.FP, dst=[rd], ...}) = isDead rd
          | isDeadInstr _ = false
        fun scan [] = ()
          | scan((blknum, CFG.BLOCK{insns, ...})::rest) =
            (if isAffected blknum then
                ((* deadblocks := !deadblocks + 1; *)
                 insns := elim(!insns, [])
                ) else ();
             scan rest)
       and elim([], code) = List.rev code
         | elim(i::instrs, code) =
          if isDeadInstr i then
             ((* deadcode := !deadcode + 1; *) elim(instrs, code))
          else elim(instrs, i::code)
    in
      if (IntHashTable.numItems affectedBlocks > 0)
        then (
          scan blocks;
	  IntHashTable.clear deadRegs;
	  IntHashTable.clear affectedBlocks)
        else ()
    end

    structure CFG = CFG

    (* use the standard register allocator *)
    structure RA =
        RegisterAllocator
          (SpillHeur)
          (MemoryRA
             (RADeadCodeElim
                (ClusterRA
                   (structure Flowgraph = CFG
                    structure Asm = Asm
                    structure InsnProps = Props
                    structure Spill = Spill
                   )
                 )
                (fun cellkind CB.GP = true
		   | cellkind CB.FP = true
		   | cellkind _ = false
                 val deadRegs = deadRegs
                 val affectedBlocks = affectedBlocks
                 val spillInit = spillInit
                )
              )
           )
    structure PrintFlowgraph =
       PrintFlowgraph (structure CFG = CFG
                       structure Asm = Asm)
    structure SpillInstr = AMD64SpillInstr (
               structure I = I
               structure Props = Props
	       val floats16ByteAligned = true)
    val spillFInstr = SpillInstr.spill CB.FP
    val reloadFInstr = SpillInstr.reload CB.FP
    val spillInstr = SpillInstr.spill CB.GP
    val reloadInstr = SpillInstr.reload CB.GP

    val name = "AMD64RegAlloc"
    val amd64CfgDebugFlg = MLRiscControl.mkFlag ("amd64-cfg-debug", "amd64 CFG debug mode")

    val nGPRegs = List.length Int.avail + List.length Int.dedicated
    val nFPRegs = List.length Float.avail + List.length Float.dedicated

    structure GPR = GetReg
        (val nRegs = nGPRegs
         val available = List.map CB.registerId Int.avail
         val first = CB.registerId (I.C.GPReg 0))
    structure FPR = GetReg
        (val nRegs = nFPRegs
         val available = List.map CB.registerId Float.avail
         val first = CB.registerId (I.C.FPReg 0))

(*
    local
      val dedicatedR = Array.array (nGPRegs, false)
      val dedicatedF = Array.array (nFPRegs, false)
      fun set (dedicated, []) = ()
        | set (dedicated, r :: rs) = (
          Array.update (dedicated, r, true);
          set (dedicated, rs))
      val _ = set (dedicatedR, List.map CB.registerId Int.dedicated)
      val _ = set (dedicatedF, List.map CB.registerId Float.dedicated)
      fun isDedicated dedicated r =
          r < Array.length dedicated andalso Array.sub (dedicated, r)
    in
      val isDedicatedR = isDedicated dedicatedR
      val isDedicatedF = isDedicated dedicatedF
    end (* local *)
*)
    local
	val dedicatedR = Vector.fromList (List.map CB.registerId Int.dedicated)
	val dedicatedF = Vector.fromList (List.map CB.registerId Float.dedicated)
    in
	fun isDedicatedR r = Vector.exists (fn q => q = r) dedicatedR
	fun isDedicatedF r = Vector.exists (fn q => q = r) dedicatedF
    end

    fun copy {dst, src, tmp} = I.COPY {k=CB.GP, sz=64, dst=dst, src=src, tmp=tmp}
    fun fcopy{dst, src, tmp} = I.COPY{k=CB.FP, sz=64, dst=dst, src=src, tmp=tmp}
    fun annotate ([], i) = i
      | annotate (a :: an, i) = annotate (an, I.ANNOTATION {a=a, i=i})

    fun resetRA() = (
        firstSpill := true;
        firstFPSpill := true;
        IntHashTable.clear affectedBlocks;
        IntHashTable.clear deadRegs;
        GPR.reset ();
        FPR.reset ())

    fun getRegLoc(s, an, cell, RA.FRAME loc) =
        Int.spillLoc {info=s, an=an, cell=cell, id=loc}
      | getRegLoc(s, an, cell, RA.MEM_REG r) =
        error "memory registers unsupported"

    fun spillR s {annotations=an, kill, reg, spillLoc, instr} = let
       fun annotate([], i) = i
  	  | annotate(a::an, i) = annotate(an, I.ANNOTATION{a=a, i=i})
        (* preserve annotation on instruction *)
        fun spill(instrAn, I.ANNOTATION{a,i}) = spill(a::instrAn, i)
          | spill(instrAn, I.KILL{regs, spilled}) =
	    {code=[annotate (instrAn,
		     I.KILL {regs=C.rmvReg (reg, regs),
			     spilled=C.addReg (reg, spilled)})],
	         proh = [], newReg=NONE}
	    | spill(instrAn, I.LIVE _) = error "spill: LIVE"
	    | spill(_, I.COPY _) = error "spill: COPY"
	    | spill(instrAn, I.INSTR _) = (case getRegLoc (s, an, reg, spillLoc)
	      of {opnd=spillLoc, kind=SPILL_LOC} =>
	            (inc intSpillCnt;
		     spillInstr (annotate(instrAn, instr), reg, spillLoc))
	       | _ => (* don't have to spill a constant *)
	              {code=[], newReg=NONE, proh=[]}
	      (* end case *))
        in
          spill([], instr)
        end (* spillR *)

    fun spillReg s {src, reg, spillLoc, annotations=an} = let
        val _ = inc intSpillCnt
        val {opnd=dstLoc, kind} = getRegLoc (s, an, reg, spillLoc)
        val srcLoc = I.Direct (64, src)
        in
          if kind = CONST_VAL orelse Props.eqOpn (srcLoc, dstLoc)
            then []
            else [I.move {mvOp=I.MOVQ, src=srcLoc, dst=dstLoc}]
        end (* spillReg *)

    fun spillCopyTmp s {copy=I.COPY{k=CB.GP, src, dst,...},
                        reg, spillLoc, annotations=an} =
        (case getRegLoc (s, an, reg, spillLoc)
          of {opnd=tmp, kind=SPILL_LOC} => (
             inc intSpillCnt;
             copy{dst=dst, src=src, tmp=SOME tmp})
           | _ => error "spillCopyTmp"
        (* end case *))
      | spillCopyTmp s {copy=I.ANNOTATION{i, a}, reg, spillLoc, annotations} =
        I.ANNOTATION{i=spillCopyTmp s {copy=i, reg=reg, spillLoc=spillLoc,
                                       annotations=annotations}, a=a}
      | spillCopyTmp _ _ = error "spillCopyTmp"

    fun reloadR s {annotations=an, reg, spillLoc, instr} = let
        fun reload (instrAn, I.ANNOTATION{a,i}) = reload (a::instrAn, i)
	  | reload(instrAn, I.LIVE{regs, spilled}) =
	    {code=[I.LIVE{regs=C.rmvReg(reg, regs),
	     spilled=C.addReg(reg, spilled)}],
	    proh=[], newReg=NONE}
	  | reload(_, I.KILL _) = error "reload: KILL"
	  | reload (_, I.COPY _) = error "reload: COPY"
	  | reload(instrAn, instr as I.INSTR _)  = (
  	    inc intReloadCnt;
	    reloadInstr (annotate(instrAn, instr), reg,
	           #opnd(getRegLoc(s,an,reg,spillLoc))))
        in
          reload([], instr)
        end (* reloadR *)

    fun reloadReg s {dst, reg, spillLoc, annotations=an} = let
        val _ = inc intReloadCnt
        val srcLoc = #opnd (getRegLoc (s, an, reg, spillLoc))
        val dstLoc = I.Direct (64, dst)
        in
          if Props.eqOpn(srcLoc,dstLoc)
            then []
            else [I.move{mvOp=I.MOVQ, src=srcLoc, dst=dstLoc}]
        end (* reloadReg *)

    fun renameR {instr, fromSrc, toSrc} = (
        inc intRenameCnt;
        reloadInstr (instr, fromSrc, I.Direct (64, toSrc)))

    fun copyInstrR ((rds as [d], rss as [s]), _) =
        if CB.sameColor(d,s) then [] else [copy {dst=rds, src=rss, tmp=NONE}]
      | copyInstrR((rds, rss), I.COPY{k=CB.GP, tmp, ...}) =
         [copy{dst=rds, src=rss, tmp=tmp}]
      | copyInstrR(x, I.ANNOTATION{i, a}) =
          copyInstrR (x, i) (* XXX *)
      | copyInstrR _ = error "copyInstrR"

    fun phases ps = let
        fun f ([], m) = m
          | f (SPILL_PROPAGATION::ps, m) = f (ps, RA.SPILL_PROPAGATION+m)
          | f (SPILL_COLORING::ps, m) = f (ps, RA.SPILL_COLORING+m)
        in
          f (ps, RA.NO_OPTIMIZATION)
        end

    fun raInt s = {
	spill = spillR s,
        spillSrc  = spillReg s,
        spillCopyTmp= spillCopyTmp s,
        reload    = reloadR s,
        reloadDst = reloadReg s,
        renameSrc = renameR,
        copyInstr = copyInstrR,
        K         = List.length Int.avail,
        getreg    = GPR.getreg,
        cellkind  = CB.GP,
        dedicated = isDedicatedR,
        spillProh = [],
        memRegs   = [],
        mode      = phases Int.phases
	} : RA.raClient

    fun getFregLoc (s, an, RA.FRAME loc) = Float.spillLoc (s, an, loc)
      | getFregLoc (s, an, RA.MEM_REG r) = raise Fail "mem regs unsupported"

    fun spillF s {annotations=an, kill, reg, spillLoc, instr} = let
        (* preserve annotation on instruction *)
        fun spill(instrAn, I.ANNOTATION{a, i}) = spill(a::instrAn, i)
  	  | spill(instrAn, I.KILL{regs, spilled}) =
	    {code=[annotate (instrAn,
		 I.KILL {regs=C.rmvFreg(reg, regs),
			 spilled=C.addFreg(reg, spilled)})],
	     proh = [], newReg=NONE}
	  | spill(instrAn, I.LIVE _) = error "spillF: LIVE"
	  | spill(_, I.COPY _) = error "spillF: COPY"
	  | spill(instrAn, I.INSTR _) = (
	    inc floatSpillCnt;
	    spillFInstr (instr, reg, getFregLoc (s, an, spillLoc)))
        in
          spill([], instr)
        end (* spillF *)

    fun spillFreg s {src, reg, spillLoc, annotations=an} = (
        inc floatSpillCnt;
        let val dst = getFregLoc (s, an, spillLoc)
        in
          if Props.eqOpn (I.Direct (64, src), dst)
	    then []
	    else [I.fmove {fmvOp=I.MOVSD, src=I.FDirect src, dst=dst}]
	end)

    fun spillFcopyTmp s {copy=I.COPY{k=CB.FP, dst, src, ...}, spillLoc, reg,
                         annotations=an} = (
        inc floatSpillCnt;
        fcopy {dst=dst, src=src, tmp=SOME (getFregLoc (s, an, spillLoc))})
      | spillFcopyTmp s {copy=I.ANNOTATION{i,a}, spillLoc, reg, annotations} =
        let val i = spillFcopyTmp s {copy=i, spillLoc=spillLoc, reg=reg,
                                     annotations=annotations}
        in  I.ANNOTATION{i=i, a=a} end
      | spillFcopyTmp _ _ = error "spillFcopyTmp"

    fun reloadF s {annotations=an,reg,spillLoc,instr} = let
        fun reload (instrAn, I.ANNOTATION{a,i}) = reload(a::instrAn, i)
	  | reload(instrAn, I.LIVE{regs, spilled}) =
	    {code=[I.LIVE{regs=C.rmvFreg(reg, regs),
	           spilled=C.addFreg(reg, spilled)}],
	     proh=[], newReg=NONE}
	  | reload(_, I.KILL _) = error "reloadF: KILL"
	  | reload (_, I.COPY _) = error "reloadF: COPY"
	  | reload(instrAn, instr as I.INSTR _) = (
  	    inc floatReloadCnt;
	    reloadFInstr (instr, reg, getFregLoc(s, an, spillLoc)))
        in
          reload([], instr)
        end (* reloadF *)

    fun reloadFreg s {dst, reg, spillLoc, annotations=an} =
        (inc floatReloadCnt;
	 let val srcLoc = getFregLoc (s, an, spillLoc)
	     val dstLoc = I.FDirect dst
	 in
	   if Props.eqOpn (srcLoc, dstLoc)
	     then []
	     else [I.fmove {fmvOp=I.MOVSD, src=srcLoc, dst=dstLoc}]
	 end)

    fun copyInstrF((rds as [_], rss as [_]), _) =
        fcopy{dst=rds, src=rss, tmp=NONE}
      | copyInstrF((rds, rss), I.COPY{k=CB.FP, tmp, ...}) =
        fcopy{dst=rds, src=rss, tmp=tmp}
      | copyInstrF(x, I.ANNOTATION{i,a}) =
        I.ANNOTATION{i=copyInstrF (x, i), a=a}
      | copyInstrF _ = error "copyInstrF"

    val copyInstrF = fn x => [copyInstrF x]

    fun renameF {instr, fromSrc, toSrc} = (
        inc floatRenameCnt;
        reloadFInstr (instr, fromSrc, I.FDirect toSrc))

    fun raFloat s = {
        spill     = spillF s,
        spillSrc  = spillFreg s,
        spillCopyTmp= spillFcopyTmp s,
        reload    = reloadF s,
        reloadDst = reloadFreg s,
        renameSrc = renameF,
        copyInstr = copyInstrF,
        K         = List.length Float.avail,
        getreg    = FPR.getreg,
        cellkind  = CB.FP,
        dedicated = isDedicatedF,
        spillProh = [],
        memRegs   = [],
        mode      = phases Float.phases
        } : RA.raClient

    fun run cfg = let
        val printCFG = if !amd64CfgDebugFlg
              then PrintFlowgraph.printCFG (!MLRiscControl.debug_stream)
              else fn msg => fn _ => ()
	val _ = printCFG "\t---Before register allocation---\n" cfg;
	val s = beforeRA cfg
	val _ = resetRA ()
	val cfg' = RA.ra [raInt s, raFloat s] cfg
        in
          removeDeadCode cfg';
          printCFG "\t---After register allocation---\n" cfg';
          cfg'
        end (* run *)

  end (* AMD64RegAlloc *)
