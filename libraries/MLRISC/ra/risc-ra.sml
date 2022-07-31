(* 
 * This functor factors out the machine independent part of the register
 * allocator.  It performs integer and floating register allocation.
 * This works well for RISC machines; but not applicable to x86.
 *)
functor RISC_RA
  (structure I         : INSTRUCTIONS
   structure Asm       : INSTRUCTION_EMITTER
   			where I = I 
   structure CFG       : CONTROL_FLOW_GRAPH 
   			where I = I
		          and P = Asm.S.P
   structure InsnProps : INSN_PROPERTIES
   			where I = I
   structure Rewrite   : REWRITE_INSTRUCTIONS
   			where I = I
   structure SpillInstr : ARCH_SPILL_INSTR
                        where I = I

      (* Spilling heuristics determines which node should be spilled.
       * You can use Chaitin, ChowHenessey, or one of your own.
       *)
   structure SpillHeur : RA_SPILL_HEURISTICS

      (* The Spill module figures out the strategies for inserting
       * spill code.  You can use RASpill, or RASpillWithRenaming,
       * or write your own if you are feeling adventurous.
       *)
   structure Spill : RA_SPILL where I = I
          
   val architecture : string

   (* Is this a pure instruction *)
   val pure : I.instruction -> bool


   datatype spillOperandKind = SPILL_LOC | CONST_VAL
   type spill_info (* user-defined abstract type *)

   (* Called before RA begins *)
   val beforeRA : CFG.cfg -> spill_info

   structure Int :
   sig

      val avail     : CellsBasis.cell list (* list of available registers *)
      val dedicated : CellsBasis.cell list (* list of registers that are dedicated *)

      val spillLoc  : 
	  { info:spill_info,
            an  :Annotations.annotations ref,
            cell:CellsBasis.cell, (* spilled cell *)
            id  :RAGraph.logical_spill_id
	   } 
              -> { opnd: I.ea,
                    kind: spillOperandKind
	 	  }

      (* Mode for RA optimizations *)
      val mode : RAGraph.mode
   end

   structure Float :
   sig

      val avail     : CellsBasis.cell list (* list of available registers *)
      val dedicated : CellsBasis.cell list (* list of registers that are dedicated *)

      val spillLoc  : 
	 spill_info * Annotations.annotations ref * RAGraph.logical_spill_id 
	     -> I.ea

      (* Mode for RA optimizations *)
      val mode : RAGraph.mode
   end
  ) : CFG_OPTIMIZATION =
struct
   structure CFG = CFG
   structure I   = CFG.I
   structure P   = InsnProps
   structure C   = I.C
   structure G   = RAGraph
   structure CB  = CellsBasis

   (* The generic register allocator *)
   structure Ra =
      RegisterAllocator
        (SpillHeur) 
        (* (ChowHennessySpillHeur) *)
        (ClusterRA 
          (structure Flowgraph = CFG
           structure Asm = Asm
           structure InsnProps = InsnProps
           structure Spill = Spill
          )
        )

   val name = "RISC_RA"

   (* Counters for register allocation *)
   val intSpillsCnt = MLRiscControl.mkCounter ("ra-int-spills", "RA int spill count")
   val intReloadsCnt = MLRiscControl.mkCounter ("ra-int-reloads", "RA int reload count")
   val intRenamesCnt = MLRiscControl.mkCounter ("ra-int-renames", "RA int rename count")
   val floatSpillsCnt = MLRiscControl.mkCounter ("ra-float-spills", "RA float spill count")
   val floatReloadsCnt = MLRiscControl.mkCounter ("ra-float-reloads", "RA float reload count")
   val floatRenamesCnt = MLRiscControl.mkCounter ("ra-float-renames", "RA float rename count")

   fun inc c = c := !c + 1

   fun error msg = MLRiscErrorMsg.error("RISC RA "^architecture,msg)

   (*
    * Make arithmetic non-overflow trapping.
    * This makes sure that if we happen to run the compiler for a long
    * period of time overflowing counters will not crash the compiler. 
    *)
   fun x + y = Word.toIntX(Word.+(Word.fromInt x, Word.fromInt y))
   fun x - y = Word.toIntX(Word.-(Word.fromInt x, Word.fromInt y))

   (* GetReg specialized to integer and floating point registers *)
   fun isDedicated (len, arr, others) r = 
     (r < len andalso Array.sub(arr, r)) orelse List.exists (fn d => r = d) others

   fun mark(arr, _, [], others) = others
     | mark(arr, len, r::rs, others) = let
	 val r = CellsBasis.registerId r
       in
	 if r >= len then mark(arr, len, rs, r::others)
	 else (Array.update(arr, r, true); mark(arr, len, rs, others))
       end

   fun annotate([], i) = i
     | annotate(a::an, i) = annotate(an, I.ANNOTATION{a=a, i=i})

   local
       val {low,high} = C.cellRange CellsBasis.GP
       val arr = Array.array(high+1,false)
       val others = mark(arr, high+1, Int.dedicated, [])
   in
       structure GR = GetReg(val first=low val nRegs=high-low+1 
                             val available=map CellsBasis.registerId Int.avail)
       val dedicatedR : int -> bool = isDedicated (high+1, arr, others)
   end

   fun getRegLoc(S, an, cell, Ra.FRAME loc) = 
         Int.spillLoc{info=S, an=an, cell=cell, id=loc}
     | getRegLoc _ = error "getRegLoc"

   fun copy((rds as [d], rss as [s]), I.COPY{sz, ...}) = 
       if CB.sameColor(d,s) then [] 
       else [I.COPY{k=CB.GP, sz=sz, dst=rds, src=rss, tmp=NONE}]
     | copy((rds, rss), I.COPY{tmp, sz, ...}) = 
        [I.COPY{k=CB.GP, sz=sz, dst=rds, src=rss, tmp=tmp}]
     | copy _ = error "copy: COPY?"

   fun spillR S {annotations, kill=true, reg, spillLoc, instr} = 
         if pure instr then {code=[], proh=[], newReg=NONE}
         else spillR S {annotations=annotations,kill=false,
                     spillLoc=spillLoc,
                     reg=reg,instr=instr}
     | spillR S {annotations=an, kill, reg, spillLoc, instr} = let
	 fun annotate([], i) = i
	   | annotate(a::an, i) = annotate(an, I.ANNOTATION{a=a, i=i})

	 (* preserve annotation on instruction *)
	 fun spill(instrAn, I.ANNOTATION{a, i}) = spill(a::instrAn, i)
	   | spill(instrAn, I.KILL{regs, spilled}) = 
	      {code=
	         [annotate
		   (instrAn, 
		    I.KILL {regs=C.rmvReg(reg, regs), 
			    spilled=C.addReg(reg, spilled)})],
	        proh = [], 
		newReg=NONE}
	   | spill(instrAn, I.LIVE _) = error "spillR: LIVE"
	   | spill(_, I.COPY _) = error "spillR: COPY"
	   | spill(instrAn, I.INSTR _) = let
	       val {opnd=spillLoc:I.ea, kind} = getRegLoc (S, an, reg, spillLoc)
             in
	        inc intSpillsCnt;
		SpillInstr.spill CB.GP (instr, reg, spillLoc)
             end
       in spill([], instr)
       end 

   (* spill src at the spill location for reg i.e. spillLoc *)
   fun spillReg S {annotations=an,src,reg,spillLoc} =
       (inc intSpillsCnt;
	#code(SpillInstr.spillToEA CB.GP (src, #opnd(getRegLoc(S, an, reg, spillLoc)))))


   (* Spill the temporary associated with a copy *)
   fun spillTmp S {annotations=an, reg, copy=I.COPY{k=CB.GP, sz, tmp, dst, src}, spillLoc} = let
          val loc = #opnd(getRegLoc(S, an, reg, spillLoc))
       in
         inc intSpillsCnt;
	 I.COPY{k=CB.GP, sz=sz, tmp=SOME loc, dst=dst, src=src}
       end
     | spillTmp _ _ = error "spillTmp"

   (* Rename integer register *)
   fun renameR{fromSrc,toSrc,instr} = 
       let val _   = inc intRenamesCnt
           val instr' = Rewrite.rewriteUse(instr, fromSrc, toSrc)
       in {code=[instr'], proh=[], newReg=SOME toSrc}
       end

   (* Reload integer register *)
   fun reloadR S {annotations=an, reg, spillLoc, instr} = let
     fun reload(instrAn, I.ANNOTATION{a, i}) = reload(a::instrAn, i)
       | reload(instrAn, I.LIVE{regs, spilled}) = 
	  {code=[I.LIVE{regs=C.rmvReg(reg, regs), spilled=C.addReg(reg, spilled)}],
	   proh=[],
	   newReg=NONE}
       | reload(_, I.KILL _) = error "reloadR: KILL"
       | reload (_, I.COPY _) = error "reloadR: COPY"
       | reload(instrAn, instr as I.INSTR _) = let
	   val spillLoc = #opnd (getRegLoc(S, an, reg, spillLoc))
         in
	   inc intReloadsCnt;
	   SpillInstr.reload CB.GP (instr, reg, spillLoc)
         end
   in reload([], instr)
   end

   (* reload the register dst from the spill location for reg, i.e. spillLoc  *)
   fun reloadReg S {annotations=an,reg,dst,spillLoc} = 
       (inc intReloadsCnt;
	#code(SpillInstr.reloadFromEA CB.GP (dst, #opnd(getRegLoc(S, an, reg, spillLoc)))))


  (*-------------------------------------------------------------*)
   local 
      val {low,high} = C.cellRange CellsBasis.FP
      val arr = Array.array(high+1,false)
      val others = mark(arr, high+1, Float.dedicated, [])
   in
      structure FR = GetReg(val first=low val nRegs=high-low+1 
                            val available=map CellsBasis.registerId Float.avail)
      val dedicatedF : int -> bool = isDedicated(high+1, arr, others)
   end

   fun getFregLoc(S, an, Ra.FRAME loc) = Float.spillLoc(S, an, loc)
     | getFregLoc _ = error "getFregLoc"

   fun fcopy((rds as [d], rss as [s]), I.COPY{sz, ...}) = 
       if CB.sameColor(d,s) then [] 
       else [I.COPY{k=CB.FP, sz=sz, dst=rds, src=rss, tmp=NONE}]
     | fcopy((rds, rss), I.COPY{tmp, sz, ...}) = 
        [I.COPY{k=CB.FP, sz=sz, dst=rds, src=rss, tmp=tmp}]
     | fcopy _ = error "fcopy: COPY?"

   (* Spill floating point register *)
   fun spillF S {annotations, kill=true, reg, spillLoc, instr} = 
         if pure instr then {code=[], proh=[], newReg=NONE}
         else spillF S {annotations=annotations,kill=false,
			spillLoc=spillLoc, reg=reg,instr=instr}
     | spillF S {annotations=an, kill, reg, spillLoc, instr} = let
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
	       (inc floatSpillsCnt;
		SpillInstr.spill CB.FP (instr, reg, getFregLoc(S, an, spillLoc)))
        in spill([], instr)
        end

   (* spill src at the spill location  for reg, i.e. spillLoc *)
   fun spillFreg S {annotations=an,reg,src,spillLoc} = 
       (inc floatSpillsCnt;
	#code(SpillInstr.spillToEA CB.FP (src, getFregLoc(S, an, spillLoc))))

   (* Spill the temporary associated with a copy *)
   fun spillFtmp S {annotations=an, reg, copy=I.COPY{k=CB.FP, sz, tmp, dst, src}, spillLoc} = let
          val loc = getFregLoc(S, an, spillLoc)
       in
         inc floatSpillsCnt;
	 I.COPY{k=CB.FP, sz=sz, tmp=SOME loc, dst=dst, src=src}
       end
     | spillFtmp _ _ = error "spillFtmp"

                   
   (* Rename floating point register *)
   fun renameF{fromSrc,toSrc,instr} =
       let val _ = inc floatRenamesCnt
           val instr' = Rewrite.frewriteUse(instr, fromSrc, toSrc)
       in  {code=[instr'], proh=[], newReg=SOME toSrc}
       end

   (* Reload floating point register *)
   fun reloadF S {annotations=an, reg, spillLoc, instr} = let
     fun reload(instrAn, I.ANNOTATION{a,i}) = reload(a::instrAn, i)
       | reload(instrAn, I.LIVE{regs, spilled}) = 
	  {code=[I.LIVE{regs=C.rmvFreg(reg, regs), spilled=C.addFreg(reg, spilled)}],
	   proh=[],
	   newReg=NONE}
       | reload(_, I.KILL _) = error "reloadF: KILL"
       | reload (_, I.COPY _) = error "reloadF: COPY"
       | reload(instrAn, instr as I.INSTR _) = 
	   (inc floatReloadsCnt;
	    SpillInstr.reload CB.FP (instr, reg, getFregLoc(S, an, spillLoc)))
   in reload([], instr)
   end

   (* reload register dst from the spill location for reg, i.e. spillLoc *)
   fun reloadFreg S {annotations=an,reg,dst,spillLoc} =
       (inc floatReloadsCnt;
	#code (SpillInstr.reloadFromEA CB.FP (dst, getFregLoc(S, an, spillLoc))))

   val KR = length Int.avail
   val KF = length Float.avail

   fun params S =
       [  { cellkind     = CellsBasis.GP,
            getreg       = GR.getreg,
            spill        = spillR S,
            spillSrc     = spillReg S,
            spillCopyTmp = spillTmp S,
            reload       = reloadR S,
            reloadDst    = reloadReg S,
            renameSrc    = renameR,
            K            = KR,
            dedicated    = dedicatedR,
            copyInstr    = copy,
            spillProh    = [],
            memRegs      = [],
            mode         = Int.mode
          } : Ra.raClient,
          { cellkind     = CellsBasis.FP,
            getreg       = FR.getreg,
            spill        = spillF S,
            spillSrc     = spillFreg S,
            spillCopyTmp = spillFtmp S,
            reload       = reloadF S,
            reloadDst    = reloadFreg S,
            renameSrc    = renameF,
            K            = KF,
            dedicated    = dedicatedF,
            copyInstr    = fcopy,
            spillProh    = [],
            memRegs      = [],
            mode         = Float.mode
          } : Ra.raClient
       ] : Ra.raClient list
  
   fun run cluster = let
       val S = beforeRA cluster
   in
       GR.reset();
       FR.reset();
       Ra.ra (params S) cluster
   end

end

