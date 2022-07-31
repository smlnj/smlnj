(*
 * This module manages the spill/reload process. 
 * The reason this is detached from the main module is that 
 * I can't understand the old code. 
 *
 * Okay, now I understand the code.
 *
 * The new code does things slightly differently.
 * Here, we are given an instruction and a list of registers to spill
 * and reload.  We rewrite the instruction until all instances of these
 * registers are rewritten.
 *
 * (12/13/99) Some major caveats when spill coalescing/coloring is used:
 * When parallel copies are generated and spill coalescing/coloring is used,
 * two special cases have to be identified:
 *
 * Case 1 (spillLoc dst = spillLoc src)
 *        Suppose we have a parallel copy
 *             (u,v) <- (x,y)
 *        where u has to be spilled and y has to reloaded.  When both
 *        u and y are mapped to location M.  The following wrong code may
 *        be generated:
 *                M <- x  (spill u)
 *                v <- M  (reload y)
 *        This is incorrect.  Instead, we generate a dummy copy and
 *        delay the spill after the reload, like this:  
 *               
 *               tmp <- x (save value of u)
 *               v <- M   (reload y)
 *               M <- tmp (spill u)
 * Case 2 (spillLoc copyTmp = spillLoc src)
 *        Another case that can cause problems is when the spill location of
 *        the copy temporary is the same as that of one of the sources:
 *
 *              (a, b, v) <- (b, a, u)  where spillLoc(u) = spillLoc(tmp) = v
 *
 *        The incorrect code is
 *              (a, b) <- (b, a) 
 *              v <- M
 *        But then the shuffle code for the copy can clobber the location M.
 *
 *              tmp <- M
 *              (a, b) <- (b, a) 
 *              v <- tmp
 *
 *       (Note that spillLoc copyTmp = spillLoc src can never happen) 
 * 
 * -- Allen
 *)

local

   val debug = false

in

functor RASpill
   (structure InsnProps : INSN_PROPERTIES
    structure Asm       : INSTRUCTION_EMITTER
    			where I = InsnProps.I
   ) : RA_SPILL =
struct

   structure I      = InsnProps.I
   structure P      = InsnProps
   structure C      = I.C
   structure Core   = RACore
   structure G      = Core.G
   structure CB     = CellsBasis

   fun error msg = MLRiscErrorMsg.error("RASpill",msg)

   val keep_dead_copies = 
       MLRiscControl.mkFlag 
          ("ra-preserve-dead-copies",
	   "Dead copies are not removed when spilling")

   fun dec1 n = Word.toIntX(Word.fromInt n - 0w1)
   fun dec{block,insn} = {block=block,insn=dec1 insn}

   structure T = RASpillTypes(I)
   open T

   fun uniq s = CB.SortedCells.return(CB.SortedCells.uniq s) 
   val i2s    = Int.toString
   fun pt2s{block,insn} = "b"^i2s block^":"^i2s insn

   val Asm.S.STREAM{emit, ...} = Asm.makeStream[]

   (* val spilledCopyTmps = MLRiscControl.getCounter "ra-spilled-copy-temps" *)

   (*
    * The following function performs spilling.
    *)
   fun spillRewrite
        {graph=G as G.GRAPH{showReg, spilledRegs, nodes, mode, ...},
         spill : spill, 
         spillCopyTmp : spillCopyTmp, 
         spillSrc : spillSrc, 
         renameSrc : renameSrc,
         reload : reload, 
         reloadDst : reloadDst, 
         copyInstr : copyInstr, 
         cellkind,
         spillSet, reloadSet, killSet
        } =
   let 
       (* Must do this to make sure the interference graph is 
        * reflected to the cells
        *)
       val _ = Core.updateCellAliases G

       val getSpillLoc = Core.spillLoc G
       fun spillLocOf(CB.CELL{id, ...}) = getSpillLoc id
       val spillLocsOf = map spillLocOf
       val getnode = IntHashTable.lookup nodes
       val getnode = fn CB.CELL{id, ...} => getnode id

       val insnDefUse = P.defUse cellkind

       (* Merge prohibited registers *)
       val enterSpill = IntHashTable.insert spilledRegs
       val addProh = app (fn c => enterSpill(CellsBasis.registerId c,true)) 

       val getSpills  = G.PPtHashTable.find spillSet
       val getSpills  = fn p => case getSpills p of SOME s => s | NONE => []
       val getReloads = G.PPtHashTable.find reloadSet
       val getReloads = fn p => case getReloads p of SOME s => s | NONE => []
       val getKills   = G.PPtHashTable.find killSet
       val getKills   = fn p => case getKills p of SOME s => s | NONE => []

       fun getLoc(G.NODE{color=ref(G.ALIASED n), ...}) = getLoc n
         | getLoc(G.NODE{color=ref(G.MEMREG(_, m)), ...}) = G.MEM_REG m
         | getLoc(G.NODE{color=ref(G.SPILL_LOC s), ...}) = G.FRAME s
         | getLoc(G.NODE{color=ref(G.SPILLED), number, ...}) = G.FRAME number
         | getLoc(G.NODE{color=ref(G.PSEUDO), number, ...}) = G.FRAME number
         | getLoc _ = error "getLoc"

       fun printRegs regs = 
           app (fn r => print(CellsBasis.toString r^" ["^
                              Core.spillLocToString G (CellsBasis.cellId r)^"] ")) regs
 
       val parallelCopies = Word.andb(Core.HAS_PARALLEL_COPIES, mode) <> 0w0

       fun chase(CB.CELL{col=ref(CB.ALIASED c), ...}) = chase c
         | chase c = c

       fun cellId(CB.CELL{id, ...}) = id

       fun sameCell(CB.CELL{id=x,...}, CB.CELL{id=y, ...}) = x=y

       fun same(x,regToSpill) = sameCell(chase x,regToSpill)

       (*
        * Rewrite the instruction given that a bunch of registers have 
        * to be spilled and reloaded.
        *)
       fun spillRewrite{pt, instrs, annotations} = 
       let 
           (*
            * Insert reloading code for an instruction.
            * Note: reload code goes after the instruction, if any.
            *)
           fun reloadInstr(instr,regToSpill,spillLoc) = 
           let val {code, proh, newReg} =
                  reload{instr=instr,reg=regToSpill,
                         spillLoc=spillLoc,annotations=annotations}
           in  addProh(proh); 
               code
           end
    
           (*
            * Renaming the source for an instruction.
            *)
           fun renameInstr(instr,regToSpill,toSrc) = 
           let val {code, proh, newReg} =
                  renameSrc{instr=instr, fromSrc=regToSpill,toSrc=toSrc}
           in  addProh(proh);
               code
           end

           (* 
            * Remove uses of regToSpill from a set of parallel copies.
            * If there are multiple uses, then return multiple moves.
            *)
           fun extractUses(regToSpill, rds, rss) =
           let fun loop(rd::rds, rs::rss, newRds, rds', rss') =
                   if same(rs,regToSpill) then
                      loop(rds, rss, rd::newRds, rds', rss')
                   else 
                      loop(rds, rss, newRds, rd::rds', rs::rss')
                 | loop(_, _, newRds, rds', rss') = (newRds, rds', rss')
           in loop(rds, rss, [], [], []) end
    
           (*
            * Insert reload code for the sources of a copy.
            * Transformation:
            *    d1..dn <- s1..sn
            * =>
            *    d1..dn/r <- s1...sn/r.
            *    reload code
            *    reload copies
            *
            *)
           fun reloadCopySrc(instr,regToSpill,spillLoc) = 
           let val (dst, src) = P.moveDstSrc instr
               val (rds, copyDst, copySrc) = extractUses(regToSpill, dst, src)
               fun processMoves([], reloadCode) = reloadCode 
                 | processMoves(rd::rds, reloadCode) =
                   let val code =
                       reloadDst{spillLoc=spillLoc,reg=regToSpill,
                                 dst=rd,annotations=annotations}
                   in  processMoves(rds, code@reloadCode)
                   end
               val reloadCode = processMoves(rds, [])
           in  case copyDst of
                 [] => reloadCode
               | _  => copyInstr((copyDst, copySrc), instr) @ reloadCode
           end 
    
           (*
            * Insert reload code
            *)
           fun reload(instr,regToSpill,spillLoc) =
               if P.moveInstr instr then   
                  reloadCopySrc(instr,regToSpill,spillLoc) 
               else
                  reloadInstr(instr,regToSpill,spillLoc)
    
           (*
            * Check whether the id is in a list
            *)
           fun containsId(id,[]) = false
             | containsId(id:CB.cell_id,r::rs) = r = id orelse containsId(id,rs)
           fun spillConflict(G.FRAME loc, rs) = containsId(~loc, rs)
             | spillConflict(G.MEM_REG(CB.CELL{id, ...}), rs) = 
                 containsId(id, rs)

           fun contains(r',[]) = false
             | contains(r',r::rs) = sameCell(r',r) orelse contains(r',rs)

           (*
            * Insert spill code for an instruction.
            * Spill code occur after the instruction.
            * If the value in regToSpill is never used, the client also
            * has the opportunity to remove the instruction.
            *)
           fun spillInstr(instr,regToSpill,spillLoc,kill) = 
           let val {code, proh, newReg} =
                  spill{instr=instr, 
                        kill=kill, spillLoc=spillLoc,
                        reg=regToSpill, annotations=annotations}
           in  addProh(proh);
               code
           end
    
           (* Remove the definition regToSpill <- from 
            * parallel copies rds <- rss.
            * Note, there is a guarantee that regToSpill is not aliased
            * to another register in the rds set.
            *)
           fun extractDef(regToSpill,rds,rss,kill) =
           let fun loop(rd::rds, rs::rss, rds', rss') =
                   if spillLocOf rd = spillLocOf rs then
                      (rs, rds@rds', rss@rss', true)
                   else if same(rd, regToSpill) then
                      (rs, rds@rds', rss@rss', kill)
                   else loop(rds, rss, rd::rds', rs::rss')
                 | loop _ = 
                     (print("rds="); 
                      app (fn r => print(CellsBasis.toString r^":"^
                                         i2s(spillLocOf r)^" ")) rds;
                      print("\nrss="); 
                      app (fn r => print(CellsBasis.toString r^":"^
                                         i2s(spillLocOf r)^" ")) rss;
                      print "\n";
                      error("extractDef: "^CellsBasis.toString regToSpill))
           in loop(rds, rss, [], []) end
    
           (*
            * Insert spill code for a destination of a copy
            *    suppose d = r and we have a copy d <- s in
            *    d1...dn <- s1...sn
            *
            *    d1...dn <- s1...sn
            * =>
            *    spill s to spillLoc 
            *    d1...dn/d <- s1...sn/s
            *
            *    However, if the spill code may ovewrite the spill location
            *    shared by other uses, we do the following less 
            *    efficient scheme:  
            *
            *    /* save the result of d */
            *    d1...dn, tmp <- s1...sn, s
            *    spill tmp to spillLoc /* spill d */
            * 
            *)
           fun spillCopyDst(instr,regToSpill,spillLoc,kill,don'tOverwrite) = 
           let val (dst, src) = P.moveDstSrc instr
               val (mvSrc,copyDst,copySrc,kill) = 
                    extractDef(regToSpill,dst,src,kill)
               val copy = case copyDst of
                            [] => []
                          | _  => copyInstr((copyDst,copySrc),instr)
           in 
	       if kill andalso not(!keep_dead_copies) 
               then (* kill the move *)
                 ((* print ("Copy "^Int.toString(hd mvDst)^" <- "^
                                 Int.toString(hd mvSrc)^" removed\n"); *) 
                  copy
                 )
               else (* normal spill *)
                 if spillConflict(spillLoc, don'tOverwrite) then
                 let (* cycle found *)
                     (*val _ = print("Register r"^Int.toString regToSpill^ 
                                  " overwrites ["^Int.toString spillLoc^"]\n")*)
                     val tmp = C.newVar regToSpill (* new temporary *)
                     val copy = copyInstr((tmp::copyDst, mvSrc::copySrc),
                                               instr) 
                     val spillCode = spillSrc{src=tmp,reg=regToSpill,
                                              spillLoc=spillLoc,
                                              annotations=annotations}
                 in  copy @ spillCode
                 end
                 else
                 let (* spill the move instruction *)
                     val spillCode = spillSrc{src=mvSrc,reg=regToSpill,
                                              spillLoc=spillLoc,
                                              annotations=annotations}
                 in  spillCode @ copy
                 end
           end
    
           (*
            * Insert spill code for a copy
            *)
           fun spillCopy(instr,regToSpill,spillLoc,kill,don'tOverwrite) =
               case P.moveTmpR instr of
                 NONE => spillCopyDst(instr,regToSpill,spillLoc,kill,
                                      don'tOverwrite)
               | SOME tmp => 
                   if same(tmp, regToSpill)
                   then ((* spilledCopyTmps := !spilledCopyTmps + 1; *)
                         [spillCopyTmp{copy=instr, reg=regToSpill,
                                      spillLoc=spillLoc,
                                      annotations=annotations}])
                   else spillCopyDst(instr,regToSpill,spillLoc,kill,
                                      don'tOverwrite)
    
           (*
            * Insert spill code
            *)
           fun spill(instr,regToSpill,spillLoc,killSet,don'tOverwrite) =
           let val kill = contains(regToSpill,killSet)
           in  if P.moveInstr instr then
                  spillCopy(instr,regToSpill,spillLoc,kill,don'tOverwrite)
               else
                  spillInstr(instr,regToSpill,spillLoc,kill)
           end

           fun contains([],reg) = false
             | contains(r::rs,reg) = same(r,reg) orelse contains(rs,reg)
           fun hasDef(i,reg) = contains(#1(insnDefUse i),reg)
           fun hasUse(i,reg) = contains(#2(insnDefUse i),reg)

           fun spillOneReg([],_,_,_,_) = []
             | spillOneReg(i::instrs,r,spillLoc,killSet,don'tOverwrite) = 
               if hasDef(i,r) 
               then 
                spillOneReg(spill(i,r,spillLoc,killSet,don'tOverwrite)@instrs,
                                  r,spillLoc,killSet,don'tOverwrite)
               else i::spillOneReg(instrs,r,spillLoc,killSet,don'tOverwrite)

           fun reloadOneReg([],_,_) = []
             | reloadOneReg(i::instrs,r,spillLoc) = 
               if hasUse(i,r) 
               then reloadOneReg(reload(i,r,spillLoc)@instrs,
                                 r,spillLoc)
               else i::reloadOneReg(instrs,r,spillLoc)

           (* This function spills a set of registers for an instruction *)
           fun spillAll(instrs,[],killSet,don'tOverwrite) = instrs 
             | spillAll(instrs,r::rs,killSet,don'tOverwrite) = 
               let val node     = getnode r
                   val spillLoc = getLoc node
               in  spillAll(
                       spillOneReg(instrs,r,spillLoc,killSet,don'tOverwrite),
                            rs,killSet,don'tOverwrite)
               end

           (* This function reloads a set of registers for an instruction *)
           fun reloadAll(instrs,[]) = instrs
             | reloadAll(instrs,r::rs) = 
               let val node     = getnode r
                   val spillLoc = getLoc node
               in  reloadAll(reloadOneReg(instrs,r,spillLoc),rs)
               end

           fun loop([], pt, newInstrs) = newInstrs
             | loop(instr::rest, pt, newInstrs) = 
               let val spillRegs = getSpills pt
                   val reloadRegs = getReloads pt
               in  case (spillRegs, reloadRegs) of
                     ([], []) => loop(rest, dec pt, instr::newInstrs)
                   | _ =>
                     (* Eliminate duplicates from the spill/reload candidates *)
                     let val killRegs   = getKills pt
                         val spillRegs  = uniq spillRegs
                         val reloadRegs = uniq reloadRegs

                         (* spill locations that we can't overwrite if we
                          * are spilling a parallel copy
                          *)
                         val don'tOverwrite = 
                             if parallelCopies then spillLocsOf reloadRegs
                             else []

                         val instrs = spillAll([instr],spillRegs,killRegs,
                                               don'tOverwrite)

                         val _ = if debug then 
                               (print("pt="^pt2s pt^"\n");
                                case spillRegs of 
                                  [] => ()
                                |  _ => (print("Spilling "); 
                                         printRegs spillRegs;
                                         print "\n");
                                case reloadRegs of
                                  [] => ()
                                | _  => (print("Reloading "); 
                                         printRegs reloadRegs; 
                                         print "\n");
                                print "Before:"; emit instr
                               ) else ()

                         val instrs = reloadAll(instrs,reloadRegs)

                         val _ =  if debug then
                               (print "After:"; app emit instrs;
                                print "------------------\n")
                               else ()

                         fun concat([], l) = l
                           | concat(a::b, l) = concat(b, a::l)
                     in  loop(rest, dec pt, concat(instrs, newInstrs)) 
                     end
                end
       in  loop(rev instrs, pt, [])
       end
   in  spillRewrite
   end
end

end (* local *)
