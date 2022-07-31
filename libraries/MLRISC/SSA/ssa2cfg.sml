(*----------------------------------------------------------------------------
 * This module rebuilds a CFG from an SSA.
 * The version is another rewrite of the algorithm in PLDI '99.  
 *
 * -- Allen (leunga@cs.nyu.edu)
 *----------------------------------------------------------------------------*)
functor SSA2CFG 
  (structure SSA       : SSA
   structure InsnProps : INSN_PROPERTIES
      sharing SSA.I = InsnProps.I
  ) : SSA2CFG =
struct

   structure SSA = SSA
   structure CFG = SSA.CFG
   structure DJ  = SSA.DJ
   structure SP  = SSA.SP
   structure RP  = SP.RTLProps
   structure I   = SSA.I
   structure C   = I.C
   structure RTL = SSA.RTL
   structure T   = RTL.T
   structure G   = Graph
   structure A   = Array
   structure W8A = Word8Array
   structure SL  = SortedList

   fun error msg = MLRiscErrorMsg.error("SSA2CFG",msg)

   val i2s = Int.toString

   exception Nothing

   (*------------------------------------------------------------------------
    * Flags
    *------------------------------------------------------------------------*)
   val consistencyCheck  = MLRiscControl.getFlag "ssa-consistency-check"
   val debug             = MLRiscControl.getFlag "ssa-debug-ssa->cfg"

   val propagateSource = false
   val coalesceEntry   = true
   val propagateSink   = false

   (*------------------------------------------------------------------------
    * Counters
    *------------------------------------------------------------------------*)
   val repairCopies = MLRiscControl.getCounter "ssa-repair-copies"
   val phiCopies    = MLRiscControl.getCounter "ssa-phi-copies"

   (*------------------------------------------------------------------------
    * Fixed physical registers
    *------------------------------------------------------------------------*)
   val R                = C.firstPseudo
   val zeroRegs = SSA.zeroRegs
   fun isZero r = W8A.sub(zeroRegs,r) <> 0w0 handle _ => false
   val fixedDefTbl      = W8A.array(R,0w0)
   val fixedUseTbl      = W8A.array(R,0w0)
   fun initTbl tbl      = app (fn r => W8A.update(tbl,r,0w1)) 
   val _                = initTbl fixedDefTbl SP.fixedDef
   val _                = initTbl fixedUseTbl SP.fixedUse
 
   val _                = W8A.appi (* never rename the zero registers! *)
                           (fn (i,0w0) => ()
                             | (i,_) => (W8A.update(fixedDefTbl,i,0w1);
                                         W8A.update(fixedUseTbl,i,0w1)))
                             (zeroRegs,0,NONE) 
   fun isDedicatedDef r = W8A.sub(fixedDefTbl,r) <> 0w0 handle _ => false
   fun isDedicatedUse r = W8A.sub(fixedUseTbl,r) <> 0w0 handle _ => false
   (*------------------------------------------------------------------------
    * Main entry point
    *------------------------------------------------------------------------*)
   fun buildCFG(SSA as G.GRAPH ssa) = 
   let val _ = if !consistencyCheck then SSA.consistencyCheck SSA else ()
       val CFG as G.GRAPH cfg = SSA.cfg SSA
       val Dom as G.GRAPH dom = SSA.dom SSA
       val N = #capacity cfg () (* number of blocks *)
       val M = #capacity ssa () (* number of ssa ops *) 
       val V = SSA.maxVar SSA   (* number of variables *)
       val showVal = SSA.showVal SSA
       val showOp  = SSA.showOp SSA
       val [ENTRY] = #entries cfg ()

       (* val _ = (print "Dedicated: ";
                app (fn r => print(showVal r^" ")) SP.fixedPhysical; 
                print "\n") 
        *)

       (*---------------------------------------------------------------------
        * Extract Various tables 
        *---------------------------------------------------------------------*)
       val defsTbl     = SSA.defsTbl SSA  (* definitions *)
       val usesTbl     = SSA.usesTbl SSA  (* uses *)
       val rtlTbl      = SSA.rtlTbl SSA   (* rtl *)
       val posTbl      = SSA.posTbl SSA   (* position/resource for phi*)
       val succTbl     = SSA.succTbl SSA  (* def/use chains *)
       val ssaOpTbl    = SSA.ssaOpTbl SSA (* instruction *)
       val defSiteTbl  = SSA.defSiteTbl SSA (* definition sites *)
       val blockTbl    = SSA.blockTbl SSA   (* block table *)
       val cellKindTbl = SSA.cellKindTbl SSA (* cellkinds *)
       val cellKindOf  = IntHashTable.find cellKindTbl
       val cellKindOf  = 
            fn r => case cellKindOf r of SOME k => k | NONE => C.GP
       val {sources, phis, ops, sinks} = SSA.nodes SSA (* linearized ssa *)

       (*---------------------------------------------------------------------
        * Machine description information
        *---------------------------------------------------------------------*)
       val constOf           = SSA.const SSA 
       val rewriteOperands   = SP.rewriteOperands{const=constOf}
       val mkCopies          = SP.copies
       val namingConstraints = SP.namingConstraints
       val opnKind           = RP.opnKind 
       val defUse            = RP.defUse(SP.OT.lookupValueNumbers
                                         (SSA.operandTbl SSA))
       (* Should value d be in the resource r? *) 
       fun dstInReg(r) = isDedicatedDef r
       fun opDstInReg(k,r) = k = RP.FIX orelse isDedicatedDef r

       (* Should value s be in the resource r? *) 
       fun srcInReg(r) = isDedicatedUse r
       fun opSrcInReg(k,r) = k = RP.FIX orelse isDedicatedUse r

       (*---------------------------------------------------------------------
        * Temporary data structures 
        *---------------------------------------------------------------------*)
       val Value = A.array(V, ~1) (* current value of resources *)
               (* names of all resources *)
       val Resources = IntHashTable.mkTable(32, Nothing) 
       val AvailIn  = A.array(N, [])

       (* Mark the value of a resource *)
       fun setValue(r,v) = A.update(Value,r,v)

       (* Lookup the value of a resource *)
       fun valueOf(r) = A.sub(Value,r)

       (*---------------------------------------------------------------------
        * Initialization
        * 1. Find out the names of all dedicated resources
        * 2. Find out all places where these are defined
        * 3. Compute the values of all these resources at the entry point
        *    of each block
        *---------------------------------------------------------------------*)
       fun initialization() =
       let 

           (* Definitions of all resources *) 
           val addResource   = IntHashTable.insert Resources
           val LocalAvailOut = A.array(N, [])

           (* Process a block *)
           fun processBlock(X, _) =
           let
               (* New resources *)
               fun markResource(i,r,v,R) = (setValue(r,v); r::R)
    
               (* Process the source nodes *)
               fun doSource([i], R) =   
                   let fun scan([], [], R) = R
                         | scan(d::ds, r::rs, R) = 
                           let val k = cellKindOf r
                           in  if k = C.MEM orelse k = C.CTRL then
                                  scan(ds, rs, R)
                               else
                                  scan(ds, rs, markResource(i,r,d,R))
                           end
                         | scan _ = error "doSource.scan"
                       val dst = A.sub(defsTbl, i)
                       val T.SOURCE{liveIn, ...} = A.sub(rtlTbl, i)
                   in  scan(dst, liveIn, R)
                   end
                 | doSource(_, R) = R
    
               (* Process the phi nodes *)
               fun doPhis([], R) = R
                 | doPhis(phi::phis, R) = 
                   let val [d] = A.sub(defsTbl, phi)
                       val r   = A.sub(posTbl, phi) 
                   in  doPhis(phis, 
                              if dstInReg r then markResource(phi,r,d,R) else R)
                   end
    
               (* Process the instructions *)
               fun doOps([], R) = R
                 | doOps(i::rest, R) = 
                   let fun scanUse([], [], [], R) = R
                         | scanUse(s::ss, r::rs, k::ks, R) = 
                           scanUse(ss, rs, ks,  
                                   if opSrcInReg(k, r) 
                                   then markResource(i,r,s,R)
                                   else R)
                         | scanUse _ = error "scanUse"
    
                       fun scanDef([], [], [], ds') = rev ds'
                         | scanDef(d::ds, r::rs, k::ks, ds') =
                           scanDef(ds, rs, ks, 
                                   if opDstInReg(k, r) 
                                   then markResource(i,r,d,R)
                                   else R)
                         | scanDef _ = error "scanDef"
                       val dst = A.sub(defsTbl, i)
                       val src = A.sub(usesTbl, i)
                       val instr        = A.sub(ssaOpTbl, i)
                       val (rdst, rsrc) = defUse instr
                       val (kdst, ksrc) = opnKind instr
                       val R = scanUse(src, rsrc, ksrc, R)
                       val R = scanDef(dst, rdst, kdst, R)
                   in  doOps(rest, R) end
    
               (* Process the sink nodes *)
               fun doSink([i], R) = 
                   let fun scan([], [], R) = R
                         | scan(s::ss, r::rs, R) = 
                           let val k = cellKindOf r
                           in  if k = C.MEM orelse k = C.CTRL then
                                 scan(ss, rs, R)
                               else
                                 scan(ss, rs, markResource(i,r,s,R)) 
                           end
                         | scan _ = error "doSink.scan"
                       val src                  = A.sub(usesTbl, i)
                       val T.SINK{liveOut, ...} = A.sub(rtlTbl, i)
                   in  scan(src, liveOut, R) end
                 | doSink(_, R) = R
    
               val R = doSource(A.sub(sources,X), [])
               val R = doPhis(A.sub(phis,X), R)
               val R = doOps(A.sub(ops,X), R)
               val R = doSink(A.sub(sinks,X), R)

               fun collect([], localAvailOut) = localAvailOut
                 | collect(r::rs, localAvailOut) = 
                   let val v = valueOf r
                   in  if v < 0 orelse isZero r 
                       then collect(rs, localAvailOut)
                       else (setValue(r, ~1);
                             addResource(r, true);
                             collect(rs, (r,v)::localAvailOut)
                            )
                   end
               val localAvailOut = collect(R, [])

               (*val _ = if debug then
                       case localAvailOut of
                         [] => ()
                       | _ => (print("Block ["^i2s X^"]: ");
                               app (fn (r,v) => 
                                    print(showVal r^"->"^showVal v^" "))
                                  localAvailOut;
                               print "\n"
                              )
                       else ()
                *)
           in  A.update(LocalAvailOut, X, localAvailOut)
           end

           val _ = #forall_nodes dom processBlock 

           (* Definitions indexed by resource *)
           val LocalDefs = IntHashTable.mkTable(32, Nothing)
           val lookupLocalDef = IntHashTable.find LocalDefs
           val lookupLocalDef = 
               fn r => case lookupLocalDef r of SOME d => d | NONE => []
           val addLocalDef = IntHashTable.insert LocalDefs

           val _ = A.appi (fn (X, localAvailOut) =>  
                             app (fn (r,v) => 
                                   addLocalDef(r, (X,v)::lookupLocalDef r))
                                  localAvailOut) (LocalAvailOut, 0, NONE)

           (* val _ = if debug then
                   (print "Resources=";
                    IntHashTable.appi 
                       (fn (r, _) => print(showVal r^" ")) Resources;
                    print "\n"
                   ) else ()
            *)

           (* Perform data flow analysis for each resource r *)
           val bot = ~1
           val top = ~2
           val LocalAvailOut = A.array(N, bot)
           val onWorkList    = A.array(N, ~1)
           val dj            = DJ.DJ Dom
           val IDFs          = DJ.IDFs dj

           fun availAnalysis(r,_) =  
           let val GlobalAvailIn = A.array(N, bot)

               fun init([], defs) = defs
                 | init((X,v)::A, defs) = 
                    (A.update(LocalAvailOut, X, v); 
                     A.update(onWorkList, X, r);
                     init(A, X::defs)
                    )

               fun cleanup [] = ()
                 | cleanup((X,v)::A) = 
                   (A.update(LocalAvailOut,X,bot); cleanup A)

               val localAvailOut = lookupLocalDef r

               val defSites = init(localAvailOut, [])

               fun meet([], v) = v
                 | meet((X,_,_)::es, v) = 
                   let val v' = A.sub(LocalAvailOut,X)
                       val v' = if v' >= 0 then v' else A.sub(GlobalAvailIn,X)
                   in  if v' = bot then v 
                       else if v = bot then v'
                       else if v' = top orelse v = top then top
                       else if v = v' then meet(es, v)
                       else top 
                   end

               fun insert([], WL) = WL
                 | insert((_,X,_)::es, WL) = 
                   if A.sub(onWorkList,X) = r then insert(es, WL)
                   else (A.update(onWorkList,X,r); insert(es, X::WL))

               fun update(X, WL) = 
                   let val oldV = A.sub(GlobalAvailIn, X)
                       val newV = meet(#in_edges cfg X, bot)
                   in  if oldV = newV then WL
                       else (A.update(GlobalAvailIn,X,newV);
                             if A.sub(LocalAvailOut,X) < 0 then WL
                             else insert(#out_edges cfg X, WL)
                            )
                   end
 
               fun iterate [] = ()
                 | iterate(X::WL) = 
                   let val _ = A.update(onWorkList, X, ~1)
                   in  iterate(update(X, WL))
                   end

               fun updateInfo [] = ()
                 | updateInfo(X::Xs) = 
                   let val v = A.sub(GlobalAvailIn,X) 
                   in  A.update(AvailIn, X, (r,v)::A.sub(AvailIn, X));
                       updateInfo Xs
                   end

               val ()   = iterate(defSites)
               val IDFs = IDFs defSites

           in  updateInfo IDFs;
               cleanup localAvailOut
           end

       in  IntHashTable.appi availAnalysis Resources
       end

       val _ = initialization()

           (* location of each SSA definition. ~1 means maps to itself *)
       val defLocation     = A.array(V, ~1)
       fun locationOf v = 
           if v < 0 then v 
           else
           let val r = A.sub(defLocation,v)
           in  if r < 0 then v else r end
       fun setLocation(v,r) = A.update(defLocation,v,r)

           (* repair map indexed by variable *)
       val defCompensation = W8A.array(V, 0w0)

           (* repair map indexed by ss_op *)
       val useCompensation = A.array(M, [])

       (*---------------------------------------------------------------------
        * Find conflicts 
        *---------------------------------------------------------------------*)
       fun conflicts X =
       let val valueTrail = ref []

           (* Set the current value *)
           fun setValue(r, v) =
           let val v' = A.sub(Value, r) 
           in  A.update(Value, r, v); 
               valueTrail := (r, v') :: !valueTrail 
           end

           fun valueOf r = A.sub(Value, r)

           (* Lookup current location for a name v;
            * Check whether compensation code is needed.
            *)
           fun checkUse(i,r,v) =
               if valueOf(r) = v then ()
               else
                  (if !debug then
                      let val j = A.sub(defSiteTbl, v) (* v is defined in j *)
                          val Y = A.sub(blockTbl,j)
                      in  print("WARNING["^i2s X^"] "^
                                showVal v^"="^showVal(valueOf(r))^" "^
                                showOp i^"\n"^
                                "Defined in ["^i2s Y^"] "^
                                showOp j^"\n"
                               )
                      end
                    else ();
                    (* mark variable v as needing repair *)
                    W8A.update(defCompensation, v, 0w1);
                    (* insert compensation code v->r at use site *)
                    let fun addCopy([], cps') =  
                              A.update(useCompensation, i,
                                 {kind=cellKindOf r, src=v, dst=r}::cps')
                          | addCopy({src,dst,kind}::cps, cps') =
                              if dst=r then
                                 if src=v then print "duplicate\n"
                                 else error "different sources in compensation!"
                              else addCopy(cps, cps')
                        val cps = A.sub(useCompensation, i)
                    in  addCopy(cps, cps) 
                    end;
                    (* Now the value of r is v *) 
                    setValue(r, v)
                  )

           (* Lookup current location for v 
            * Make sure that 
            *)
           fun checkDefIsAvail(i, v) = 
               let val r = A.sub(defLocation, v)
               in  if r < 0 orelse valueOf(r) = v then () 
                        (* okay *)
                   else (* v has been mapped into r.
                         * we need to preserve the value of v somehow 
                         *)
                   (if !debug then
                      let val j = A.sub(defSiteTbl, v) (* v is defined in j *)
                          val Y = A.sub(blockTbl,j)
                      in  print("WARNING["^i2s X^"] "^
                                showVal v^" mapped to "^showVal r^
                                " but valueOf("^showVal r^")="^
                                showVal(valueOf(r))^" in "^
                                showOp i^"\n"^
                                "Defined in ["^i2s Y^"] "^
                                showOp j^"\n"
                               )
                      end
                    else ();
                    (* mark variable v as needing repair *)
                    repairCopies := !repairCopies + 1;
                    W8A.update(defCompensation, v, 0w1)
                   )
               end

           (* Pop the value stack *)
           fun undoValue [] = ()
             | undoValue((r,v)::rs) = 
               (A.update(Value, r, v); undoValue rs)

           (* Process the source node *)
           fun doSource [i] =
               let fun scan([], []) = ()
                     | scan(d::ds, r::rs) = 
                       if coalesceEntry andalso X = ENTRY orelse 
                          propagateSource orelse dstInReg r then 
                          (setValue(r, d); setLocation(d,r); scan(ds, rs))
                       else 
                          (setValue(r, ~1); scan(ds, rs))
                     | scan _ = error "doSource.scan"
                   val dst = A.sub(defsTbl, i)
                   val T.SOURCE{liveIn, ...} = A.sub(rtlTbl, i)
               in  scan(dst, liveIn)
               end
             | doSource _ = ()

           (* Process the nodes *)
           fun doPhysicalAvailIn X =
           let fun init [] = ()
                 | init((r,v)::bindings) = 
                   (setValue(r, v); 
                    (* if debug then
                       print("["^i2s X^"] "^showVal r^
                             "="^i2s v^"\n")
                    else (); *)
                    init bindings)
               val availIn = A.sub(AvailIn,X)
           in  init availIn
           end

           (* Process the phi nodes *)
           fun doPhis [] = ()
             | doPhis(phi::phis) = 
               let val [d] = A.sub(defsTbl, phi)
                   val r   = A.sub(posTbl, phi) 
               in  if dstInReg r then 
                      (setValue(r, d); setLocation(d, r)) else ();
                   doPhis(phis)
               end

           (* Process the instructions *)
           fun doOps [] = ()
             | doOps(i::rest) = 
               let fun scanUse([], [], []) = ()
                     | scanUse(s::ss, r::rs, k::ks) = 
                        (if s >= 0 then
                           (if opSrcInReg(k, r) then 
                                (* If s is actually a zero register; its 
                                 * value is always available *) 
                               if isZero r then ()
                               else 
                                 (checkUse(i, r, s); 
                                  checkDefIsAvail(i, s)
                                 )
                            else 
                               checkDefIsAvail(i, s)
                           ) 
                         else ();
                         scanUse(ss, rs, ks)
                        )
                        
                   fun scanDef([], [], []) = ()
                     | scanDef(d::ds, r::rs, k::ks) =
                       (if opDstInReg(k, r) 
                        then (setValue(r, d); setLocation(d, r)) 
                        else ();
                        scanDef(ds, rs, ks)
                       )
                     | scanDef _ = error "scanDef"

                   val dst          = A.sub(defsTbl, i)
                   val src          = A.sub(usesTbl, i)
                   val instr        = A.sub(ssaOpTbl, i)
                   val (rdst, rsrc) = defUse instr
                   val (kdst, ksrc) = opnKind instr
              in   scanUse(src, rsrc, ksrc);
                   scanDef(dst, rdst, kdst);
                   doOps(rest)
              end

           (* Process the sink node *)
           fun doSink [i] = 
               let fun scan([], []) = ()
                     | scan(s::ss, r::rs) = 
                       (if s >= 0 then 
                          (if propagateSink orelse srcInReg(r) 
                           then checkUse(i, r, s) else (); 
                           checkDefIsAvail(i, s)
                           )
                        else ();
                        scan(ss, rs)
                       )
                     | scan _ = error "doSink.scan"
 
                   val src                  = A.sub(usesTbl, i)
                   val T.SINK{liveOut, ...} = A.sub(rtlTbl, i)
               in  scan(src, liveOut)
               end
             | doSink(_) = ()

       in  doPhysicalAvailIn X;
           doSource(A.sub(sources, X));
           doPhis(A.sub(phis, X));
           doOps(A.sub(ops, X));
           doSink(A.sub(sinks, X));
           app conflicts (#succ dom X);    (* walk the tree! *)
           undoValue(!valueTrail)
       end

       val _ = conflicts ENTRY

       (*
        * How to make a set of parallel copies.
        * The source can contain constants! 
        *)
       fun makeCopies(cps) = 
       let fun split([], cps', loadConsts) = mkCopies cps' @ loadConsts
             | split((cp as {src, dst, kind})::cps, cps', loadConsts) = 
               if src >= 0 then split(cps, cp::cps', loadConsts) 
               else 
               let val loadConsts = 
                   case constOf src of
                     SP.OT.INT i => 
                       InsnProps.loadImmed{t=dst, immed=i}::loadConsts
                   | SP.OT.OPERAND opn => 
                       InsnProps.loadOperand{t=dst, opn=opn}::loadConsts
               in  split(cps, cps', loadConsts)
               end
       in  split(cps, [], []) end

           (* renaming stack indexed by SSA variable name *)
       val stacks          = A.array(V, []) 

       (*---------------------------------------------------------------------
        * Rename and insert phi-functions.
        * Also insert repair code for resources.
        *---------------------------------------------------------------------*)
       fun rename X =
       let val renamingTrail = ref [] 

           (* Lookup current location for a name v;
            * Check whether compensation code is needed.
            *)
           fun locationOf(i,v) =
               if v < 0 then v (* immediate operand *)
               else if W8A.sub(defCompensation,v) <> 0w0 then v
               else 
                (case A.sub(stacks, v) of 
                   [] => v 
                | v'::_ => v'
                ) 

           (* Create a new renaming entry in the renaming stack *)
           fun renameDef(v, r) = 
               (A.update(stacks, v, r::A.sub(stacks, v));
                renamingTrail := v :: !renamingTrail
               )

           (* Pop the renaming stack *)
           fun undoRenaming [] = ()
             | undoRenaming(r::rs) = 
               let val _::vs = A.sub(stacks, r)
               in  A.update(stacks, r, vs); undoRenaming rs end

           (* Copy a value v to a register r *)
           fun copyToReg(v, r, cps) = 
               if v = r then cps else {kind=cellKindOf r, dst=r, src=v}::cps

           (* Copy a register r to a value v *)
           fun copyFromReg(r, v, cps) = 
               if v = r then cps else {kind=cellKindOf r, dst=v, src=r}::cps

           fun duplicate(s,d,[]) = false
             | duplicate(s,d,{src,dst,kind}::cps) = 
               if dst = d then 
                  if src = s then true 
                  else error "duplicate: different source"
               else duplicate(s,d,cps)
 
           (* Insert repair code from r -> v at definition of v *)
           fun compensation(r, v, cps) = 
               if W8A.sub(defCompensation,v) <> 0w0 
               then if duplicate(r,v,cps) then cps
                    else copyFromReg(r, v, cps)
               else cps

           (* Get repair code at uses of instruction j *)
           fun repair(j) = A.sub(useCompensation, j) 

           (* Process the source node *)
           fun doSource [i] =
               let fun scan([], [], cps) = cps
                     | scan(d::ds, r::rs, cps) = 
                       if coalesceEntry andalso X = ENTRY orelse 
                          propagateSource orelse dstInReg r then 
                          (renameDef(d, r); 
                           scan(ds, rs, compensation(r, d, cps))
                          )
                       else scan(ds, rs, copyFromReg(r, d, cps))
                     | scan _ = error "doSource.scan"
                   val dst = A.sub(defsTbl, i)
                   val T.SOURCE{liveIn, ...} = A.sub(rtlTbl, i)
                   val cps = scan(dst, liveIn, [])
               in  cps
               end
             | doSource _ = []

           (* Process the phi nodes *)
           fun doPhis [] = ()
             | doPhis(phi::phis) = 
               let val [d] = A.sub(defsTbl, phi)
                   val r   = A.sub(posTbl, phi) 
               in  if dstInReg r then renameDef(d, r) else ();
                   doPhis(phis)
               end

           fun notMoved(dst',[]) = true
             | notMoved(dst',{dst,src,kind}::cps) = 
                 dst<>dst' andalso notMoved(dst', cps)
 
           (* Process the instructions *)
           fun doOps([], instrs) = instrs
             | doOps(i::rest, instrs) = 
               let fun scanUse([], [], [], ss', cps) = (rev ss', cps)
                     | scanUse(s::ss, r::rs, k::ks, ss', cps) = 
                       (* actual value of s is in resource s' *) 
                       let val s' = locationOf(i, s)
                       in  if opSrcInReg(k, r) then 
                              ((* subsequent use of s now point to r *)
                               scanUse(ss, rs, ks, r::ss', 
                                       (* Make sure it is not copied multiple
                                        * times *)
                                       if notMoved(r, cps) then 
                                       copyToReg(s', r, cps) else cps)
                              )
                           else 
                              scanUse(ss, rs, ks, s'::ss', cps)
                       end    
                     | scanUse _ = error "scanUse"

                   fun scanDef([], [], [], ds', cps) = (rev ds', cps)
                     | scanDef(d::ds, r::rs, k::ks, ds', cps) =
                       if opDstInReg(k, r) then
                         (renameDef(d, r);
                          (* subsequent use of d now point to r;
                           * may need repair code here.
                           *)
                          scanDef(ds, rs, ks, r::ds', compensation(r, d, cps)) 
                         )
                       else
                          scanDef(ds, rs, ks, d::ds', cps)
                     | scanDef _ = error "scanDef"

                   val dst          = A.sub(defsTbl, i)
                   val src          = A.sub(usesTbl, i)
                   val instr        = A.sub(ssaOpTbl, i)
                   val (rdst, rsrc) = defUse instr
                   val (kdst, ksrc) = opnKind instr
                   val (instrSrc, srcCopies) = 
                        scanUse(src, rsrc, ksrc, [], repair(i))
                   val (instrDst, dstCopies) = scanDef(dst, rdst, kdst, [], [])
                   val instr'   = rewriteOperands
                                   {instr=instr, dst=instrDst, src=instrSrc}
                   (* Create instructions in in reverse order *)
                   val copiesIn  = makeCopies srcCopies
                   val copiesOut = makeCopies dstCopies
                   val instrs = 
                        List.revAppend(copiesOut,instr'::
                           List.revAppend(copiesIn, instrs))
              in   doOps(rest, instrs)
              end

           (* Check if the block Y is an exit with a 
            * simple jump instruction.
            *)
           fun isSimpleExit Y = false
               andalso
               (case (A.sub(sinks,Y), A.sub(ops,Y)) of
                  ([_], [j]) => 
                    (case A.sub(usesTbl, j) of
                      [] => true
                    | _  => false
                    ) 
                  | _ => false
               )

           (* Process the sink node *)
           fun doSink([i], cps) = 
               if isSimpleExit X then cps 
               else 
               let fun scan([], [], cps) = cps
                     | scan(s::ss, r::rs, cps) = 
                       let val s' = locationOf(i, s)
                       in  if propagateSink orelse dstInReg r then 
                             (* dealt with in repair later *)
                              scan(ss, rs, cps)
                           else 
                             scan(ss, rs, copyToReg(s', r, cps)) 
                       end
                     | scan _ = error "doSink.scan"
 
                   val src                  = A.sub(usesTbl, i)
                   val T.SINK{liveOut, ...} = A.sub(rtlTbl, i)
                   val cps = scan(src, liveOut, repair(i) @ cps)
               in  cps
               end
             | doSink(_, cps) = cps

           (* Process phi copies from all successors of X *)
           fun doPhiCopies X = 
           let fun scan([], cps) = cps
                 | scan((X,Y,_)::es, cps) =
                   if isSimpleExit Y then scan(es, cps) (* HACK!!! *)
                   else
                   let val phis = A.sub(phis, Y)
                   in  case phis of 
                         [] => cps (* nothing to do *)
                       | i::_ =>
                         let val T.PHI{preds, ...} = A.sub(rtlTbl, i)
                             fun ith([], n) = error "doPhiCopies"
                               | ith(p::ps, n) = if p = X then n 
                                                 else ith(ps,n+1)
                             val n = ith(preds, 0)
                             fun collect([], cps) = cps
                               | collect(phi::phis, cps) =
                                 let val [d] = A.sub(defsTbl, phi)
                                     val s   = List.nth(A.sub(usesTbl, phi),n)
                                     val r   = A.sub(posTbl, phi)
                                     val s'  = locationOf(phi,s)
                                     val d'  = if dstInReg r then r else d   
                                     val cps = copyToReg(s', d', cps)
                                     val cps = compensation(s', d, cps)
                                 in  (* renameDef(s, d'); XXX *)
                                     collect(phis, cps)
                                 end
                         in  scan(es, collect(phis, cps))
                         end
                   end
               val cps = scan(#out_edges cfg X, [])
           in  cps
           end

           (*
            * Stupid MLRISC hack:
            *  If entry copies or exit copies contain floating point,
            *  generate a new floating point name just to make sure 
            *  register allocation is run.
            *)
           fun floatingPointHack copies = 
           let fun containsFP [] = false 
                 | containsFP({kind,src,dst}::copies) =
                   kind = C.FP orelse containsFP copies
           in  if containsFP copies then (C.newFreg(); ()) else () end

           val entryCopies  = doSource(A.sub(sources, X))
           val _            = floatingPointHack entryCopies 
           val entryCopies  = makeCopies entryCopies
           val _            = doPhis(A.sub(phis, X))
           val instrs       = doOps(A.sub(ops, X), entryCopies)
           val phiCopies    = doPhiCopies(X)
           val exitCopies   = doSink(A.sub(sinks, X), phiCopies)
           val _            = floatingPointHack exitCopies 
           (* val _ = case exitCopies of 
                      [] => ()
                    | _ => (print("EXIT["^i2s X^"]=");
                            app (fn {src, dst, ...} => 
                                print(i2s src^"->"^i2s dst^" ")) exitCopies; 
                            print "\n") *)
           val exitCopies   = makeCopies exitCopies


           (* Insert the copies before the jump instruction at the  
            * end of the block.  Copies are in normal order. 
            * Instructions are in reversed order
            *)
           fun spliceCopies(instrs, copies) =
               case instrs of
                 [] => rev copies
               | jmp::rest =>
                 if InsnProps.instrKind jmp = InsnProps.IK_JUMP then
                    jmp::List.revAppend(copies, rest)
                 else
                    List.revAppend(copies, instrs)

           val instrs = spliceCopies(instrs, exitCopies)
       in  
           CFG.insns (#node_info cfg X) := instrs; (* update block! *)
           app rename (#succ dom X);    (* walk the tree! *)
           undoRenaming(!renamingTrail)
       end

   in  rename ENTRY;
       (* Move the instructions from the entry block to all its successors *)
       let val entryInsns = CFG.insns (#node_info cfg ENTRY)
       in  case !entryInsns of
             [] => () (* okay *)
           | instrs  => 
             (print "WARNING: Instructions in ENTRY\n";
              entryInsns := []; (* remove it *)
              app (fn (_,Y,_) => 
                   let val insns = CFG.insns(#node_info cfg Y)
                   in  insns := !insns @ map InsnProps.replicate instrs
                   end
                  ) (#out_edges cfg ENTRY)
             )
       end;
       CFG
   end

end
