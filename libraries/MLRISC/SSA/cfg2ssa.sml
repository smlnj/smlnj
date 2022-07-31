(*----------------------------------------------------------------------
 * This module constructs an SSA graph from an control flow graph.
 * The control flow graph is kept abstract so that we can specialize
 * this module to various representations.
 *
 * Some improvements:
 * 1.  This implementation uses a variant Sreedhar et al.'s DJ-graph to compute
 *     the iterated dominance frontier.  This variant computes liveness
 *     by demand at the same time DF^+ is computed.  So there is no need 
 *     to computing liveness using iterative dataflow analysis.  The advantage
 *     is that |DF^+(S) \intersect Liveness| can be substantially smaller
 *     than |DF^+(S)| + |Liveness|, so in some situations much less work 
 *     is performed.
 * 2.  We identify those registers that are only defined once in the 
 *     compilation unit.   These registers will not be given new names.
 *     This way we eliminate a lot of unncessary renaming!  
 *     This is particular important since each renaming has to propagate 
 *     along all the information associated with cellkinds (and/or gc types).
 *
 * -- Allen (leunga@cs.nyu.edu)
 *----------------------------------------------------------------------*)
functor CFG2SSA
   (structure SSA : SSA
    structure InsnProps : INSN_PROPERTIES
      sharing SSA.I = InsnProps.I 
   ) : CFG2SSA =  
struct
   structure SSA  = SSA
   structure CFG  = SSA.CFG
   structure Dom  = SSA.Dom
   structure SP   = SSA.SP  
   structure RTL  = SSA.RTL
   structure T    = RTL.T
   structure P    = SP.RTLProps
   structure C    = SSA.C
   structure SL   = SortedList
   structure DJ   = SSA.DJ
   structure G    = Graph
   structure A    = Array
   structure W8A  = Word8Array
   structure CDJ = CompressedDJGraph(Dom)

   fun error msg = MLRiscErrorMsg.error("CFG2SSA", msg)

   (*----------------------------------------------------------------------
    * Common flags
    *----------------------------------------------------------------------*)
   val copyProp          = MLRiscControl.getFlag "ssa-copy-prop"
   val keepName          = MLRiscControl.getFlag "ssa-keep-name"
   val removeUselessPhis = MLRiscControl.getFlag "ssa-remove-useless-phis"
   val consistencyCheck  = MLRiscControl.getFlag "ssa-consistency-check"
   val debugSSA          = MLRiscControl.getFlag "ssa-debug"
   val ssaStats          = MLRiscControl.getFlag "ssa-stats"
   val _ = copyProp   := true (* by default, perform copy propagation *)
   val _ = removeUselessPhis := true (* by default, remove useless phi-nodes *)
   val debug = false

   datatype ssa_variant = 
              MINIMAL | SEMI_PRUNED | PRUNED 
            | C_MINIMAL | C_PRUNED 
   val ssa_variant = C_PRUNED
   val add_entry = true
   val sanity_check = true

   (*
    * Counters 
    *)
   val phi_nodes = MLRiscControl.getCounter "phi-nodes"
   val ssa_nodes = MLRiscControl.getCounter "ssa-nodes"

   exception NoName
   exception NoLiveIn

   (*------------------------------------------------------------------
    * Hacks to deal with zero registers in the architecture
    *------------------------------------------------------------------*)
   val zeroRegs = SSA.zeroRegs
   fun isZero r = W8A.sub(zeroRegs,r) <> 0w0 handle _ => false

   (*------------------------------------------------------------------
    * Deal with pinned resources
    *------------------------------------------------------------------*)
   val pinnedUseTbl = SSA.pinnedUseTbl
   val pinnedDefTbl = SSA.pinnedDefTbl
   fun isPinnedUse r = W8A.sub(pinnedUseTbl,r) <> 0w0 handle _ => false
   fun isPinnedDef r = W8A.sub(pinnedDefTbl,r) <> 0w0 handle _ => false
   fun hasPinnedUse [] = false
     | hasPinnedUse (r::rs) = isPinnedUse r orelse hasPinnedUse rs 
   fun hasPinnedDef [] = false
     | hasPinnedDef (r::rs) = isPinnedDef r orelse hasPinnedDef rs 

   (*------------------------------------------------------------------
    * Check if a variable is live in Entry
    *------------------------------------------------------------------*)
   fun addEntry(G.GRAPH cfg) =
   if add_entry then 
   let val N        = #capacity cfg ()
       val liveIn   = A.array(N, ~1)
       val in_alpha = A.array(N, ~1)
       val [ENTRY]  = #entries cfg ()
       val counter  = ref 0
       fun addEntry f {defs, localLiveIn} = 
       let val v = !counter
           val _ = counter := !counter + 1
           fun markLiveIn(b) =
           let fun markPred [] = ()
                 | markPred((j,_,_)::es) =
                    (if A.sub(liveIn,j) <> v andalso
                        A.sub(in_alpha,j) <> v then
                       markLiveIn j
                     else ();
                     markPred es
                    )
           in  A.update(liveIn,b,v);
               markPred(#in_edges cfg b)
           end
           fun markDef b = A.update(in_alpha, b, v)
           fun addEntries([],defs) = defs
             | addEntries((_,b,_)::es,defs) = 
               if A.sub(liveIn,b) = v then addEntries(es, b::defs)  
               else addEntries(es, defs)
       in  app markDef defs;
           app markLiveIn localLiveIn;
           f (addEntries(#out_edges cfg ENTRY, defs))
       end
   in  addEntry
   end 
   else (fn f => fn {defs, localLiveIn} => f defs)


   (*----------------------------------------------------------------------
    * Main function
    *----------------------------------------------------------------------*)
   fun buildSSA{cfg, dom} = 
   let (*------------------------------------------------------------------
        * Flags 
        *------------------------------------------------------------------*)
       val copyProp    = !copyProp
       val keepName    = !keepName

       (* extracts the gc map *)
       val gcmap = #get SSA.GCMap.GCMAP (!(CFG.annotations cfg))

       (* create a name table if we want to keep around the original name *)
       val nameTbl     = if keepName then 
                            SOME(IntHashTable.mkTable(37, NoName)) else NONE
       val SSA         = SSA.newSSA{cfg=cfg, dom=dom, 
                                    nameTbl=nameTbl, gcmap=gcmap}
       val cellKindTbl = SSA.cellKindTbl SSA
       val addCellKind = IntHashTable.insert cellKindTbl
       val regmap      = C.lookup (CFG.regmap cfg)
       val maxPos      = SSA.maxPos SSA

       (*------------------------------------------------------------------
        * Graph structures and tables
        *------------------------------------------------------------------*)
       val Dom as G.GRAPH dom = dom cfg
       val N                  = #capacity dom () (* number of blocks *)
       val V                  = C.maxCell()      (* max variable id *)
       val CFG as G.GRAPH cfg = Dom.cfg Dom
       val DU                 = A.array(N, [])
       val localLiveIn        = A.array(V, [])
       val G.GRAPH ssa        = SSA
       val startId            = #order ssa ()
       val ssaOpsCount        = ref startId
       val showOp             = SSA.showOp SSA
       val showVal            = SSA.showVal SSA
       val [ENTRY]            = #entries cfg ()
       val [EXIT]             = #exits cfg ()

       (*------------------------------------------------------------------
        * Special instructions
        *------------------------------------------------------------------*)
       val phiOp    = SP.phi
       val sinkOp   = SP.sink
       val sourceOp = SP.source

       (*------------------------------------------------------------------
        * Propagate gc info
        *------------------------------------------------------------------*)
       val propagateGCInfo =
           case gcmap of
              NONE => (fn _ => ())
           |  SOME map => 
                let val lookup = IntHashTable.lookup map
                    val add    = IntHashTable.insert map
                in  fn {from, to} => 
                       (lookup to; ()) handle _ => 
                        (add(to, lookup from) handle _ => ())
                end
            
       (*------------------------------------------------------------------
        * Check for initial and terminal blocks
        *------------------------------------------------------------------*)
       fun isInitial b = b = ENTRY orelse b <> EXIT andalso 
                         List.exists(fn (i, _, _) => i=ENTRY) (#in_edges cfg b)
       fun isTerminal b = b <> ENTRY andalso
                          List.exists(fn (_, i, _) => i=EXIT) (#out_edges cfg b)
       fun hasSource b = List.exists(fn (i, _, _) => i=ENTRY) (#in_edges cfg b)

       (*------------------------------------------------------------------
        * Initialize cellkinds of physical registers 
        *------------------------------------------------------------------*)
       fun initCellKinds() = 
           app (fn k => 
                let val {low, high} = C.cellRange k 
                    fun loop r =
                        if r <= high then (addCellKind(r,k); loop(r+1))
                        else ()
                in  loop low end
                handle _ => ()
               ) C.cellkinds

       (*------------------------------------------------------------------
        * How to get the live out of an exit block.
        * Remove all zero registers from the liveOut!
        *------------------------------------------------------------------*)
       fun getLiveOut(b,block) = 
           SL.uniq(
              List.foldr 
                (fn (r,S) =>
                  let val r = regmap r
                  in  if isZero r then S else r::S end)  
                [] (C.CellSet.toCellList(CFG.liveOut block))
           )

       (*------------------------------------------------------------------
        * Initialize all the special tables 
        *------------------------------------------------------------------*)
       fun initTables() =
       let val getOperands = 
               P.defUse(SP.OT.makeNewValueNumbers(SSA.operandTbl SSA))
           fun getRegs([], rs) = rs
             | getRegs(v::vs, rs) = getRegs(vs, if v >= 0 then v::rs else rs)
           val updateCellKind = P.updateCellKind{update=addCellKind}

           (* A source node for each entry, and a sink node for each exit *)
           val _ =  ssaOpsCount := !ssaOpsCount + 
                                   length(#entries cfg ()) + 
                                   length(#exits cfg ()) +
                                   1   (* ENTRY node *)

           (* Compute def/use 
            * Also, create the instructions.
            *) 
           fun defUse(b,b') = 
           let fun scan([], du, def, use) = (du, def, use)
                 | scan(insn::insns, du, def, use) = 
                    (updateCellKind insn;
                     let val (d,u) = getOperands insn
                         val d'    = SL.uniq d
                         val u'    = SL.uniq(getRegs(u,[]))
                         val use   = SL.merge(SL.difference(use,d'),u')
                         val def   = SL.difference(SL.merge(def,d'),u')
                     in  scan(insns, (d,u)::du, def, use) end
                    )
               val liveOut = getLiveOut(b,b')      
               val (du, def, use) = scan(!(CFG.insns b'),[],[],liveOut)
           in  A.update(DU, b, du);
               ssaOpsCount := !ssaOpsCount + length du; (* count the instrs *)
               (def, use)
           end

           fun enterLocalLiveInInfo(b,b') = 
           let fun mark [] = ()
                 | mark(r::rs) = 
                   (A.update(localLiveIn,r,b::A.sub(localLiveIn,r)); mark rs)
               val (_,use) = defUse(b,b')
           in  mark use
           end
 
       in  #forall_nodes cfg enterLocalLiveInInfo
       end 
      
       (*------------------------------------------------------------------
        * Definitions inside a block.
        *------------------------------------------------------------------*)
       fun defsOf(b) =
       let val defs = 
               foldr (fn ((d,_),l) => List.revAppend(d,l)) [] (A.sub(DU,b))
       in  SL.uniq defs 
       end

       (*------------------------------------------------------------------
        * How to rename a variable
        *------------------------------------------------------------------*)
       val renameVar = SSA.newRenamedVar SSA

       (*------------------------------------------------------------------
        * Compute the locations of all phi-functions based on the
        * definition site information.  We also prune out non-live phi
        * functions at the same time.  Note that even after pruning the
        * set of phi-functions may still be an overestimate, because of
        * copy propagation.
        *------------------------------------------------------------------*)
       fun placePhiFunctions() =
       let val defSites = A.array(V, []) (* variable -> blocks *)
           val phis     = A.array(N, []) (* block -> phi functions *)
           val prList = List.foldr (fn (r,"") => Int.toString r
                                     | (r,s) => Int.toString r^","^s) ""
           val _ = #forall_nodes cfg
                     (fn (b,_) =>
                        app (fn v => A.update(defSites,v,b::A.sub(defSites,v)))
                           (defsOf b))
           val LiveIDFs = 
               case ssa_variant of
                 MINIMAL     => let val IDFs = DJ.IDFs(DJ.DJ Dom)
                                    val addEntry = addEntry CFG
                                in  addEntry IDFs end
               | SEMI_PRUNED => let val IDFs = DJ.IDFs(DJ.DJ Dom) 
                                in  fn {defs,localLiveIn=[]} => []
                                     | {defs,localLiveIn} => IDFs defs 
                                end
               | PRUNED => DJ.LiveIDFs(DJ.DJ Dom) 
               | C_MINIMAL   => let val IDFs = CDJ.IDFs(CDJ.DJ Dom)
                                    val addEntry = addEntry CFG
                                in  addEntry IDFs end
               | C_PRUNED => if sanity_check then
                             let val dj1 = CDJ.LiveIDFs(CDJ.DJ Dom) 
                                 val dj2 = DJ.LiveIDFs(DJ.DJ Dom) 
                             in  fn x =>
                                 let val idf1 = dj1 x
                                     val idf2 = dj2 x
                                     fun pr s = foldr (fn (x,l) =>
                                           Int.toString x^" "^l) "" s
                                 in  if SL.uniq idf1 = SL.uniq idf2 then idf1
                                     else (print("IDF1="^pr idf1^"\n");
                                           print("IDF2="^pr idf2^"\n");
                                           idf1)
                                 end
                             end
                             else CDJ.LiveIDFs(CDJ.DJ Dom) 
           fun insertPhi(v, [], n) = n
             | insertPhi(v, defSites, n) = 
               let fun insert([], n) = n
                     | insert(b::bs, n) =
                       (A.update(phis, b, (v,v,[])::A.sub(phis, b));
                        insert(bs, n+1)
                       )
                   val blocks = LiveIDFs{defs=defSites, 
                                         localLiveIn=A.sub(localLiveIn,v)}
                   (* val _ = print("r"^Int.toString v^" defs="^prList defSites
                                ^" IDF="^prList blocks^"\n")*)
               in  insert(blocks, n)
               end
       in  ssaOpsCount := 
              A.foldri insertPhi (!ssaOpsCount) (defSites,0,NONE);
           phis
       end

       (*------------------------------------------------------------------
        * Compute the SSA form by walking the dominator tree and renaming 
        * all definitions in the program.  We also do a few things in the
        * process:
        *
        *  (1) Keep track of live in variables in each entry point.
        *      These are found by looking at the renaming stack.  If
        *      the renaming stack is empty at the time of lookup, the
        *      value being looked up is a live in value.
        *  
        *------------------------------------------------------------------*)
       fun walkTree() = 
       let val phis    = placePhiFunctions() (* compute the phi functions *)
           val stacks  = A.array(V, [])      (* renaming stack *)
           val preds   = A.array(N, [])      (* predecessors of block N;
                                                must be in the same order as the
                                                arguments of phi-functions *)
           val liveInSets = IntHashTable.mkTable(3, NoLiveIn)
           val lookupLiveInSet = IntHashTable.lookup liveInSets

           val dominatingEntries = Dom.entryPos Dom

           (* Create the liveIn sets *)
           val _ = app (fn Z => 
                         IntHashTable.insert liveInSets 
                            (Z, IntHashTable.mkTable(32,NoLiveIn))
                       ) (#succ dom ENTRY)

           val defCounts = W8A.array(V, 0w1) (* count # of definitions *)

           (* Various tables *)
           val _ = SSA.reserve SSA (!ssaOpsCount); (* reserve space *)
           val newOp = SSA.newOp SSA

           (* Get a new name *)
           fun newName v = 
               if W8A.sub(defCounts,v) = 0w0 
               then (W8A.update(defCounts,v,0w1); v)
               else renameVar v 

           (* Reset the renaming stack *)
           fun reset [] = ()
             | reset(v::vs) =
               let val _::tl = A.sub(stacks,v)
               in  A.update(stacks, v, tl);
                   reset vs
               end 

           val SOME infPos = Int.maxInt
           val SOME neginfPos = Int.minInt

           (* Add a new live in register r with new name v in block Y.
            * We either have to add a new entry in the liveInSet or insert
            * a new phi-node. 
            *)
           fun addLiveIn(r,Y) =
               let val L = lookupLiveInSet Y
               in  IntHashTable.lookup L r handle _ =>
                   let val v = newName r
                       val _ = IntHashTable.insert L (r,v)
                   in  if isInitial Y then ()
                       else (* Y is not an ENTRY; add phi node *)
                       let fun addPreds([], vs') = rev vs'
                             | addPreds(Z::Zs, vs') = 
                               let val W  = A.sub(dominatingEntries, Z)
                                   val v' = addLiveIn(r,W)
                               in  addPreds(Zs, v'::vs')
                               end
                           val preds = A.sub(preds, Y)
                           val vs'  = addPreds(preds, []) 
                       in  (* print("["^Int.toString Y^"] live in phi "^
                                  showVal r^"\n"); *)
                           A.update(phis, Y, (r, v, vs')::A.sub(phis, Y));
                           ssaOpsCount := !ssaOpsCount + 1
                       end;
                       v
                   end 
               end

           (* Add live in at entry *)
           (* val addLiveIn = fn(r,Y) => addLiveIn(r,ENTRY) *)

           (* Rename block X.
            * Y is the block that is immediately dominated by ENTRY and 
            * dominates X.
            *) 
           fun walk(X, Y, ssa_id) =
           let val oldDefs = ref []

               (* Lookup the current name for v *)
               fun lookup v =
                   case A.sub(stacks,v) of
                     [] => addLiveIn(v, Y) (* v is live in *)
                   | v'::_ => v'

               (* Rename uses by looking up from the renaming stack *)
               and renameUse v = if v < 0 then v else lookup v 
               and renameUses [] = []
                 | renameUses (v::vs) = renameUse v::renameUses vs

               (* Rename a definition of v.
                * We'll try to keep the original name whenever possible.
                * For example, if the original name has only one definition;
                * then we don't have to rename it at all.
                *
                * For zero registers, make sure we don't create a new definition
                *)
               and renameDef v =
               let val v' = newName v
               in  if isZero v then v' 
                   else
                      let val vs = A.sub(stacks,v)
                      in  A.update(stacks,v,v'::vs);
                          oldDefs := v :: !oldDefs;
                          v'
                      end
               end

               and renameDefs [] = []
                 | renameDefs (v::vs) = renameDef v::renameDefs vs
 
               fun copyDef(dst,src) =
                   (propagateGCInfo{from=dst,to=src}; 
                    A.update(stacks,dst,src::A.sub(stacks,dst));
                    oldDefs := dst :: !oldDefs
                   )

                   (* parallel copies *)
               fun copy{dst, src} = 
                  ((* print("Copying ");
                   app (fn r => print(Int.toString r^" ")) dst;
                   print "<- ";
                   app (fn r => print(Int.toString r^" ")) src;
                   print "\n"; *)
                   ListPair.app copyDef (dst, map lookup src)
                  )

                   (* rename the definition of a phi function *) 
               fun renamePhiDef X =
               let fun rn [] = []
                     | rn((v',v,uses)::rest) = (v',renameDef v,uses)::rn rest
               in  A.update(phis, X, rn(A.sub(phis,X))) end

                   (* simplify parallel copies *)
               fun simplifyCopies(dst, src) = 
               let fun loop(d::ds, s::ss, dst, src) = 
                       if d = s then loop(ds, ss, dst, src)
                       else loop(ds, ss, d::dst, s::src)
                     | loop(_, _, dst, src) = (dst, src)
               in  loop(dst, src, [], []) end

                   (*
                    * Insert a sink node into the ssa graph
                    *)
               fun addSink(id, block) =
               let val liveOut = getLiveOut(X, block)
                   val uses = renameUses liveOut
               in  newOp{id=id, instr=sinkOp, pos=infPos,
                         block=X, defs=[], uses=uses,
                         rtl=T.SINK{block=X, liveOut=liveOut}
                        };
                   if debug then print("new "^showOp id^"\n") else ();
                   #set_exits ssa (id:: #exits ssa ());
                   id + 1
               end

                   (* rename the instructions in block X *)
               fun renameBlock(X, ssa_id) =
               let (* scan blocks, rename instructions and add new ssa ops *)
                   fun scan(id, i::insns, pos, (defs, uses)::DU) = 
                       let fun addOp(instr,defs,uses,p) = 
                           let val rtl = P.rtl instr
                               val rtl = if hasPinnedUse uses orelse
                                            hasPinnedDef defs then
                                            RTL.pin rtl else rtl
                               val uses = renameUses uses
                               val defs = renameDefs defs
                           in  newOp{id=id,instr=instr,defs=defs,uses=uses,
                                     rtl=rtl, block=X, pos=p};
                               if debug then print("new "^showOp id^"\n") 
                               else ();
                               scan(id+1, insns, pos+128, DU)
                           end 
                       in  case InsnProps.instrKind i of
                             InsnProps.IK_COPY =>
                                (* copy propagation *)
                                 (copy{dst=defs, src=uses};
                                  scan(id, insns, pos, DU)
                                 )
                           | InsnProps.IK_JUMP => addOp(i,defs,uses,infPos)
                           | _ => addOp(i,defs,uses,pos)
                       end
                     | scan(id, _, pos, _) = (id, pos)

                   val block  = #node_info cfg X
                   val insns  = !(CFG.insns block)
                   val DU     = A.sub(DU,X)

                   val (ssa_id, pos) = scan(ssa_id, rev insns, 0, DU)
                   val ssa_id = if isTerminal X then addSink(ssa_id, block) 
                                else ssa_id
               in  maxPos := Int.max(!maxPos, pos);
                   ssa_id
               end

                   (* rename the uses of a phi function *) 
               fun renamePhiUse X =
               let fun rename_phi_of_Y (e as (X,Y,_)) =
                   let val Y_phis = A.sub(phis, Y)
                       fun insertUses [] = []
                         | insertUses((v',v,uses)::rest) = 
                            ((*print("["^Int.toString X^"->"^Int.toString Y^
                                   "] Renaming phi "^Int.toString v'^"\n");*)
                             (v',v,renameUse v'::uses)::insertUses rest)
                   in  A.update(preds,Y,X::A.sub(preds,Y)); 
                       A.update(phis,Y,insertUses Y_phis)
                   end
               in  app rename_phi_of_Y (#out_edges cfg X) end

               val _      = renamePhiDef X
               val ssa_id = renameBlock(X, ssa_id)
               val _      = renamePhiUse X

               fun walkSucc([], _, ssa_id) = ssa_id
                 | walkSucc(X::Xs, Y, ssa_id) = 
                    walkSucc(Xs, Y, walk(X, Y, ssa_id))

               val ssa_id = walkSucc(#succ dom X, Y, ssa_id)
           in  reset (!oldDefs);
               ssa_id
           end

           fun walkAll([], ssa_id) = ssa_id
             | walkAll(X::Xs, ssa_id) = walkAll(Xs, walk(X, X, ssa_id))  

           (*
            * Insert a source definitions for all zero registers
            *)
           fun addZeroRegs(ENTRY) = 
           let val L   = IntHashTable.mkTable(16,NoLiveIn)
               val add = IntHashTable.insert L
               fun defineZero(k) = 
                   case C.zeroReg k of
                     SOME r => let val v = newName r
                               in  add(r,v);
                                   A.update(stacks,r,[v])
                               end
                   | NONE => ()
           in  IntHashTable.insert liveInSets (ENTRY, L);
               app defineZero C.cellkinds
           end

           val _ = addZeroRegs ENTRY

              (* Insert all normal nodes first *)
           val ssa_id = ref(walkAll(#succ dom ENTRY, startId))

              (* 
               * Insert a source node into the ssa graph.
               *)
           fun addSource(X, liveInSet) = 
           if isInitial X then 
           let val LiveIn = IntHashTable.listItemsi liveInSet
               val liveInSet = 
                  ListMergeSort.sort (fn ((i,_),(j,_)) => i < j) LiveIn

               fun mark([], regs, defs) = (regs, defs)
                 | mark((r,d)::l, regs, defs) =
                   mark(l, r::regs, d::defs)

               val (regs, defs) = mark(liveInSet, [], [])
               val id = !ssa_id 

           in  (* print("LiveIn["^Int.toString X^"] = ");
               app (fn (r,v) => 
                    print(Int.toString r^" "^showVal r^" "^Int.toString v^",")) 
                    liveInSet;
               print "\n"; *)
               newOp{id=id, instr=sourceOp, pos=neginfPos,
                     block=X, defs=defs, uses=[], 
                     rtl=T.SOURCE{block=X, liveIn=regs}
                    };
               if debug then print("new "^showOp id^"\n") else ();
               #set_entries ssa (id:: #entries ssa ());
               ssa_id := !ssa_id + 1
           end
           else ()

           val _ = IntHashTable.appi addSource liveInSets

               (* Now reserve space for extra phi nodes for live-in values *)
           val _ = SSA.reserve SSA (!ssaOpsCount); (* reserve space *)
           val newOp = SSA.newOp SSA 
               
               (* Place phi functions *)
           fun placePhi (B as (b,block)) = 
           let val preds  = A.sub(preds, b)  
               val phis   = A.sub(phis, b)
               val phiRTL = T.PHI{preds=preds, block=b}
               fun newPhi(id, []) = id
                 | newPhi(id,(t',t,s)::phis) = 
                   (newOp{id=id, defs=[t], uses=s, rtl=phiRTL, 
                          instr=phiOp, block=b, pos=t'};
                    if debug then print("new phi "^showOp id^"\n") else ();
                    newPhi(id+1, phis)
                   )
             
           in  ssa_id := newPhi(!ssa_id, phis)
           end
 
       in  #forall_nodes cfg placePhi
       end

       fun computeStatistics(G.GRAPH ssa) = 
           (#forall_nodes ssa (fn (i,i') =>
                case InsnProps.instrKind i' of
                  InsnProps.IK_PHI => phi_nodes := !phi_nodes + 1
                | _ => ());
            ssa_nodes := !ssa_nodes + #order ssa ()
           )

   in  initCellKinds();
       initTables();
       walkTree();
       SSA.computeDefUseChains SSA;
       if !ssaStats then computeStatistics SSA else ();
       if !removeUselessPhis then SSA.removeUselessPhiFunctions SSA else ();    
       if !consistencyCheck then SSA.consistencyCheck SSA else ();
       if !debugSSA then 
          print("[SSA: "^Int.toString(!ssaOpsCount)^" nodes "^
                Int.toString(N)^" blocks]\n")
       else ();
       SSA
   end

end
