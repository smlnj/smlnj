(* Disclaimer...
 * =============
 * 
 * I've written and re-written many global schedulers thru the years.
 * It is always hard to get right.  Hopefully this is the last time I have
 * to write/rewrite one for a long while... 
 *
 * A parameterizable list scheduler.
 * ================================
 * This list scheduler does a few things:
 * 1. Works on a region at a time instead of one basic block
 * 2. Can perform replication
 * 3. Can perform register renaming to get around of anti-/output- dependences 
 * 4. Recognizes the distinction between initializations (which can be
 *    speculation) versus stores (which cannot be).
 *
 * Some notes on how the list scheduling algorithm work:
 * 1. (Side)-entries and (side)-exits are cfg edges that come into and out of
 *    the current region.  
 * 2. The region to be scheduled has to be acyclic.  Cyclic edges are cut
 *    arbitrarily (by the region forming combinator.)
 * 3. Every block that has side-entries has an "live-in" node that summaries
 *    all the values that are defined coming in the side-entires.  Similarly,
 *    for all blocks with side-exits we have "live-out" nodes. 
 * 4. During list scheduling, multiple blocks may be "open" at the same time.
 *    Instructions can only be placed within open blocks.
 * 5. Once every instruction (that appears in the block originally) 
 *    has been scheduled, the block is then "closed". 
 * 6. A new block is opened if all its predecessors is closed.
 * 7. "Ready" instructions, i.e. instructions with all its predecessors
 *    schedules are put onto a priority list.  The priority list is ranked
 *    by the execution frequency of the instruction.
 * 8. At each step, an instruction i is chosen from the priority list to 
 *    be scheduled.  This instruction has to be placed at "all" open blocks
 *    that reaches the block where instruction originates.  This may involve
 *    replicating the instruction.  For this transformation to be legal,
 *    structural and profitability checks have  to be performed.
 * 
 *    a.  Structural check determines whether it is semantics preserving 
 *        to put this instruction into the set of open blocks. 
 *    b.  Profitability check determines whether it is profitable, i.e. is
 *        it okay to put this instruction to these blocks or should we delay
 *        it.
 * 
 *    Instructions that fail these criteria are moved into a pending queue.
 * 9. Instructions from the pending queue are released back into the ready 
 *    queue whenever the set of open blocks change. 
 * 10. BUT ... this is not the entire story.  When scheduling dags, the
 *     dependency graph initially built is insufficient to summarize all 
 *     dependences.  For that to work, incremental liveness computation
 *     must also be performed.  This is how it works:
 *     
 *     a.  Each open block keeps track of what registers are live at the
 *         current time.  Liveness can be inferred via the dependence dag
 *
 * -- Allen (leunga@cs.nyu.edu) 6/1/00
 *)
functor ListScheduler
    (structure DDG        : SCHEDULER_DDG
     structure IR         : MLRISC_IR
     structure InsnProps  : INSN_PROPERTIES
     structure FormatInsn : FORMAT_INSTRUCTION
     (* structure Rewrite    : REWRITE_INSTRUCTIONS *)
        sharing DDG.I = InsnProps.I = IR.I = (* = Rewrite.I *)
                FormatInsn.I  
    ) : LIST_SCHEDULER =
struct
 
   structure IR         = IR
   structure CFG        = IR.CFG
   structure DDG        = DDG
   structure I          = DDG.I
   structure SchedProps = DDG.SchedProps
   structure G          = Graph
   structure A          = Array
   structure DA         = DynArray
   structure W8A        = Word8Array
   structure PQ         = PriorityQueue

   fun error msg = MLRiscErrorMsg.error("ListScheduler",msg)

   val debug = true
   val verbose = true
   val safetyCheck = true

   val i2s = Int.toString

   exception NotOpened
   exception NotLive

   val dummyJump = (~1,DDG.NODE{instr=InsnProps.nop(),defs=[],uses=[],b= ~1})

   (* data structure to hold info about a block *)
   datatype openBlock = 
      OPEN_BLOCK of
      {bid           : int,                          (* block id *)
       reachables    : W8A.array,                    (* reachable set *)
       rt            : SchedProps.reservation_table, (* reservation table *)
       sigma         : I.instruction list DA.array,
       liveSet       : DDG.edge G.edge list IntHashTable.hash_table,
       jumpScheduled : bool ref,
       jumpTime      : int ref,
       jumpNode      : DDG.node G.node ref
      } 

   val profitabilityRatio = 0.5

   fun listScheduler{ranking, cpu_info, blockIdTbl, cfg, region, ddg} = 
   let
       (* Extract architecture info from the data base *)
       val SchedProps.CPU_INFO{newTable, findSlot, pipeline, insert, ...} = 
              cpu_info

       (* The data structures:
        * succ, pred   --- adjacency lists
        * blockMap     --- mapping from internal block id -> real block id
        * liveInMap    --- mapping from internal block id -> live in node
        * liveOutMap   --- mapping from internal block id -> live out node
        * issueTimeTbl --- node id --> its issue time
        * inDegsTbl    --- node id --> its current in-degree
        * insnCountTbl --- internal block id --> number of unscheduled instrs
        *)
       val DDG as G.GRAPH ddg = ddg
       val CFG as G.GRAPH cfg = cfg
       val Region as G.GRAPH region = region
       val {succ, pred, ...} = DDG.internalInfo DDG
       val SOME{blockMap, liveInMap, liveOutMap, ...} = !(DDG.globalInfo DDG)
       val N = #capacity ddg () (* number of instructions *)
       val M = #order region () (* number of blocks in the region *)

       (* Internal tables indexed by instruction id *)
       val issueTimeTbl = A.array(N,~1) (* issue times of instructions *)
       val inDegsTbl    = A.array(N,0) (* in-degree of a node *)

       (* Internal tables indexed by block id *)
       val insnCountTbl = A.array(M, 0) (* number of instructions per block *)  
       val freqTbl      = A.array(M, 0) (* execution frequency of blocks *)
       val predCountTbl = A.array(M, 0) (* in degree of blocks *)
       val rtTbl        = A.array(M,newTable 0)
       val startTimeTbl = A.array(M, 0)
       val maxTimeTbl   = A.array(M, 0)
       val isLegalTbl   = A.array(M, 0) (* is it legal to schedule 
                                             block id at this time *)
       val isProfitableTbl = A.array(M, 0)
       val profitabilityTbl = A.array(M, 0.0) (* priority of block *)
       val liveSetTbl   = A.array(M, IntHashTable.mkTable(0, NotLive))

       val stampCounter = ref 0
       fun newStamp() = 
       let val st = !stampCounter + 1
       in  stampCounter := st; st end

       (* Linearize the schedule *)
       fun linearize sigma = 
           DA.foldl (fn (instrs,l) => instrs @ l) [] sigma

       (* It is okay to move an instruction from block id *) 
       fun isLegalMove id = A.sub(isLegalTbl, id) = !stampCounter
       fun isProfitableMove id = A.sub(isProfitableTbl, id) = !stampCounter

       val showInsn = FormatInsn.toString [] (I.C.lookup (CFG.regmap CFG))
       fun showOp(DDG.NODE{instr,b,...}) = 
              showInsn instr^" ["^i2s(A.sub(blockMap,b))^"]"

       fun isJump instr =
           case InsnProps.instrKind instr of
             InsnProps.IK_JUMP => true
           | _ => false

       (* Real priority function *)
       fun priorityFun(I as (i,DDG.NODE{b=b_i,...}), 
                       J as (j,DDG.NODE{b=b_j,...})) = 
           let val p_i = A.sub(profitabilityTbl,b_i)
               val p_j = A.sub(profitabilityTbl,b_j)
           in  case Real.compare(p_i,p_j) of
                 EQUAL => ranking(I,J)
               | GREATER => true
               | LESS => false
           end

 
       (* Initialization steps:
        * 1. Initialize the frequency array
        * 2. Count the number of predecessors of each block in the region
        * 3. Count the number of non-special instructions
        * 4. Initialize the pending queue
        *)
       fun initialize() =
       let (* Initialize the frequencies *) 
            val _ = 
              A.appi (fn (id,b) =>
                      let val CFG.BLOCK{freq, ...} = #node_info region b
                      in  A.update(freqTbl, id, !freq);
                          A.update(predCountTbl, id, length(#in_edges region b))
                      end) (blockMap, 0, NONE)
           val pendingNodes = 
               foldr 
                (fn ((i,i'),pending) =>
                 let val inEdges = #in_edges ddg i
                     val n       = length inEdges
                     val DDG.NODE{b, instr, ...} = i'
                     fun addToPending() =
                        if n = 0 
                        then (i, i')::pending
                        else (A.update(inDegsTbl, i, n); pending)
                 in  case InsnProps.instrKind instr of
                       InsnProps.IK_SINK   => pending
                     | InsnProps.IK_SOURCE => pending
                     | _ =>
                       (A.update(insnCountTbl, b, A.sub(insnCountTbl, b) + 1);
                        addToPending()
                       )
                 end
                ) [] (#nodes ddg ())
       in  pendingNodes
       end

       (* Queues *)
       val readyQueue = PQ.create priorityFun 
       val enqueue    = PQ.insert readyQueue
       val pending    = ref(initialize())
       (*
       val enqueue    = if debug then 
           (fn (i,i') => (print("QUEUEING "^showOp i'^"\n"); enqueue (i,i'))) 
           else enqueue
        *)

       (* === Incremental liveness computation routines === *) 

       (* 
        * Add an instruction into the current live set of block bid.
        *)
       fun addInstrToLiveSet(i, i' as DDG.NODE{defs, uses, ...}, liveSet) =
       let val lookupLiveSet = IntHashTable.find liveSet
           val lookupLiveSet = fn b => case lookupLiveSet b of SOME x => x
                                                             | NONE => []
           val updateLiveSet = IntHashTable.insert liveSet

           fun rmvUse r = 
           let fun loop([], es') = es'
                 | loop((e as (j,k,_))::es, es') =
                   if i = k then loop(es, es') else loop(es, e::es')
               val es = lookupLiveSet r
               val es = loop(es, [])
           in  updateLiveSet(r, es) end

           fun rmvUses [] = ()
             | rmvUses(r::uses) = (rmvUse r; rmvUses uses)

           fun addDef(r, e) = updateLiveSet(r, e::lookupLiveSet r)

           fun addDefs [] = ()
             | addDefs((edge as (i,j,e as DDG.EDGE{r,d,...}))::es) = 
               ((* print(i2s i^" -> "^i2s j^" "^DDG.edgeToString e^"\n"); *)
                if r >= 0 then addDef(r, edge) else (); 
                addDefs es
               )
 
       in  rmvUses uses; 
           addDefs (A.sub(succ, i))
       end

       (* 
        * Check whether it is a legal code motion to move an instruction i
        * from block "from" to block "to".  Instruction i must have no
        * unscheduled predecessors at this point.
        *)
       fun isIllegalCodeMotion(i, i' as DDG.NODE{defs, ...}, liveSet) = 
       let (* Check whether instruction i defines a register r 
            * that is currently live.  If so, the associated code motion is
            * illegal (without renaming)
            *)
           val lookupLiveSet = IntHashTable.find liveSet
           val lookupLiveSet = fn b => case lookupLiveSet b of SOME x => x
                                                             | NONE => []
            (*
             * Add an output- dependence edge between two nodes
             *)
           fun addOutputDepEdge(i,j,r) = 
               (#add_edge ddg (i,j, DDG.EDGE{l= ~1, d=DDG.OUTPUT, r=r});
                A.update(inDegsTbl, j, A.sub(inDegsTbl, j) + 1)
               )

           fun isLiveReg r =
               let fun loop [] = false
                     | loop((j,k:int,e)::es) = 
                        if i = k then loop es else 
                        (if debug then 
                           print("BAD: "^i2s j^" -> "^i2s k^" "^
                                 DDG.edgeToString e^
                                 " "^showOp(#node_info ddg j)^" -> "^
                                 " "^showOp(#node_info ddg k)^"\n"
                                )
                         else ();
                         true
                        )
                     (* if i = k then i is the use of r so it doesn't count *)
               in  loop(lookupLiveSet r) 
               end
           fun canKillLiveValues [] = false
             | canKillLiveValues((r,_)::defs) = 
               isLiveReg r orelse canKillLiveValues defs  
       in  canKillLiveValues defs 
       end

       (* Find out the time slot to insert the instruction j in 
        * reservation table rt (from block id) 
        *)
       fun findScheduleSlot(bid, rt, p, j, j') =
       let fun earliest([], t) = t
             | earliest((i,j,e as DDG.EDGE{l,...})::es, t) =
               let val t' = A.sub(issueTimeTbl,i)
                   val t'' = t' + l + 1
               in  (* if debug then
                     print(i2s i^" -> "^i2s j^" "^DDG.edgeToString e^
                           " t'="^i2s t'^" t''="^i2s t''^"\n")
                   else (); *)
                   earliest(es, Int.max(t, t''))
               end
           val t_min = earliest(A.sub(pred, j), A.sub(startTimeTbl, bid)) 
       in  findSlot(rt, t_min, p) 
       end

       (* Release an instruction when all its predecessors 
        * have been scheduled. Note that fake sink and source instructions
        * must be treated specially and so we don't release them onto the queue.
        *)
       fun releaseInstr j = 
       let val j' as DDG.NODE{instr,b,...} = #node_info ddg j
       in  case InsnProps.instrKind instr of 
              InsnProps.IK_SOURCE => ()
           |  _ => if isProfitableMove b then enqueue(j,j') 
                   else pending := (j,j') :: !pending 
       end

       (* Release the successors of an instruction 
        * after it has been scheduled 
        *)
       fun updateSucc(i) =
       let fun loop [] = ()
             | loop((i,j,_)::es) = 
               let val n = A.sub(inDegsTbl, j)
               in  A.update(inDegsTbl, j, n-1);
                   if n = 1 then releaseInstr j else ();
                   loop es
               end
       in  loop(A.sub(succ, i)) 
       end

       (* Release the live-in node for block id *)
       fun releaseLiveIn(bid,liveSet) =
       let val liveInNode as (j,j') = IntHashTable.lookup liveInMap bid
       in  if A.sub(issueTimeTbl, j) < 0 then
              (addInstrToLiveSet(j,j',liveSet);
               A.update(issueTimeTbl, j, 0);
               updateSucc j;
               if debug then print("LIVEIN "^showOp j'^"\n") else ()
              )
           else ()
       end handle _ => () (* no live-in node, so don't bother *)

       (* Release the live-out node for block id *)
       fun releaseLiveOut(bid, liveSet) = 
       let val liveOutNode as (j,j' as DDG.NODE{instr,...}) = 
               IntHashTable.lookup liveOutMap bid
       in  case InsnProps.instrKind instr of 
              InsnProps.IK_SINK =>
              (addInstrToLiveSet(j,j',liveSet);
               A.update(issueTimeTbl, j, 0);
               updateSucc j;
               if debug then print("LIVEOUT "^showOp j'^"\n") else ()
              )
           |  _ => error("releaseLiveOut "^showOp j')
       end handle _ => () (* no live-out node, so don't bother *)
 
       fun printOpenBlocks blocks =
           "[ "^
           foldr (fn (OPEN_BLOCK{bid,...},l) => 
                   i2s(A.sub(blockMap,bid))^" "^l) "" blocks
           ^"]"

       (* Move legal pending nodes from the pending queue and the ready queue
        * to the ready queue. 
        *)
       fun moveLegalPendingToReady() =
       let fun scan([], pending) = pending
             | scan((node as (j,DDG.NODE{b,...}))::nodes, pending) = 
               if isProfitableMove b then 
                  (enqueue node; scan(nodes, pending))
               else 
                  scan(nodes, node::pending)
           val waiting = List.revAppend(PQ.toList readyQueue, !pending)
       in  PQ.clear readyQueue;
           pending := scan(waiting, [])
       end


       (* Given a set of openBlocks, compute the set of legal blocks
        * and profitable blocks that can be scheduled at the current time.
        * At this point, we also compute the profitability of moving 
        * an instruction from bid to the openBlockList.
        * Move instructions from pending queue to the priority queue.
        *)
       fun updatePermittableCodeMotion openBlockList =
       let val stamp = newStamp()

           (* What is the cost of moving an instruction from block source to 
            * the blocks in openBlockList?
            *)
           fun codeMotionCost(source) = 
           let fun loop([], C) = C
                 | loop(OPEN_BLOCK{reachables, bid=target, ...}::L, C) = 
                   if W8A.sub(reachables, source) = 0w0 then loop(L, C)
                   else let val freq = A.sub(freqTbl, target)
                        in  loop(L, C+freq) end
           in  loop(openBlockList, 0) 
           end

           (* Check whether it is profitable to move an instruction from
            * block source.  1.0 means non-speculative.  < 1.0 means
            * speculative
            *)
           fun isProfitable(source) =
           let val origCost = A.sub(freqTbl, source)
               val moveCost = codeMotionCost(source) 
               val profitability = real origCost / real moveCost
           in  A.update(profitabilityTbl,source,profitability); 
               profitability >= profitabilityRatio 
           end

           fun markLegal([]) = ()
             | markLegal(bid::Xs) =  
               if A.sub(isLegalTbl, bid) = stamp then markLegal Xs else 
               (A.update(isLegalTbl, bid, stamp);
                if debug then print(i2s(A.sub(blockMap,bid))) else ();
                if isProfitable bid then 
                   (if debug then print "+" else ();
                    A.update(isProfitableTbl, bid, stamp) 
                   )
                else ();
                if debug then print " " else (); 
                markLegal
                   (markSucc(#out_edges region (A.sub(blockMap, bid)), Xs)) 
               )
           and markSucc([], Xs) = Xs
             | markSucc((_,Y,_)::es, Xs) = 
               if predAllLegal Y then markSucc(es, A.sub(blockIdTbl,Y)::Xs)
               else markSucc(es, Xs) 

           and predAllLegal X = 
               let fun loop [] = true
                     | loop((Y,_,_)::es) = 
                       A.sub(isLegalTbl, A.sub(blockIdTbl, Y)) = stamp
                       andalso loop es
               in  (* IMPORTANT: prevent hoisting past side entries! *)
                   case #entry_edges region X of
                     [] => loop(#in_edges region X) 
                   | _ => false 
               end
       in  if debug then print("LEGAL: ") else ();
           markLegal(map (fn OPEN_BLOCK{bid,...} => bid) openBlockList);
           if debug then print("\n") else ();
           moveLegalPendingToReady(); 
           openBlockList
       end

       (* Open a new block b. 
        * Mark all blocks that b reaches.
        *)
       fun openBlock(b, openBlockList) = 
       let val bid = A.sub(blockIdTbl, b)
       in  if A.sub(isLegalTbl, bid) < 0 then (* closed permenantly! *)
              openBlockList
           else openBlock'(bid, b, openBlockList)
       end

       and openBlock'(bid, b, openBlockList) = 
       let val reachables = W8A.array(M,0w0)
           fun markReachables b = 
           let val bid = A.sub(blockIdTbl, b)
           in  if W8A.sub(reachables, bid) = 0w0 then
                   (W8A.update(reachables, bid, 0w1);
                    app markReachables (#succ region b)
                   )
               else ()
           end
           val _  = markReachables b
           fun mergeIncomingBlocks() =
           let val liveSet = IntHashTable.mkTable(32,NotLive)
               val lookupLiveSet = IntHashTable.find liveSet
               val lookupLiveSet = 
                   fn b => case lookupLiveSet b of SOME x => x | NONE => []
               val addLiveSet = IntHashTable.insert liveSet
               fun merge([], NONE) = (newTable 5, 0)
                 | merge([], SOME(_,t,rt)) = (rt, t+1)
                 | merge((Y,_,CFG.EDGE{w,...})::es,rt) = 
                   let val Y_id      = A.sub(blockIdTbl, Y) 
                       val liveSet_Y = A.sub(liveSetTbl, Y_id)
                       val rt = 
                           case rt of 
                             NONE => SOME(!w,
                                          A.sub(maxTimeTbl,Y_id),
                                          A.sub(rtTbl,Y_id))
                           | SOME(w',_,rt') => 
                              if !w > w' then 
                                     SOME(!w,
                                          A.sub(maxTimeTbl,Y_id),
                                          A.sub(rtTbl,Y_id))
                              else rt
                   in  IntHashTable.appi (fn (r,es) => 
                          addLiveSet(r, List.revAppend(es, lookupLiveSet r)))
                          liveSet_Y;
                       merge(es, rt)
                   end
               val (rt, startTime) = merge (#in_edges region b, NONE)
           in  A.update(rtTbl, bid, rt);
               A.update(startTimeTbl, bid, startTime);
               A.update(liveSetTbl, bid, liveSet);
               (liveSet, rt)
           end
           val _  = if debug then 
                      print("OPENING "^i2s b^" "^printOpenBlocks openBlockList^
                            "("^i2s(A.sub(insnCountTbl,bid))^" insns)\n") 
                    else ();
           val (liveSet,rt) = mergeIncomingBlocks()
           (* release live-in anchor of block b *)
           val _ = releaseLiveIn(bid, liveSet)
           val openBlock = 
                OPEN_BLOCK{bid=bid, rt=rt, 
                           reachables=reachables,
                           liveSet=liveSet,
                           jumpScheduled=ref false,
                           sigma=DA.array(5,[]),
                           jumpTime=ref 10000000,
                           jumpNode=ref dummyJump
                          }
           val openBlockList = 
                updatePermittableCodeMotion(openBlock::openBlockList)
       in  if A.sub(insnCountTbl, bid) = 0 then
               closeBlock(bid, openBlockList)
           else
               openBlockList
       end

       (* Close a block *)
       and closeBlock(bid, openBlockList) = 
       let fun rmv((x as OPEN_BLOCK{bid=bid',rt,jumpScheduled,jumpTime,liveSet,
                                    jumpNode=ref(j,j'),sigma,...})::L, L') = 
               if bid = bid' then 
                  let val b = A.sub(blockMap, bid)
                      val CFG.BLOCK{insns, ...} = #node_info region b
                      val instrs = linearize sigma
                      val instrs = if !jumpScheduled then
                                     let val DDG.NODE{instr=jmp,...} = j'
                                     in  addInstrToLiveSet(j, j', liveSet);
                                         jmp :: instrs 
                                     end
                                   else instrs
                      val _ = insns := instrs;
                      (* release live-in anchor of block id if it hasn't already
                         been released *)
                      val _ = releaseLiveIn(bid, liveSet);
                      (* release live-out anchor of block id *)
                      val _ = releaseLiveOut(bid, liveSet); 
                      val n = A.sub(insnCountTbl, bid) 
                  in  if n > 0 then
                         print("WARNING block "^i2s b^" has "^i2s n^
                               " instruction(s) left over\n")
                      else ();
                      List.revAppend(L',L)
                  end
               else rmv(L, x::L')
             | rmv([], _) = raise NotOpened (* not found, it's okay *)
           fun decCounts([], openBlockList) = openBlockList
             | decCounts((_,Y,_)::es, openBlockList) = 
               let val bid_Y =  A.sub(blockIdTbl, Y)
                   val n = A.sub(predCountTbl, bid_Y) - 1
               in  A.update(predCountTbl, bid_Y, n);
                   if n = 0 then decCounts(es, openBlock(Y, openBlockList))
                   else decCounts(es, openBlockList)
               end
           val openBlockList = rmv(openBlockList, [])
           val _ = if debug then 
                     print("CLOSING "^i2s(A.sub(blockMap, bid))^" "^
                           printOpenBlocks openBlockList^"\n")
                   else ()
           val out_edges = #out_edges region (A.sub(blockMap, bid))

           val openBlockList = decCounts(out_edges, openBlockList)   
       in (* mark this block as closed forever *)
           A.update(isLegalTbl, bid, ~1);
           updatePermittableCodeMotion openBlockList 
       end handle NotOpened => openBlockList

       (* Close all blocks that have jump instruction scheduled *)
       fun closeAllJumpedBlocks openBlockList =
       let fun loop([], L') = L'
             | loop((B as OPEN_BLOCK{bid, jumpScheduled,...})::L, L') =
               if !jumpScheduled then loop(L, closeBlock(bid, B::L'))
               else loop(L, B::L')
       in  loop(openBlockList, []) end

       (* Schedule an instruction: 
        * Given an instruction and a set of openBlocks, find out where 
        * the instruction has to be inserted at. 
        *)
       fun scheduleInstr(openBlockList, j, j' as DDG.NODE{instr,b,...}) = 
       let val isJump = isJump instr 
           (* val blockName = #create MLRiscAnnotations.COMMENT
                                 (i2s(A.sub(blockMap, bid))) *)

           val pipeline = pipeline instr

           (* Pass one: find out where to perform the code motion 
            * and whether it is legal 
            *)
           fun pass1([], insertionPoints) = pass2(insertionPoints, 0)
             | pass1((B as OPEN_BLOCK{rt,bid,reachables,liveSet,jumpTime,...})::
                     openBlocks, insertionPoints) = 
               if W8A.sub(reachables, b) = 0w0 (* unreachable! *) 
               then pass1(openBlocks, insertionPoints)
               else if bid <> b andalso 
                       isIllegalCodeMotion(j, j', liveSet) then
                        (* this is illegal; put instruction back to the
                         * pending queue.
                         *)
                    (if debug then print("ILLEGAL "^showOp j'^"\n") else ();
                     pending := (j,j') :: !pending;
                     openBlockList
                    )
               else let val time = findScheduleSlot(bid, rt, pipeline, j, j')
                    in  if time > !jumpTime then 
                          (* Can't schedule this instruction because
                           * it must follow the jump instruction! 
                           * Close this block instead.
                           *)
                         (pending := (j,j') :: !pending;
                          closeBlock(bid, openBlockList)
                         )
                        else
                         pass1(openBlocks, (time, B)::insertionPoints)
                    end
                          
                   
           (* Pass two: perform the actual insertion *)
           and pass2([], replicationCount) = finish()
             | pass2((time, 
                      OPEN_BLOCK{bid, rt, reachables, liveSet, sigma, 
                                 jumpScheduled, jumpTime, jumpNode, ...})::
                     insertionPoints, replicationCount) = 
                  (* a copy of instruction j has to be placed in reservation
                   * table rt.
                   *)
                   let val instr = if replicationCount > 0 then 
                                   InsnProps.replicate instr else instr
                       (* val instr = if bid <> b then 
                                      InsnProps.annotate(instr,blockName) 
                                   else instr *)
                   in  insert(rt, time, pipeline);
                       A.update(issueTimeTbl, j, 
                                Int.max(time, A.sub(issueTimeTbl, j)));
                       A.update(maxTimeTbl, bid, 
                                Int.max(time, A.sub(maxTimeTbl, bid)));
                       if debug andalso (verbose orelse b <> bid) then
                         print(
                           "Time "^i2s time^
                           (if replicationCount > 0 then 
                              " ("^i2s replicationCount^")"
                            else "")^
                           " "^showInsn instr^
                           " ["^i2s(A.sub(blockMap,b))^
                           "] scheduled in block "^i2s(A.sub(blockMap,bid))^
                           (if b <> bid then " ***" else "")^
                           (if !jumpScheduled then "!!!" else "")^
                           "\n")
                       else ();
                       (* Jump processing *) 
                       if isJump then
                          (jumpScheduled := true; 
                           jumpTime := time;
                           jumpNode := (j, j')
                          )
                       else 
                          (addInstrToLiveSet(j, j', liveSet);
                           DA.update(sigma, time, instr::DA.sub(sigma, time))
                          );
                       pass2(insertionPoints, replicationCount+1)
                   end

           (* Do these things after successfully scheduled an instruction *) 
           and finish() = 
           let val _ = updateSucc j;
               val n = A.sub(insnCountTbl, b) - 1
           in  A.update(insnCountTbl, b, n);
               (* if we have run out instructions or else 
                * we have scheduled the jump instruction, we can 
                * close the current block.  
                * At this point we wait until we can't find any instructions 
                * that be scheduled ahead of the current jump instruction. 
                *)
               if isJump then openBlockList
               else if n = 0 then closeBlock(b, openBlockList) 
               else openBlockList
           end

       in  pass1(openBlockList, [])
       end

       (* Main loop *)
       fun schedule(openBlockList) =  
       if PQ.isEmpty readyQueue then
          let val L = closeAllJumpedBlocks openBlockList 
              val L = updatePermittableCodeMotion L
          in  case L of
                [] => ()
              | _  => schedule L 
          end
       else
          let val (j, j' as DDG.NODE{b,...}) = PQ.deleteMin readyQueue
              val openBlockList = scheduleInstr(openBlockList,j,j')
          in  schedule openBlockList
          end

       fun scheduleAll() = 
       let val entries = #entries region ()
           (* find blocks without predecessors in the region *)
           val openBlockList = foldr 
                  (fn (b,L) => if A.sub(predCountTbl, A.sub(blockIdTbl, b)) = 0 
                               then openBlock(b,L) else L
                  ) [] entries
       in  case openBlockList of
             [] => error "cyclic region"  
           | _  => schedule(updatePermittableCodeMotion openBlockList) 
       end

       fun sanityCheck() =
       let val ok = ref true
       in  #forall_nodes ddg 
               (fn (i,i') =>
                  if A.sub(issueTimeTbl,i) < 0 then
                    (print("UNSCHEDULED "^showOp i'^
                          " |pred|="^i2s(A.sub(inDegsTbl, i))^"\n");
                     app (fn (j,i,e) =>
                          if A.sub(issueTimeTbl,j) < 0 then
                             (print("\t"^i2s j^" -> "^i2s i^" "^
                                    DDG.edgeToString e);
                              print("\t"^showOp(#node_info ddg j)^"\n")
                             )
                          else ()) (#in_edges ddg i);
                     print "\n";
                     ok := false
                    )
                  else ()
               );
           if !ok then () else error "Scheduling error"
       end

   in  (* #forall_edges ddg (fn (i,j,e) =>
          print(showOp(#node_info ddg i)^" -> "^showOp(#node_info ddg j)^" "^
                DDG.edgeToString e^"\n")); *)
       scheduleAll();
       if safetyCheck then sanityCheck() else ()
   end 

end
