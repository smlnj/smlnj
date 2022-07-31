(*
 *  Partition a cluster into multiple smaller clusters for region-based
 *  register allocation.
 *)
functor ClusterPartitioner
  (structure Flowgraph : FLOWGRAPH
   structure InsnProps : INSN_PROPERTIES
     sharing Flowgraph.I = InsnProps.I
  ) : RA_FLOWGRAPH_PARTITIONER = 
struct
   structure F        = Flowgraph
   structure I        = F.I
   structure C        = I.C
   structure PQ       = PriorityQueue
   structure Liveness = Liveness(Flowgraph)
   structure A        = Array

   type flowgraph = F.cluster

   val debug = true

   fun error msg = MLRiscErrorMsg.error("ClusterPartitioner",msg)

   val maxSize = MLRiscControl.getInt "ra-max-region-size"
   val _       = maxSize := 300

   fun numberOfBlocks(F.CLUSTER{blkCounter,...}) = !blkCounter

   (* 
    * Partition the cluster into a set of clusters so that each can
    * be allocated independently.
    *)
   fun partition(F.CLUSTER{blkCounter, blocks, entry, exit, 
                           annotations, ...}) 
        cellkind processRegion = 
       (* Number of basic blocks *)
   let val N = !blkCounter

       val _ = if debug then 
                  print("[Region based register allocation: "^
                        Int.toString N^"]\n") 
               else ()
       val maxSize = !maxSize

       (* Perform global liveness analysis first.
        * Unfortunately, I know of no way of avoiding this step because
        * we have to know which values are live across regions. 
        *)
       val _ = Liveness.liveness{blocks=blocks,
                                 defUse=InsnProps.defUse cellkind,
                                 getCell=C.getCellsByKind cellkind,
                                 updateCell=C.updateCellsByKind cellkind
                                }

       val F.ENTRY{succ=entrySucc, ...} = entry
       val F.EXIT{pred=exitPred, ...} = exit
       val initTrail = [(entrySucc,!entrySucc), (exitPred, !exitPred)]

       (* Priority queue of basic blocks in non-increasing order 
        * of execution frequency  
        *)
       fun higherFreq(F.BBLOCK{freq=a,...}, F.BBLOCK{freq=b,...}) = !a > !b
         | higherFreq _ = error "higherFreq"
       val blocks    = List.foldr (fn (b as F.BBLOCK _,l) => b::l | (_,l) => l)
                           [] blocks
       val seedQueue = PQ.fromList higherFreq blocks

       (* Current region id *)
       val regionCounter = ref 0
       fun newRegionId() =
       let val regionId = !regionCounter 
       in  regionCounter := !regionCounter + 1; regionId end
           
       (* Has the block been included in any region? 
        * Non-negative means yes.  The number is the region id in which
        * the block belongs.
        *) 
       val processed = A.array(N, ~1)

       fun hasBeenProcessed n = A.sub(processed,n) >= 0 
       fun markAsProcessed(n, regionId) = A.update(processed,n,regionId)

       (* Get an unprocessed seed block from the queue *)
       fun getSeedBlock(regionId) =
           case PQ.deleteMin seedQueue of
             block as F.BBLOCK{blknum, insns, ...} =>
               if hasBeenProcessed blknum then getSeedBlock(regionId)
               else block
           | _ => error "getSeedBlock"

       fun resetTrail [] = ()
         | resetTrail((r,x)::trail) = (r := x; resetTrail trail)

       (*
        * Grow a region.  Currently, region growth is limited only by size.
        * Note that we only select nodes with one out edges as possible
        * region cut points.   We also try not to make a region too small
        * as it will waste initialization time.  It's a delicate balance.
        *)
       fun growRegion() =
       let val regionId = newRegionId()
           fun add([], Q) = Q
             | add((b as F.BBLOCK{blknum, ...},_)::bs, Q) = 
                  if hasBeenProcessed blknum then add(bs, Q) 
                  else add(bs, b::Q)
             | add(_::bs, Q) = add(bs, Q)
           fun grow((b as F.BBLOCK{blknum, succ, pred, insns, ...})::F, B,
                    size, blks, m) = 
               if hasBeenProcessed blknum 
               then grow(F, B, size, blks, m)
               else
               let val n = length(!insns)
                   val newSize = size + n
               in  if m > 0 andalso newSize > maxSize andalso length(!succ) = 1
                   then grow(F, B, size, blks, m) 
                   else (markAsProcessed(blknum, regionId);
                         grow(F, add(!pred,add(!succ,B)), newSize, 
                              b::blks, m+1)
                        )
               end
             | grow([], [], size, blks, m) = (size, blks, m)
             | grow([], B, size, blks, m) = grow(rev B, [], size, blks, m)
             | grow _ = error "grow"

           (* Find a seed block *)
           val seed = getSeedBlock(regionId)

           (* Grow until we reach some limit *)
           val (totalSize, blocks, blockCount) = grow([seed], [], 0, [], 0)

           (* Now create a cluster with only these blocks 
            * We have to update the edges so that region-entry edges
            * are made into entry edges and region-exit edges are
            * made into exit edges.  
            *)
           fun makeSubgraph(blocks) =
           let fun inSubgraph(y) = A.sub(processed,y) = regionId
               fun processSucc(b,x,(e as (F.BBLOCK{blknum=y, ...},freq))::es, 
                                 es', exit, exitFreq) = 
                    if inSubgraph(y) then 
                         processSucc(b,x,es,e::es',exit,exitFreq) 
                    else processSucc(b,x,es,es',true, exitFreq + !freq) 
                 | processSucc(b,x,(e as (F.EXIT{blknum=y,...},freq))::es,es',
                               exit, exitFreq) = 
                    processSucc(b,x,es,es', true, exitFreq + !freq) 
                 | processSucc(b,x,[],es',true, exitFreq) = 
                    let val w = ref exitFreq
                    in  exitPred := (b,w) :: !exitPred;
                        ((exit,w)::es', true)
                    end
                 | processSucc(b,x,[],es', false, exitFreq) = (es', false)
                 | processSucc _ = error "processSucc"

               fun processPred(b,x,(e as (F.BBLOCK{blknum=y, ...},freq))::es,
                                 es', entry, entryFreq) = 
                    if inSubgraph(y) then 
                         processPred(b,x,es,e::es',entry,entryFreq)
                    else processPred(b,x,es,es',true,entryFreq + !freq) 
                 | processPred(b,x,(e as (F.ENTRY{blknum=y,...},freq))::es,es',
                               entry, entryFreq) = 
                    processPred(b,x,es,es',true, entryFreq + !freq) 
                 | processPred(b,x,[], es', true, entryFreq) = 
                    let val w = ref entryFreq
                    in  entrySucc := (b,w) :: !entrySucc;
                        ((entry,w)::es', true)
                    end
                 | processPred(b,x,[], es', false, entryFreq) = (es', false)
                 | processPred _ = error "processPred"

               fun processNodes([], trail) = trail
                 | processNodes(
                     (b as F.BBLOCK{blknum=n,liveIn,liveOut,succ,pred,...})
                       ::nodes, trail) =
                   let val (succ', exit) = processSucc(b,n,!succ,[],false,0)
                       val trail = if exit then (succ, !succ)::trail else trail
                       val (pred', entry) = processPred(b,n,!pred,[],false,0)
                       val trail = if entry then (pred, !pred)::trail else trail
                   in  succ := succ';
                       pred := pred';
                       (* To save space, clear liveIn and 
                        * liveOut information (if it is not an exit)
                        *)
                       liveIn := CellsBasis.CellSet.empty;
                       if exit then () else liveOut := CellsBasis.CellSet.empty;
                       processNodes(nodes, trail)
                   end
                 | processNodes _ = error "processNodes"

               val _     = entrySucc := []
               val _     = exitPred := []
               val trail = processNodes(blocks, initTrail)
           in  trail
           end

           (* Make a subgraph with the appropriate edges *)
           val trail = makeSubgraph(blocks)

           val region = 
               F.CLUSTER{blkCounter  = blkCounter,
                         blocks      = blocks,
                         entry       = entry,
                         exit        = exit,
                         annotations = annotations
                        }
       in  (regionId, region, trail, blockCount)
       end

       (*
        * Extract a new region to compile.  Raises PQ.EmptyPriorityQueue if
        * everything is finished.
        *)
       fun iterate() = 
       let val (id, region, trail, blockCount) = growRegion() (* get a region *)
       in  if debug then
              print("[Region "^Int.toString id^" has "^Int.toString blockCount^
                    " blocks]\n")
           else ();
           processRegion region; (* allocate this region *)
           resetTrail trail;     (* reset the flowgraph *)
           iterate()             (* process next region *)
       end

   in  (* Repeat until the entire flowgraph has been processed *)
       iterate() handle PQ.EmptyPriorityQueue => ();
       if debug then print "[Region based register allocation done]\n" else ()
   end

end
