(*
 * Partition the IR into regions according to partition criteria
 * and frequencies.  Then feed the regions into the instruction
 * scheduler.
 *
 * The partitional criteria can be:
 *   1. The maximum number of blocks
 *   2. The maximum number of instructions
 *)

functor RegionBuilder(IR : MLRISC_IR) : REGION_BUILDER =
struct
   structure IR   = IR
   structure CFG  = IR.CFG
   structure Util = IR.Util
   structure G    = Graph
   structure A    = Array
   structure PQ   = PriorityQueue
   structure DA   = DynArray

   fun error msg = MLRiscErrorMsg.error("RegionBuilder",msg)

   val view_IR = MLRiscControl.getFlag "view-IR"

   val i2s = Int.toString
 
   fun regionBuilder {maxBlocks, maxInstrs, sideEntries, minFreqRatio,
                      traceOnly, internalBackEdges, insertDummyBlocks
                     }  (IR as G.GRAPH cfg) schedule =
   let val N = #capacity cfg ()
       val G.GRAPH loop = IR.loop IR 

       (* Note: tables must be dynamic because the cfg may be changed 
        * while scheduling is being performed
        *)
       val processed = DA.array(N, false)
       fun isProcessed i = DA.sub(processed, i) 
       fun markAsProcessed i = DA.update(processed, i, true)
       val blockIdTbl = DA.array(N, 0)

       (* A queue of all the blocks ranked by priority 
        * Give loop headers extra priority
        *)
       fun freqOf(CFG.BLOCK{freq,...}) = !freq 
       fun highestFreqFirst((i,i'),(j,j')) = 
           let val f_i = freqOf i'
               val f_j = freqOf j'
           in  if f_i = f_j then #has_node loop i  
               else f_i > f_j 
           end
       val seeds = PQ.fromList highestFreqFirst (#nodes cfg ())

       (* Initialization *)
       fun initialization() =
           (app markAsProcessed (#entries cfg ());
            app markAsProcessed (#exits cfg ())
           )

       (* Locate an unprocessed seed block; raises exception if everything
        * is done.
        *)
       fun newSeed() =
       let val (i,i') = PQ.deleteMin seeds
       in  if isProcessed i then newSeed() 
           else if freqOf i' = 0 then raise PQ.EmptyPriorityQueue
           else (i,i')
       end

       (* Grow a region according to the various parameters *) 
       fun grow(seed as (s,s')) = 
       let val freq    = real(freqOf s')
           val minFreq = freq * minFreqRatio

           (* Remove non candidates *)
           fun prune(j,j') = isProcessed j orelse real(freqOf j') < minFreq 

           fun pruneEdge(w) = real(!w) < minFreq

           fun followSucc([], blocks) = blocks
             | followSucc((_,j,CFG.EDGE{w,...})::es, blocks) =
               let val j' = #node_info cfg j
               in  if pruneEdge w orelse prune(j,j') then followSucc(es, blocks)
                   else followSucc(es, (j,j')::blocks)
               end

           fun followPred([], blocks) = blocks
             | followPred((j,_,CFG.EDGE{w,...})::es, blocks) =
               let val j' = #node_info cfg j
               in  if pruneEdge w orelse prune(j,j') then followPred(es, blocks)
                   else followPred(es, (j,j')::blocks)
               end


           val queue   = PQ.fromList highestFreqFirst [seed]
           val enqueue = PQ.insert queue

           fun chooseBest [] = []
             | chooseBest ((j,j')::rest) = 
               let val w = freqOf j'
                   fun find([],j,j',w) = [(j,j')]
                     | find((k,k')::rest, j, j', w) = 
                       let val w' = freqOf k'
                       in  if w' > w then find(rest, k, k', w') 
                           else find(rest, j, j', w)
                       end
               in  find(rest, j, j', w) end

           fun add([], blocks, blockCount) = (blocks, blockCount)
             | add((j,j')::rest, blocks, blockCount) = 
               if isProcessed j then add(rest, blocks, blockCount) 
               else (markAsProcessed j; 
                     enqueue (j,j'); 
                     add(rest, j::blocks, blockCount+1)
                    )

           (* Find the region using best first search *)
           fun collect(front, back, blockCount) =
           if PQ.isEmpty queue orelse blockCount >= maxBlocks then 
               front @ rev back 
           else
           let val node as (j,j') = PQ.deleteMin queue
               val succs  = followSucc(#out_edges cfg j, [])
               val succs  = if traceOnly then chooseBest succs else succs
               val (back, blockCount) = add(succs, back, blockCount)
               (* val preds  = followPred(#in_edges cfg j, [])
               val preds  = if traceOnly then chooseBest preds else preds 
               val (front, blockCount) = add(preds, front, blockCount) *)
           in  collect(front, back, blockCount)
           end
           
           val _ = markAsProcessed s (* mark the seed block as processed *)
           val blocks = collect([s], [], 1)
           (* The blocks collected are not in linear order *)
       in  blocks
       end

       (* Create a new subgraph from the blocks *)
       fun makeSubgraph blocks =
           if traceOnly then TraceView.trace_view blocks IR
           else AcyclicSubgraphView.acyclic_view blocks IR

       (* 
        * Perform tail duplication if no side entries are allowed
        * BUG: make sure liveness information is kept up-to-date! XXX
        *)
       fun tailDuplication(root, subgraph) =
       let val {nodes, edges} = Util.tailDuplicate IR 
                                  {subgraph=subgraph, root=root}
           val ins = PQ.insert seeds
           fun newNode (b,b') = (ins(b,b'); DA.update(blockIdTbl, b, 0))
       in  (* add new nodes created as a consequence of tail duplication
            * onto the queue so that they will be properly processed later.
            *)
           app newNode nodes
       end


       (* Create a new region *) 
       fun createRegion() = 
       let val seed     = newSeed()
           val blocks   = grow seed 
           val subgraph = makeSubgraph blocks;
       in  if sideEntries then () else tailDuplication(hd blocks, subgraph);
           subgraph
       end

       (* Number of instructions *)
       fun numberOfInstructions(G.GRAPH cfg) =
       let val size = ref 0
       in  #forall_nodes cfg (fn (_,CFG.BLOCK{insns, ...}) =>
                  size := !size + length(!insns));
           !size 
       end

       fun sizeOf(G.GRAPH cfg) = #order cfg ()

       (* Main loop *) 
       fun main() =
       let val region as G.GRAPH R = createRegion()
           val size = sizeOf region
       in  if size <= 1 then ()
           else
             let val numberOfInstructions = numberOfInstructions region
             in  if numberOfInstructions <= 2 then ()
                 else
                   let val _ = 
                       (print("REGION["^i2s(#order R ())^"] ");
                        app (fn (X,_) => print(i2s X^" ")) (#nodes R ());
                        print "\n")

                   in if !view_IR then IR.viewSubgraph IR region else ();
                      schedule{ir=IR, region=region, 
                               blockIdTbl=DA.baseArray blockIdTbl,
                               numberOfInstructions=numberOfInstructions};
                      if !view_IR then IR.viewSubgraph IR region else ()
                   end
             end;
           main()
       end 
 
   in  initialization();
       main() handle PQ.EmptyPriorityQueue => ()
   end

end
