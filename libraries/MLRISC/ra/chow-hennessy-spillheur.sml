(*
 * This module implements a Chow-Hennessy-style spill heuristic 
 *)
structure ChowHennessySpillHeur : RA_SPILL_HEURISTICS =
struct

   structure G    = RAGraph
   structure Heap = PriorityHeap

   open G

   exception NoCandidate

   val mode = RACore.COMPUTE_SPAN
 
   val cache = ref NONE : (G.node * real) Heap.priority_queue option ref

   fun init() = cache := NONE

   (*
    * Potential spill phase.
    * Find a cheap node to spill according to Chow Hennessy's heuristic.
    *)
    fun chooseSpillNode{graph as G.GRAPH{span, ...},
                        hasBeenSpilled, spillWkl} =
    let fun chase(NODE{color=ref(ALIASED n),...}) = chase n
          | chase n = n
        (* The spill worklist is maintained only lazily.  So we have
         * to prune away those nodes that are already removed from the
         * interference graph.  After pruning the spillWkl, 
         * it may be the case that there aren't anything to be 
         * spilled after all.
         *)
        fun chowHennessy spills =
        let (* Compute savings due to moves *)
            val spillSavings = RACore.moveSavings graph
            val lookupSpan = IntHashTable.find (Option.valOf(!span))
            val lookupSpan = 
                fn r => case lookupSpan r of SOME s => s | NONE => 0.0
            val _          = span := NONE
            fun loop([], L, pruned) = (L, pruned)
              | loop(node::rest, L, pruned) = 
                (case chase node of
                  node as NODE{number, pri, defs, uses,
                               degree=ref deg, color=ref PSEUDO,...} => 
                  if hasBeenSpilled number 
                  then loop(rest, L, false)
                  else
                  let fun newnode() =
                      let val span = lookupSpan number
                          val savings =  spillSavings number
                          val spillCost = !pri
                          val totalCost = spillCost - savings
                          (*val rank = ((real totalCost)+0.01) / real(span)*)
                          val rank = (totalCost + 0.5) / (span + real deg)
                      in  loop(rest, (node, rank)::L, false) end
                  in  case (!defs, !uses) of
                        (_, []) =>  (* one def no use *)
                          loop(rest, (node, ~1.0 - real(deg))::L, false)
                      | ([d], [u]) => (* defs after use; don't use *) 
                        let fun plus({block,insn},n) = {block=block,insn=insn+n}
                        in  if d = plus(u,1) orelse d = plus(u,2) then 
                               loop(rest, L, false)
                            else 
                               newnode()
                        end
                      | _ => newnode() 
                  end 
                | _ => loop(rest, L, pruned) (* discard node *)
                )
        in  loop(spills, [], true)
        end

        fun chooseNode heap =
        let fun loop() = 
            let val (node,cost) = Heap.deleteMin heap
            in  case chase node of
                   node as NODE{color=ref PSEUDO, ...} =>
                      {node=SOME(node), cost=cost, spillWkl=spillWkl}
                |  _ => loop()    
            end
        in  loop()
        end handle _ => {node=NONE, cost=0.0, spillWkl=[]}

    in  case !cache of
          SOME heap => chooseNode heap
        | NONE => 
          let val (L, pruned) = chowHennessy(spillWkl)
          in  if pruned then (* done *) 
                 {node=NONE, cost=0.0, spillWkl=[]}
              else
                (case L of
                  [] => raise NoCandidate
                | _  => let fun rank((_,x), (_,y)) = Real.<(x, y)
                            val heap = Heap.fromList rank L
                        in  cache := SOME heap; 
                            chooseNode heap
                        end
                )
          end
    end
end
