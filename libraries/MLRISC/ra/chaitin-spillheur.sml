(*
 * This module implements the Chaitin heuristic (but weighted by
 * priorities)
 *)
structure ChaitinSpillHeur : RA_SPILL_HEURISTICS =
struct

   structure G = RAGraph

   open G

   exception NoCandidate

   val mode = RACore.NO_OPTIMIZATION

   fun init() = ()

   (*
    * Potential spill phase.
    * Find a cheap node to spill according to Chaitin's heuristic.
    *)
    fun chooseSpillNode{graph, hasBeenSpilled, spillWkl} = 
    let fun chase(NODE{color=ref(ALIASED n),...}) = chase n
          | chase n = n
        val infiniteCost = 123456789.0
        val don'tUse     = 223456789.0
  
        (* The spill worklist is maintained only lazily.  So we have
         * to prune away those nodes that are already removed from the
         * interference graph.  After pruning the spillWkl, 
         * it may be the case that there aren't anything to be 
         * spilled after all.
         *)

        (*
         * Choose node with the lowest cost and have the maximal degree
         *)
        fun chaitin([], best, lowestCost, spillWkl) = 
              (best, lowestCost, spillWkl)
          | chaitin(node::rest, best, lowestCost, spillWkl) = 
             (case chase node of
               node as NODE{number, pri, defs, uses,
                            degree=ref deg, color=ref PSEUDO,...} => 
               let fun cost() = !pri / real deg
                   val cost = 
                      case (!defs, !uses) of
                        (_,[]) => (* defs but no use *)
                                  ~1.0 - real deg
                      | ([d],[u]) => (* defs after use; don't use *) 
                         let fun plus({block,insn},n) = {block=block,insn=insn+n}
                         in  if d = plus(u,1) orelse d = plus(u,2) 
                             then don'tUse else cost()
                         end
                      | _ => cost()
               in  if cost < lowestCost andalso not(hasBeenSpilled number)
                   then 
                     (case best of
                        NONE => chaitin(rest, SOME node, cost, spillWkl)
                      | SOME best => 
                          chaitin(rest, SOME node, cost, best::spillWkl)
                     )
                   else
                     chaitin(rest, best, lowestCost, node::spillWkl)
               end
             | _ => (* discard node *)
                chaitin(rest, best, lowestCost, spillWkl)
             )

        (* val _ = print("["^Int.toString(length spillWkl)^"]") *)

        val (potentialSpillNode, cost, newSpillWkl) = 
             chaitin(spillWkl, NONE, infiniteCost, [])
    in  case (potentialSpillNode, newSpillWkl) of
          (NONE, []) => {node=NONE, cost=cost, spillWkl=[]}
        | (NONE, _) => raise NoCandidate
        | (SOME node, spillWkl) => 
              {node=SOME node, cost=cost, spillWkl=spillWkl}
    end
end
