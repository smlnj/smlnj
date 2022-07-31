(*
 * Global code motion algorithm in SSA form.
 * This is based on Cliff Click's algorithm.
 * I've generalized it to 
 * 
 *   1. Take into account of execution frequencies to find the ``best'' 
 *      position to move an instruction.
 *   2. Perform partial redundancy elimination.  (not finished)
 *
 * -- Allen (leunga@cs.nyu.edu)
 *)

signature SSA_GLOBAL_CODE_MOTION =
sig 
   include SSA_OPTIMIZATION 
   val computeEarliest : SSA.ssa -> SSA.block Array.array
   val computeBest     : SSA.ssa * SSA.block Array.array -> SSA.ssa
end 

functor SSAGCM(SSA : SSA) : SSA_GLOBAL_CODE_MOTION =
struct
   structure SSA  = SSA
   structure CFG  = SSA.CFG
   structure Dom  = SSA.Dom
   structure SP   = SSA.SP
   structure RTL  = SSA.RTL
   structure T    = RTL.T
   structure W    = CFG.W
   structure G    = Graph
   structure A    = Array

   type flowgraph = SSA.ssa

   val debug = MLRiscControl.getFlag "ssa-debug-gcm";

   val name = "Global Code Motion"

   fun error msg = MLRiscErrorMsg.error("SSAGCM",msg)

   val i2s = Int.toString
   val b2s = Bool.toString

   (* Compute the a mapping from ssa_id to earliest block that can be moved. *)
   fun computeEarliest(SSA as G.GRAPH ssa) =
   let val Dom         = SSA.dom SSA
       val M           = #capacity ssa ()

       val rtlTbl      = SSA.rtlTbl SSA
       val blockTbl    = SSA.blockTbl SSA
       val usesTbl     = SSA.usesTbl SSA
       val defSiteTbl  = SSA.defSiteTbl SSA

          (* Tables *)
       val earliestTbl = A.array(M,~1)  (* position for an instruction *)
       val entryPos    = Dom.entryPos Dom  (* block_id -> entry position *)
       val levels      = Dom.levelsMap Dom (* block id -> dominator level *)

       (* 
        * For each value, try to schedule it as early as possible.
        * This is constrained by the position of an instruction's operand.
        *)
       fun scheduleEarly(i, i') = 
           if A.sub(earliestTbl,i) >= 0 then () 
           else if RTL.can'tMoveUp(i') then 
                (A.update(earliestTbl, i, A.sub(blockTbl,i))) 
           else
           let val b = A.sub(blockTbl, i)
               val _ = A.update(earliestTbl,i,b) (* the initial position *)
               fun scan([],pos_i,depth_i) = pos_i
                 | scan(v::uses,pos_i,depth_i) =
                   if v < 0 then scan(uses, pos_i, depth_i)
                   else
                   let val j       = A.sub(defSiteTbl, v)
                       val _       = scheduleEarly(j, A.sub(rtlTbl, j))
                       val pos_j   = A.sub(earliestTbl,j)
                       val depth_j = A.sub(levels,pos_j)
                   in  (* print("\tearliest("^showVal v^")="^i2s pos_j^"\n"); *)
                       if depth_j > depth_i then
                           scan(uses,pos_j,depth_j)
                       else
                           scan(uses,pos_i,depth_i)
                   end
               val pos_i   = A.sub(entryPos,b)
               val depth_i = A.sub(levels,pos_i)
               val earliest = scan(A.sub(usesTbl, i), pos_i, depth_i)
           in  (* if RTL.isLooker(A.sub(rtlTbl,i)) then *)
               (* print("Pass 1 "^showOp i^" orig="^i2s b^
                        " earliest="^i2s earliest^
                        "\n"); *)
               (* if dominates(earliest, b) then () 
               else error "WTF! earliest > b"; *)
               A.update(earliestTbl,i,earliest)
           end

       (* Visit all the pinned instructions first *)
       fun pass1 i =
           if RTL.can'tMoveUp(A.sub(rtlTbl, i)) then
              (A.update(earliestTbl,i,A.sub(blockTbl, i));
               app (fn (j,_,_) => scheduleEarly(j, A.sub(rtlTbl,j))) 
                  (#in_edges ssa i)
              )
           else ()
   in  SSA.forallNodes SSA pass1; (* schedule earliest *)
       earliestTbl
   end


   (* Find the best position and move the instructions *)
   fun computeBest(SSA as G.GRAPH ssa, earliestTbl) =
   let val Dom         = SSA.dom SSA
       val M           = #capacity ssa ()

       val rtlTbl      = SSA.rtlTbl SSA
       val blockTbl    = SSA.blockTbl SSA
       val usesTbl     = SSA.usesTbl SSA
       val defSiteTbl  = SSA.defSiteTbl SSA

       val levels  = Dom.levelsMap Dom (* block id -> dominator level *)
       val succTbl = SSA.succTbl SSA
       val freqTbl = SSA.freqTbl SSA   (* frequencies indexed by block id *)
       val idoms   = Dom.idomsMap Dom  (* block id -> immediate dominator *)
       val moveOp  = SSA.moveOp SSA

       val showOp  = SSA.showOp SSA
       val showVal = SSA.showVal SSA

       fun dump(i,b,earliest,latest,best) = 
           print(showOp i^" orig="^i2s b^
                 " earliest="^i2s earliest^
                 " latest="^i2s latest^
                 " best="^i2s best^
                 "\n")

       (*
        * Schedule an instruction as late as possible.
        * Visited nodes are indicated by earliest = ~1
        *)  
       fun scheduleLate i = scheduleLate'(i,A.sub(rtlTbl,i))
       and scheduleLate'(i,T.PHI _) = A.sub(blockTbl,i)
         | scheduleLate'(i,T.SINK _) = A.sub(blockTbl,i)
         | scheduleLate'(i,T.SOURCE _) = A.sub(blockTbl,i)
         | scheduleLate'(i,i') =
           if A.sub(earliestTbl,i) < 0 then A.sub(blockTbl, i)
           else 
           let val earliest = A.sub(earliestTbl,i)
               val _ = A.update(earliestTbl,i,~1) (* mark node as visited *)
               val b = A.sub(blockTbl, i)

               (* Find the least common ancestor of two nodes in the dominator*)
               fun LCA(~1,b) = b
                 | LCA(a,~1) = a
                 | LCA(a,b) = 
                   let val la = A.sub(levels,a)
                       val lb = A.sub(levels,b)
                   in  if la > lb then LCA'(a,b,la-lb) else LCA'(b,a,lb-la) end
               and LCA'(a,b,0) = LCA''(a,b)
                 | LCA'(a,b,l) = LCA'(A.sub(idoms,a),b,l-1)
               and LCA''(a,b) =
                   if a = b then a else LCA''(A.sub(idoms,a),A.sub(idoms,b))
              
               fun scan([],~1,trivialUsesOnly) = (b,trivialUsesOnly)
                 | scan([],latest,trivialUsesOnly) = (latest,trivialUsesOnly)
                 | scan((_,j,r)::es,latest,trivialUsesOnly) = 
                   let val j' = A.sub(rtlTbl, j)
                       val pos_j = scheduleLate'(j, j')
                       val (latest,trivialUsesOnly) = 
                           case j' of
                              T.PHI{preds,...} =>
                              let val s = A.sub(usesTbl,j)
                                  fun loop(b::bs,s::ss,lca) = 
                                       if s = r then loop(bs,ss,LCA(lca,b))
                                       else loop(bs,ss,lca)
                                    | loop(_,_,bs') = bs'
                              in  (loop(preds,s,latest), trivialUsesOnly) end
                           |  T.SINK _ => (LCA(latest,pos_j), trivialUsesOnly)
                           |  _ => (LCA(latest,pos_j), false)
                   in  scan(es,latest,trivialUsesOnly) end
               val (latest,trivialUsesOnly) = 
                        scan(A.sub(succTbl, i),
                             if RTL.can'tMoveDown i' then b else ~1,
                             true)

               (* if latest = ~1 then the instruction is dead *)
               (* Performance hack: if the instruction is a constant 
                * computation and if it is only used in a phi function,
                * then DO NOT hoist them up because that will just increase
                * register pressure!
                *)
               fun find_best(pos,best_pos,best_freq) =
                   let val w = A.sub(freqTbl,pos) handle _ => 
                               error(showOp i^
                                     " pos "^i2s pos^" earliest="^i2s earliest^
                                     " latest="^i2s latest^" orig="^i2s b^
                                     " pinned="^b2s(RTL.can'tMoveUp i'))
                       val (best_pos,best_freq) = 
                             if w < best_freq then (pos,w) 
                             else (best_pos,best_freq)
                   in if pos = earliest then best_pos
                      else find_best(A.sub(idoms,pos),best_pos,best_freq)
                   end

               fun isConstant i = 
                   (case A.sub(usesTbl, i) of
                     [v] => v < 0 
                   | _ => false
                   )
               val best = if earliest = ~1 then ~1 
                          else if latest = ~1 then ~1
                          else if trivialUsesOnly andalso isConstant(i) 
                          then latest
                          else find_best(latest,latest,A.sub(freqTbl,latest))
           in
               (* A.update(pos,i,best); *)
               if best = ~1 then ()
               else if best <> b then 
                  (if !debug then dump(i,b,earliest,latest,best) else ();
                   (* dump(i,b,earliest,latest,best); *)
                 (*if dominates(best, latest) then () else error "best>latest";
                   if dominates(earliest, best) then () else error "early>best";
                  *)
                   moveOp{id=i,block=best}
                  )
               else ();
               (* dump(i,b,earliest,latest,best); *)
               best
           end

       fun pass2 i = 
             (* Warning: this must be pinned instead of can'tMoveDown
              * because an op that can't be moved down doesn't mean it
              * can't be moved up!   If this is not done the pos of i would
              * be wrong!
              *)
             if RTL.pinned(A.sub(rtlTbl, i)) then
                 (A.update(earliestTbl,i,~1);
                  app (fn (_,j,_) => (scheduleLate j;())) 
                     (A.sub(succTbl,i)) 
                 )
             else ()

       fun pass3 i = (scheduleLate i; ())
   
       (*
        * Now, try to perform partial redundancy elimination.
        * Given an instruction i, find its earliest position not constrained
        * by the closest dominating uses that are phi-nodes.
        *)  
       (* 
       val defsTbl = SSA.defsTbl SSA
       fun pre i = 
           if W8A.sub(visited,i) = 0w3 then ()
           else
           let val _     = W8A.update(visited, i, 0w3)
               val rtl_i = A.sub(rtlTbl,i)
               val b_i   = A.sub(blockTbl, i)  
           in  if RTL.can'tMoveUp rtl_i then () else 
               let val earliest_i = A.sub(earliestTbl, i)
                   val depth_i    = A.sub(levels, earliest_i)
                   val uses_i     = A.sub(usesTbl, i)

                   fun moveable([], phis, preds, earliest, depth) = 
                         (phis, preds, earliest)
                     | moveable(v::uses, phis, preds, earliest, depth) = 
                       if v < 0 then 
                          moveable(uses, phis, preds, earliest, depth) 
                       else
                       let val j     = A.sub(defSiteTbl,v)
                           val b_j   = A.sub(blockTbl, j)
                       in  if b_j = earliest_i then
                              case A.sub(rtlTbl, j) of
                                T.PHI{preds, ...} => 
                                   moveable(uses, j::phis, preds, 
                                            earliest, depth)
                              | _ => ([], [], earliest) 
                                      (* not hoistable *)
                           else 
                              let val d_j = A.sub(levels, b_j)
                              in  if d_j < depth_i then
                                    if d_j > depth then
                                       moveable(uses, phis, preds, b_j, d_j)
                                    else
                                       moveable(uses, phis, preds, 
                                                earliest, depth)
                                  else
                                    ([], [], earliest) 
                              end
                       end

                   (* Is the phi-node partially redundant? 
                    * It is if is of the form t <- phi(...,t,...)
                    *)
                   fun replicationCost(newEarliest, preds, phi) =
                       let val [t] = A.sub(defsTbl, phi)
                           val uses = A.sub(usesTbl, phi)

                           fun search(~1, best, bestCost) = (best, bestCost)
                             | search(X, best, bestCost) = 
                               let val costX = A.sub(freqTbl, X)
                                   val (best, bestCost) =
                                      if costX < bestCost then
                                         (X, costX) 
                                      else
                                         (best, bestCost) 
                               in  if X = newEarliest then (best, bestCost)
                                   else search(A.sub(idoms, X), best, bestCost)
                               end

                           fun cost([], [], c) = c
                             | cost(p::ps, v::vs, c) = 
                               if v = t then 
                                  cost(ps, vs, c)
                               else
                                  let val (best, bestCost) = 
                                          search(p, p, A.sub(freqTbl, p))
                                  in  cost(ps, vs, c + bestCost) end
                       in  cost(preds, uses, 0) end

   
                   val start = A.sub(earliestTbl,i)
                   val d = A.sub(levels,start)
                   val (phis, preds, newEarliest) = 
                         moveable(uses_i, [], [], start, d)
               in  case phis of
                     [] => ()
                   | [phi] => 
                     let val cost  = A.sub(freqTbl,b_i)
                         val cost' = replicationCost(newEarliest, preds, phi)
                     in  if cost' < cost then
                          (print("PARTITIALLY REDUNDANT ["^i2s b_i^"] "^
                                 showOp i^" depth="^i2s depth_i^"\n");
                           print("Cost="^W.toString cost^
                                 " new cost="^W.toString cost'^"\n");
                           print("USES:\n");
                           app (fn v => 
                                if v < 0 then ()
                                else let val j   = A.sub(defSiteTbl, v)
                                         val b_j = A.sub(blockTbl, j)
                                         val d_j = A.sub(levels, b_j)
                                     in  print("\t["^i2s b_j^"] "^showOp j^
                                               " depth="^i2s d_j^"\n")
                                     end) uses_i
                          ) 
                         else ()
                     end
                   | _ => ()
               end
           end

       fun pass4 i = 
           if RTL.can'tMoveUp(A.sub(rtlTbl, i)) then
              (W8A.update(visited,i,0w3); 
               app (fn (j,_,_) => pre j) (#in_edges ssa i)
              )
           else ()
       *)

   in
       SSA.forallNodes SSA pass2; (* schedule latest *)
       SSA.forallNodes SSA pass3; (* schedule remaining source instructions *)
       (* SSA.forallNodes SSA pass4; *)
       SSA.changed SSA;
       SSA
   end

   (* Run the optimization *)
   fun run SSA = computeBest(SSA, computeEarliest SSA)

end
