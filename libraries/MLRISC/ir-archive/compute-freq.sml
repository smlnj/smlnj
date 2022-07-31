(*
 * This module computes frequencies when given branch probabilities.
 * Bug fix: 
 *   This module didn't work on irreducible flowgraphs!
 *   The problem was caused 
 *
 * -- Allen
 *)

functor ComputeFrequencies
    (structure Loop : LOOP_STRUCTURE
     structure Freq : FREQ
    ) : COMPUTE_FREQUENCIES =
struct

   structure Loop = Loop
   structure Dom  = Loop.Dom
   structure G    = Graph
   structure S    = BitSet  
   structure W    = Freq
   structure A    = Array
   structure H    = HashArray

   val op div = W.div

   fun compute_frequencies
         {cfg,loop,loopMultiplier,nodeFreq,edgeFreq,branchProb,isTakenBranch} =
   let val G.GRAPH cfg          = cfg
       val Loop as G.GRAPH loop = loop 
       val ENTRY                = case #entries cfg () of
                                     [ENTRY] => ENTRY
                                   | _ => raise Graph.NotSingleEntry
       val N                    = #capacity cfg ()
       val marked               = S.create N
       val number_of_entries    = length(#out_edges cfg ENTRY)     
       val entry_weight         = W.*(W.fromInt 100,number_of_entries)

          (* indexed by headers *)
       val likely_exits         = H.array(N,[])
       val exit_counts          = H.array(N,0)

          (* indexed by nodes *)
       val entry_edges          = A.tabulate(N,#in_edges cfg)
       val header_of            = Loop.header Loop
       val nodeFreqs            = A.array(N,0)
       val branchProbs          = A.array(N,0)
       val TIMES                = 20
 
       val _ = #forall_nodes cfg (fn (b,b') =>
                  (A.update(nodeFreqs,b,!(nodeFreq b'));
                   A.update(branchProbs,b,branchProb b')
                  ))

       fun is_exit_edge (e as (i,j,_)) = 
            List.exists (fn (i',j',_) => i = i' andalso j = j')
               (H.sub(likely_exits,A.sub(header_of,i)))

       val sum = List.foldr (fn ((_,_,e),m) => !(edgeFreq e) + m) 0

       fun exit_weight_of i = 
       let val h = A.sub(header_of,i)
           val w = A.sub(nodeFreqs,h)
       in  w div (loopMultiplier * H.sub(exit_counts,h))
       end

       val entryEdges = Loop.entryEdges Loop

       fun preprocess(header,Loop.LOOP{exits,...}) = 
       let val real_exits = 
               List.filter (fn (i,_,_) => A.sub(branchProbs,i) > 0) exits
       in  H.update(likely_exits,header,real_exits);
           H.update(exit_counts,header,length real_exits);
           A.update(entry_edges,header,entryEdges header) 
       end

       fun propagate(0,_) = (print "Out of time\n")
         | propagate(n,[]) = ()
         | propagate(n,i::worklist) =
       let val _ = S.reset(marked,i)
           val old_weight = A.sub(nodeFreqs,i)
           val new_weight = sum(A.sub(entry_edges,i))
           val new_weight = if i = ENTRY then entry_weight
                            else (case H.sub(likely_exits,i) of
                                   [] => new_weight (* not a real loop! *)
                                 | _ => W.*(new_weight,loopMultiplier) 
                                 )
       in  if old_weight = new_weight then
                propagate(n,worklist)
           else (A.update(nodeFreqs,i,new_weight);
                 propagate_edge_weight(#out_edges cfg i,new_weight,[]);
                 propagate'(n,#out_edges cfg i,worklist)
                )
       end

       and propagate'(n,[],worklist) = propagate(n,worklist)
         | propagate'(n,(i,j,_)::es,worklist) =
           if S.markAndTest(marked,j) then 
                propagate'(n,es,worklist)
           else propagate'(Int.-(n,1),es,j::worklist)

       and propagate_edge_weight([],W,es') = process_non_exits(W,es')
         | propagate_edge_weight((edge as (i,_,e))::es,W,es') =
           if is_exit_edge edge then 
              let val exit_weight = exit_weight_of(A.sub(header_of,i))
                  val w = edgeFreq e
              in  w := exit_weight; 
                  propagate_edge_weight(es,W-exit_weight,es')
              end
           else
              propagate_edge_weight(es,W,edge::es')

       and process_non_exits(W,[]) = ()
         | process_non_exits(W,[(_,_,e)]) = edgeFreq e := W
         | process_non_exits(W,es as [edge1 as (i,_,e1),(_,_,e2)]) =
           if i = ENTRY then divide_evenly(W,es) else
           let val w = edgeFreq e1
               val w' = edgeFreq e2
               val (w_F,w_T) = if isTakenBranch edge1 then (w',w) else (w,w')
               val p = A.sub(branchProbs,i)
           in  w_T := W.*(W,p) div 100;
               w_F := W - !w_T
           end
         | process_non_exits(W,es) = divide_evenly(W,es)

       and divide_evenly(W,es) = 
           let val W' = W div (length es)
           in  app (fn (_,_,e) => edgeFreq e := W') es
           end

   in
       #forall_nodes loop preprocess;
       propagate(TIMES * N, [ENTRY]);
       #forall_nodes cfg (fn (b,b') => nodeFreq b' := A.sub(nodeFreqs,b))
   end handle Overflow => print "[Overflow]\n"

end
