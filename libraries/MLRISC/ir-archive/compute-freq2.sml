(*
 * This module computes frequencies when given branch probabilities.
 * The last module still didn't work on irreducible flowgraphs!
 * I'm rewriting it using a completely different algorithm.
 *
 * -- Allen
 *)

functor ComputeFrequencies2
   (structure DerivedGraph : DERIVED_GRAPH
    structure Freq : FREQ
   ) : COMPUTE_FREQUENCIES2 =
struct

   structure Derived = DerivedGraph
   structure W       = Freq
   structure G       = Graph
   structure A       = Array
   structure HT      = HashTable

   val op div   = W.div
   val SOME inf = W.maxInt

   fun compute_frequencies
         {cfg=G.GRAPH cfg,derived as G.GRAPH dg,
          loopMultiplier,nodeFreq,edgeFreq,branchProb,isTakenBranch} =
   let val ENTRY             = case #entries cfg () of
                                [ENTRY] => ENTRY
                               | _ => raise Graph.NotSingleEntry
       val N                 = #capacity cfg ()

       fun hash(i,j,_) = Word.<<(Word.fromInt i,0w16) + Word.fromInt j
       fun equal((a:int,b:int,_),(c,d,_)) = a = c andalso b = d
       exception NotThere
       val edgeProbs = HT.mkTable (hash,equal) (10,NotThere) 
       val addProb   = HT.insert edgeProbs
       val getProb   = HT.lookup edgeProbs

       fun computeEdgeProb(n,n') =
       let fun divide_evenly(edges) =
           let val W' = 100 div (length edges)
               fun loop([],w) = ()
                 | loop([e],w) = addProb(e,w)
                 | loop(e::es,w) = (addProb(e,W'); loop(es,w-W'))
           in  loop(edges,100) end
           val edges = #out_edges cfg n 
       in  if n = ENTRY then divide_evenly edges else
           case edges of
             [] => ()
           | [e] => addProb(e,100)
           | [e1,e2] =>  
             let val prob = branchProb n'
                 val prob = if isTakenBranch e1 then prob else 100 - prob
             in  addProb(e1,prob);
                 addProb(e2,100-prob)
             end
           | es => divide_evenly es
       end

       (* Initialize the set of edge probabilities *)
       val _ = #forall_nodes cfg computeEdgeProb

       val visited = A.array(N,~1)

       fun process(scc as stamp::_,_) = 
       let val _ = app (fn b => A.update(visited,b,stamp)) scc
           fun collect([],inFreq,isLoop) = (inFreq,isLoop)
             | collect(n::ns,inFreq,isLoop) =
               let fun loop([],inFreq,isLoop) = (inFreq,isLoop)
                     | loop((i,j,e)::es,inFreq,isLoop) = 
                       if A.sub(visited,i) = stamp
                       then loop(es,inFreq,true)
                       else loop(es,inFreq + 
                              !(nodeFreq(#node_info cfg i)) * getProb e,
                               isLoop)
                   val (inFreq,isLoop) = loop(#in_edges dg n,inFreq,isLoop)
               in  collect(ns,inFreq,isLoop) end
           val (freq,isLoop) = collect(scc,0,false)
           val freq = if stamp = ENTRY then
                         W.*(W.fromInt 100,length(#out_edges cfg ENTRY)) 
                      else if isLoop then freq * loopMultiplier div 100
                      else freq div 100
       in  app (fn b => nodeFreq(#node_info cfg b) := freq) scc
       end

   in  GraphSCC.scc (ReversedGraphView.rev_view derived) process ();
       HT.appi (fn ((i,_,e),w) => 
                   edgeFreq e := (w * !(nodeFreq(#node_info cfg i))) div 100)
            edgeProbs
   end handle Overflow => ()

end
