(*
 * Perform elimination based dataflow analysis, using Sreedhar's DJ-graph  
 * based algorithm.  I'm using the eager elimination method because in 
 * practice it is linear and is much easier to implement.  
 *)
functor DJDataflow(Dom : DOMINATOR_TREE) : DJ_DATAFLOW = 
struct

   structure Dom = Dom
   structure G   = Graph
   structure A   = Array

   val debug = true

   fun error msg = MLRiscErrorMsg.error("DJDataflow",msg)

   fun analyze {closure, var_elim, fixpoint, compute} (Dom as G.GRAPH dom) =
   let val L                  = Dom.max_levels Dom
       val N                  = #capacity dom ()
       val CFG as G.GRAPH cfg = Dom.cfg Dom
       val levelsMap          = Dom.levelsMap Dom
       val idomsMap           = Dom.idomsMap Dom

       (*
        * These store the current join edges during the graph reduction process
        *) 
       val joinOutEdges       = A.array(N,[])        
       val joinInEdges        = A.array(N,[])        

       (* Priority lists indexed by levels so that all non-join nodes
        * always appear in front of the list
        *)
       val PriorityList       = A.array(L,[])

       (* A node i is a non-join node iff all its predecessors are either
       *  i or idom(i) 
        *)
       fun isNonJoinNode i =
           case A.sub(joinInEdges,i) of
              [] => true
            | [(j,_,_)] => i = j 
            | _  => false

       (* Remove the join edge y -> z with edge id *)
       fun removeJoinEdge(y,z,id) =
       let fun rmvEdges((e as (_,_,id'))::es, es') =
               if id' = id then List.revAppend(es', es)
               else rmvEdges(es, e::es')
             | rmvEdges _ = error "rmvEdges"
       in  A.update(joinOutEdges, y, rmvEdges(A.sub(joinOutEdges,y), []));
           A.update(joinInEdges, z, rmvEdges(A.sub(joinInEdges, z), []))
       end

       (* Remove all same level i join edges in nodes *)
       fun removeAllSameLevelEdges(i,nodes) =
       let fun rmv([], es') = es'
             | rmv((e as (x,y,_))::es, es') =
               rmv(es, if A.sub(levelsMap,y) = i then es' else e::es')
           fun loop [] = ()
             | loop(x::xs) =
               (A.update(joinOutEdges, x, rmv(A.sub(joinOutEdges, x), []));
                A.update(joinInEdges, x, rmv(A.sub(joinInEdges, x), []));
                loop xs
               )
       in  loop nodes 
       end
 

       (* Insert a new join edge *)
       fun insertJoinEdge(e as (x,y,_)) = 
          (A.update(joinOutEdges, x, e::A.sub(joinOutEdges, x));
           A.update(joinInEdges, y, e::A.sub(joinInEdges, y))
          )

       (* Does the edge y -> z exists? *)
       fun doesn'tHasEdge(y,z) =
       let fun loop [] = true
             | loop((_,z',_)::es) = z<>z' andalso loop es
       in  loop(A.sub(joinOutEdges, y)) end

       (* Put all nodes into its level in the priority list.
        * Initialize the join edges arrays.
        *)
       fun initialize() =
       let val edgeId = ref 0
       in  #forall_nodes cfg
             (fn (i,_) => 
              let val lvl = A.sub(levelsMap,i)
                  fun addJoinEdges([],id,outEdges) = 
                        (A.update(joinOutEdges,i,outEdges); id)
                    | addJoinEdges((i,j,_)::es,id,outEdges) =
                      if A.sub(idomsMap,j) = i then 
                         addJoinEdges(es, id, outEdges)
                      else let val e = (i,j,id)
                           in  A.update(joinInEdges,j,e::A.sub(joinInEdges,j));
                              addJoinEdges(es, id+1, e::outEdges)
                           end
              in  A.update(PriorityList,lvl,i::A.sub(PriorityList,lvl)); 
                  edgeId := addJoinEdges(#out_edges cfg i, !edgeId, [])
              end
              )
       end

       (* All these nodes are on the same level and they cannot be reduced.
        * Determine the SCC for the nodes at level i
        *)
       fun CollapseIrreducible(i, joinNodes) = 
       let fun out_edges y = 
           let fun filter([], es') = es'
                 | filter((e as (y,z,_))::es, es') =
                   filter(es, if A.sub(levelsMap,z) = i then e::es' else es')
           in  filter(A.sub(joinOutEdges, y), []) end
           fun dumpSCC scc =
              (print "scc:\n";
               app (fn x => 
                      (print(Int.toString x^" ");
                       app (fn (x,y,_) => 
                          print(Int.toString x^"->"^Int.toString y^" "))
                            (out_edges x);
                        print "\n"
                       ))
                   scc
              )
           fun processSCC(scc) = 
               (if debug then dumpSCC scc else ();
                fixpoint{scc=scc}
               )
           val sccs = 
               GraphSCC.scc'
                 {N         = #capacity cfg (),
                  nodes     = joinNodes,
                  out_edges = out_edges
                 } op:: []
       in  app processSCC sccs;
           removeAllSameLevelEdges(i,joinNodes)
       end
      
       (* Self loops y->y 
        * Compute the closure H_y : O_y = f^*_y(O_y)
        * Then delete the edge y->y.
        * y must be a non-join node before and after this transformation.
        *) 
       fun Eager1(y,id) = 
           (closure{y=y};
            removeJoinEdge(y,y,id)
           )

       (* Same level y->z 
        * Eliminate O_y in H_z by replacing it with the RHS of H_y
        * Delete the edge y->z;
        * if (z becomes a non-join node) then put z at the head of the 
        *    priority list at level i
        *)
       fun Eager2a(y,z,i,id,queue,prune) = 
           (var_elim{y=y,z=z};
            removeJoinEdge(y,z,id);
            if isNonJoinNode z then (z::queue,true) else (queue,prune)
           )

       (* Different levels y->z 
        * Eliminate O_y in H_z by replacing it with RHS of H_y
        * x = idom(y)
        * Delete the edge y->z
        * if (x->z does not exist) then
        *    Insert a new J edge x->z
        *)
       fun Eager2b(y,z,id) = 
           (var_elim{y=y,z=z};
            let val x = A.sub(idomsMap,y)
            in  removeJoinEdge(y,z,id);
                if doesn'tHasEdge(x,z) then insertJoinEdge(x,z,id)
                else ()
            end
           )

       fun ReduceLevel i = 
       let fun loop([], [], _) = ()
             | loop([], joinNodes, prune) =  
               let fun filter([], ns) = ns
                     | filter(z::zs, ns) = 
                       if isNonJoinNode z then filter(zs, ns)
                       else filter(zs, z::ns)
                   (* If prune is true, then the joinNodes list may
                    * contain processed non-join nodes, 
                    * so we have to prune them out first.
                    *)
                   val joinNodes = 
                       if prune then filter(joinNodes,[]) else joinNodes
               in  case joinNodes of
                     [] => ()
                   | _ => (CollapseIrreducible(i,joinNodes); ReduceLevel i)
               end
             | loop(y::Q, joinNodes, prune) = 
                if isNonJoinNode y then
                   let fun process([], Q, prune) = (Q, prune)
                         | process((y,z,id)::es,Q, prune) = 
                           if z = y then (Eager1(y,id); process(es, Q, prune))
                           else if A.sub(levelsMap,z) = i then 
                                let val (Q, prune) = Eager2a(y,z,i,id,Q,prune)
                                in process(es, Q, prune) end
                           else (Eager2b(y,z,id); process(es, Q, prune))
                       val (Q, prune) = process(A.sub(joinOutEdges, y),Q,prune)
                   in  loop(Q, joinNodes, prune)
                   end
                else loop(Q, y::joinNodes, prune)
       in  loop(A.sub(PriorityList,i), [], false) 
       end

       (* Propagate the results from the top of the dominator tree
        * down to its children.
        *)
       fun DomTDPropagate() = 
       let val ENTRY = hd(#entries dom ())
           fun walk y =
           let fun walkChildren [] = ()
                 | walkChildren((_,z,_)::es) =
                    (compute{y=y, z=z}; walk z; walkChildren es) 
           in walkChildren(#out_edges dom y) end
       in  walk ENTRY 
       end
              
       fun MainDFA() =
       let fun loop ~1 = ()
             | loop i = (ReduceLevel i; loop(i-1))
       in  initialize();
           loop(L-1);
           DomTDPropagate()
       end
          
   in  MainDFA()
   end

end
