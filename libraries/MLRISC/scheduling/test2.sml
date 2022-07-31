(*
 * This is the example I used in the PACT '98 paper 
 *)

structure Test2 =
struct

   structure G = DirectedGraph

   fun makeDag nodes edges =
   let val dag as Graph.GRAPH G = G.graph("Test1",(),10)
   in  app (#add_node G) nodes;
       app (#add_edge G) edges;
       dag
   end

   val dag = makeDag [(1,(0,2)),
                      (2,(1,2)),
                      (3,(0,9)),
                      (4,(0,9)),
                      (5,(0,7)),
                      (6,(0,9)),
                      (7,(6,9)),
                      (8,(6,8))
                     ]
                     [(1,2,0),
                      (1,4,0),
                      (2,3,0),
                      (2,5,0),
                      (4,3,1),
                      (4,5,1),
                      (3,6,1),
                      (7,6,0)
                     ]

   fun close dag =
       TransitiveClosure.acyclic_transitive_closure2 
          {+   = fn(i,j) => i+j+1,
           max = Int.max
          } dag

   fun leung (dag as Graph.GRAPH G) =
   let val dag' as Graph.GRAPH G' = G.graph("Tmp",(),10)
       val _    = #forall_nodes G (#add_node G')
       val _    = #forall_edges G (#add_edge G')
   in  LeungPalemPnueli.rank
                       {dag = dag',
                        l   = fn(_,_,l) => l,
                        d   = fn(_,(_,d)) => d,
                        r   = fn(_,(r,_)) => r,
                        m   = 1
                       } 
   end

   structure View = GraphViewerFn(daVinci)
   structure L    = GraphLayout

   fun view dag =
       View.view(
          L.makeLayout{node=fn(n,(r,d))=>
                          [L.LABEL(Int.toString n^" r="^Int.toString r^
                                                  " d="^Int.toString d)
                          ],
                       edge=fn(i,j,l)=>[L.LABEL(Int.toString l),L.COLOR "red"],
                       graph=fn _ =>[]} dag
       )

end
