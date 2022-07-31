CM.make "../cm/Graphs.cm";
functor TestShortestPaths(SP : SINGLE_SOURCE_SHORTEST_PATHS
                                  where type Num.elem = int) =
struct
val G as Graph.GRAPH g = DirectedGraph.graph("foo",(),10) :
    (string,int,unit) Graph.graph 
val _ = app (#add_node g)
          [(0,"s"),
           (1,"u"),
           (2,"v"),
           (3,"x"),
           (4,"y")
          ]
val E =   [(0,1,10),
           (0,2,5),
           (1,2,2),
           (2,1,3),
           (1,3,1),
           (2,3,9),
           (2,4,2),
           (3,4,4),
           (4,3,6), 
           (4,0,7)
          ] 
val _ = app (#add_edge g) E

val dist' = [0,8,5,9,7]
val pred' = [~1,2,0,1,2]

fun test() = 
    let fun weight(_,_,w) = w
        val {dist,pred} = SP.single_source_shortest_paths
                            {graph=G,weight=weight,s=0}
        val dist'' = Array.foldr op:: [] dist
        val pred'' = Array.foldr op:: [] pred
    in  if dist' <> dist'' orelse pred' <> pred'' then
           raise Match else ();
        {dist=dist,pred=pred} 
    end

end

structure TestDijkstra = TestShortestPaths(
               Dijkstra(struct open Int 
                                       type elem = int
                                       val zero = 0
                                       val inf = 10000000
                                       val == : int * int -> bool = op=
                                 end))
structure TestBellmanFord = TestShortestPaths(
             BellmanFord(struct open Int 
                                       type elem = int
                                       val zero = 0
                                       val inf = 10000000
                                       val == : int * int -> bool = op=
                                   end))
