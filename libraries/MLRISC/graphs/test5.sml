CM.make "../cm/Graphs.cm";

(* page 556 in CLR *)
functor TestAllPairsShortestPaths(AP : ALL_PAIRS_SHORTEST_PATHS 
                                      where type Num.elem = int) =
struct
val G as Graph.GRAPH g = DirectedGraph.graph("foo",(),10) :
    (string,int,unit) Graph.graph 
val _ = app (#add_node g)
          [(1,"1"),
           (2,"2"),
           (3,"3"),
           (4,"4"),
           (5,"5")
          ]
val E =   [(1,2,3),
           (1,3,8),
           (1,5,~4),
           (2,4,1),
           (2,5,7),
           (3,2,4), 
           (4,1,2),
           (4,3,~5),
           (5,4,6)
          ] 
val _ = app (#add_edge g) E
(* val _ = app (fn (i,j,w) => #add_edge g (j,i,w)) E *)

val dist' = [[0,1,~3,2,~4],
             [3,0,~4,1,~1],
             [7,4,0,5,3],
             [2,~1,~5,0,~2],
             [8,5,1,6,0]
            ]
val pred' = [[~1,3,4,5,1],
             [4,~1,4,2,1],
             [4,3,~1,2,1],
             [4,3,4,~1,1],
             [4,3,4,5,~1]
            ]

fun toList M =
let val N = 5
    fun f(i,j) = if j > N then [] else Array2.sub(M,i,j)::f(i,j+1)
    fun g(i) = if i > N then [] else f(i,1)::g(i+1)
in  g 1
end

fun test() = 
    let fun weight(_,_,w) = w
        val {dist,pred} = AP.all_pairs_shortest_paths {graph=G,weight=weight}
        val dist=toList dist 
        val pred=toList pred
    in  if dist <> dist' orelse pred <> pred' then raise Match else ();
        {dist=dist,pred=pred}
    end

end

structure TestWarshall = TestAllPairsShortestPaths(
            FloydWarshall(struct type elem = int
                                   open Int
                                   val zero = 0 
                                   val == : int * int -> bool = op =
                                   val inf = 100000000
                                end))
structure TestJohnson = TestAllPairsShortestPaths(
             Johnson(struct type elem = int
                                   open Int
                                   val zero = 0 
                                   val == : int * int -> bool = op =
                                   val inf = 100000000
                                end))
