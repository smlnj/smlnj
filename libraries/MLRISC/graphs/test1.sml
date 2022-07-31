CM.make "../cm/Graphs.cm";
structure TestMaxFlow =
struct
val G as Graph.GRAPH g = DirectedGraph.graph("foo",(),10) :
    (string,int,unit) Graph.graph 
structure MaxFlow = MaxFlow(struct type elem = int open Int
                                   val zero = 0 
                                   val == : int * int -> bool = op =
                            end)
val _ = app (#add_node g)
          [(0,"s"),
           (1,"v1"),
           (2,"v2"),
           (3,"v3"),
           (4,"v4"),
           (5,"t")
          ]
val _ = app (#add_edge g)
          [(0,1,16),
           (0,2,13),
           (1,2,10),
           (2,1,4),
           (1,3,12),
           (2,4,14),
           (3,2,9),
           (4,3,7), 
           (3,5,20),
           (4,5,4)
          ] 
fun flows((i,j,c),f) = 
    print(Int.toString i ^ " -> " ^ Int.toString j ^ " flow="^Int.toString f^
          " cap="^Int.toString c^"\n")
fun cap(i,j,c) = c

fun test() = 
    let val flow = MaxFlow.max_flow{graph=G,s=0,t=5,capacity=cap,flows=flows}
    in  if flow <> 23 then raise Match else flow
    end

end
