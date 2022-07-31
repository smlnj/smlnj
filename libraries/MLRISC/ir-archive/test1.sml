structure TestDJGraph =
struct
structure Graph = Graph;
val G as Graph.GRAPH g = DirectedGraph.graph("foo",(),10) :
    (string,int,unit) Graph.graph 
structure Dom = DominatorTree(DirectedGraph)
structure DJ  = DJGraph(Dom)
val _ = app (#add_node g)
          [(0,"s"),
           (1,"v1"),
           (2,"v2"),
           (3,"v3"),
           (4,"v4"),
           (5,"t")
          ]
val _ = #set_entries g [0]
val _ = #set_exits g [5]
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

val dom  = Dom.makeDominator G
val pdom = Dom.makePostdominator G
val IDFs = DJ.IDFs dom 

end
