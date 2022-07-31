CM.make "../cm/Graphs.cm";
structure TestMinCut =
struct
val G as Graph.GRAPH g = DirectedGraph.graph("foo",(),10) :
    (string,int,unit) Graph.graph 
val _ = app (#add_node g)
          [(0,"s"),
           (1,"v1"),
           (2,"v2"),
           (3,"v3"),
           (4,"v4"),
           (5,"t")
          ]
val E =   [(0,1,16),
           (0,2,13),
           (1,2,10),
           (2,1,4),
           (1,3,12),
           (2,4,14),
         (*(3,2,9),*)
           (4,3,7), 
           (3,5,20),
           (4,5,4)
          ] 
val _ = app (#add_edge g) E
(* val _ = app (fn (i,j,w) => #add_edge g (j,i,w)) E *)

structure MinCut = MinCut(struct type elem = int
                                   open Int
                                   val zero = 0 
                                   val == : int * int -> bool = op =
                            end)

fun test() = 
    let fun weight(_,_,w) = w
        val (cut,w) = MinCut.min_cut {graph=G,weight=weight}
    in  if w <> 23 then raise Match else ();
        (cut,w)
    end

end
