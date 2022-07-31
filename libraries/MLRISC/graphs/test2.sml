CM.make "../cm/Graphs.cm";
(* See page 249 of Aho Hopcroft Ullman *)
structure TestMatching =
struct
val G as Graph.GRAPH g = DirectedGraph.graph("foo",(),10) :
    (string,int,unit) Graph.graph 
val _ = app (#add_node g)
          [(1,"1"),
           (2,"2"),
           (3,"3"),
           (4,"4"),
           (5,"5"),
           (6,"6"),
           (7,"7"),
           (8,"8"),
           (9,"9"),
           (10,"10")
          ]
val _ = app (#add_edge g)
          (rev[(1,6,1),
           (1,7,1),
           (1,8,1),
           (2,6,1),
           (2,9,1),
           (2,10,1),
           (3,6,1),
           (3,7,1),
           (4,8,1),
           (4,9,1),
           (4,10,1),
           (5,6,1)
          ])
fun matching((i,j,c),_) = 
    print(Int.toString i ^ " -> " ^ Int.toString j ^ "\n")

fun test() = 
    let val (_,card) = BipartiteMatching.matching G matching ()
    in  if card <> 5 then raise Match else card
    end

end
