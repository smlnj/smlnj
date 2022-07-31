(*
 * This example is from the paper
 * ``A New Frameowrk for Elimination Based Data Flow Analysis using DJ Graphs''
 * By Sreedhar et. al. 
 * This is the irreducible example.
 *)
structure TestDJDataflow2 =
struct
structure Graph = Graph;
val CFG as Graph.GRAPH cfg = DirectedGraph.graph("cfg",(),10) :
    (string,unit,unit) Graph.graph 
structure Viewer = GraphViewer(AllDisplays)
structure L   = GraphLayout
structure Dom = DominatorTree(DirectedGraph)
structure DJ  = DJGraph(Dom)
structure Dataflow = DJDataflow(Dom)

val _ = app (#add_node cfg)
          [(0,"0"),
           (1,"1"),
           (2,"2"),
           (3,"3"),
           (4,"4"),
           (5,"5"),
           (6,"6"),
           (7,"7"),
           (8,"8")
          ]
val _ = #set_entries cfg [0]
val _ = #set_exits cfg [9]
val _ = app (#add_edge cfg)
          [(0,1,()),
           (1,2,()),
           (1,3,()),
           (2,4,()),
           (3,4,()),
           (4,5,()), 
           (4,6,()),
           (5,7,()),
           (6,7,()),
           (7,4,()),
           (7,8,()),
           (8,3,())
          ] 

val Dom = Dom.makeDominator CFG

fun viewCFG _ =
    Viewer.view(L.makeLayout
                  {graph = fn _ => [],
                   node  = fn (i,_) => [L.LABEL(Int.toString i)],
                   edge  = fn (i,j,_) => [L.COLOR "red"]
                  } CFG)
fun viewDom _ =
    Viewer.view(L.makeLayout
                  {graph = fn _ => [],
                   node  = fn (i,_) => [L.LABEL(Int.toString i)],
                   edge  = fn (i,j,_) => [L.COLOR "red"]
                  } Dom)
fun viewDJ _ = 
    let fun iso kind G = 
            IsomorphicGraphView.map (fn x => x) (fn x => kind) (fn g => ()) G
        val idom = Dom.immediately_dominates Dom
        val Dom = iso [L.COLOR "red"] Dom 
        val CFG = iso [L.COLOR "green"] CFG 
        val CFG' = SubgraphView.subgraph_view
                      (map #1 (#nodes cfg ()))
                      (fn (i,j,_) => not(idom(i,j))) CFG
        val DJ = UnionGraphView.union_view (fn _ => ()) (Dom,CFG')
    in  Viewer.view(L.makeLayout 
                  {graph = fn _ => [],
                   node  = fn (i,_) => [L.LABEL(Int.toString i)],
                   edge  = fn (i,j,e) => e
                  } DJ)
    end

fun testDataflow() = 
let fun closure{y} = print("Closure "^Int.toString y^"\n")
    fun var_elim{y,z} = print("Variable elim "^Int.toString y^
                              "->"^Int.toString z^"\n")
    fun fixpoint{scc} = ()
    fun compute{y,z} = ()
in  Dataflow.analyze{closure=closure, var_elim=var_elim, 
                     fixpoint=fixpoint, compute=compute} Dom
end

end

