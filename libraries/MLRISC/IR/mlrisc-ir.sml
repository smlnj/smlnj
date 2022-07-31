(*
 * MLRISC IR
 *
 * This is for performing whole program analysis.
 * All optimizations are based on this representation.
 * It provides a few useful views: dominator tree, control dependence graph,
 * loop nesting (interval) structure etc. Also there is a mechanism to
 * incrementally attach additional views to the IR.  The SSA infrastructure
 * is implemented in such a manner.
 *
 * -- Allen
 *)

functor MLRISC_IR
   (structure CFG         : CONTROL_FLOW_GRAPH
    structure CDG         : CONTROL_DEPENDENCE_GRAPH
    structure Loop        : LOOP_STRUCTURE
    structure GraphViewer : GRAPH_VIEWER
    structure Util        : CFG_UTIL
       sharing Loop.Dom = CDG.Dom
       sharing Util.CFG = CFG
   ) : MLRISC_IR =
struct

   structure I    = CFG.I
   structure CFG  = CFG
   structure Dom  = Loop.Dom
   structure CDG  = CDG
   structure Loop = Loop
   structure G    = Graph
   structure A    = Annotations
   structure Util = Util
   structure L    = GraphLayout
  
   type cfg  = CFG.cfg
   type IR   = CFG.cfg
   type dom  = (CFG.block,CFG.edge_info,CFG.info) Dom.dominator_tree
   type pdom = (CFG.block,CFG.edge_info,CFG.info) Dom.postdominator_tree
   type cdg  = (CFG.block,CFG.edge_info,CFG.info) CDG.cdg
   type loop = (CFG.block,CFG.edge_info,CFG.info) Loop.loop_structure

   val layouts = ref [] : (string * (IR -> L.layout)) list ref

   fun addLayout name layout =
   let fun f((x,y)::rest) = if x = name then (x,layout)::rest
                            else (x,y)::f rest
         | f [] = [(name,layout)]
   in  layouts := f(!layouts) end

   exception NoLayout 

   fun findLayout name =
   let fun f [] = (print ("[Can't find "^name^"]\n"); raise NoLayout)
         | f((x,layout)::rest) = if x = name then layout else f rest
   in  f(!layouts) end

   fun view name IR = GraphViewer.view(findLayout name IR) 
           handle NoLayout => ()

   fun views names IR = 
       let val layouts = map (fn n => findLayout n IR) names
       in  GraphViewer.view(GraphCombinations.sums layouts)
       end handle NoLayout => ()

   fun viewSubgraph IR subgraph = 
         GraphViewer.view (CFG.subgraphLayout{cfg=IR,subgraph=subgraph})

   (*
    * This function defines how we compute a new view 
    *)

   val verbose = MLRiscControl.getFlag "verbose"

   fun memo name compute = 
   let val {get,set,...} = A.new(SOME(fn _ => name))
       fun getView(IR as G.GRAPH ir : IR)=
       let val CFG.INFO{annotations, ...} = #graph_info ir 
           fun process(SOME(ref(SOME info))) =
                 (if !verbose then print ("[reusing "^name^"]") else (); info)
             | process(SOME r) =
                 let val _    = 
                        if !verbose then print("[computing "^name) else ()
                     val info = compute IR
                     val _    = if !verbose then print "]" else ()
                 in  r := SOME info; info end
           |  process NONE = 
              let val r = ref NONE
                  fun kill() = (r := NONE; 
                                if !verbose then print("[uncaching "^name^"]")
                                else ())
              in  annotations := #create CFG.CHANGED(name, kill) :: 
                                 set(r,!annotations);
                  process(SOME r) 
              end
       in  process(get (!annotations)) end
   in  getView
   end

   (*
    *  Extract various views from an IR
    *) 

   val dom = memo "dom" Dom.makeDominator
   val pdom = memo "pdom" Dom.makePostdominator
   fun doms IR = (dom IR,pdom IR)
   val cdg  = memo "cdg" 
             (fn IR => CDG.control_dependence_graph CFG.cdgEdge (pdom IR))
   val loop = memo "loop" (Loop.loop_structure o dom)
   val changed = CFG.changed 

   (*
    *  Methods to layout various graphs
    *)
   fun defaultEdge _  = [L.COLOR "red"]
   fun defaultGraph _ = []  
   fun layoutDom' IR G = 
   let val {node,...} = CFG.viewStyle IR
   in  L.makeLayout {edge = defaultEdge,
                     graph= defaultGraph,
                     node = node} G
   end
 
   fun layoutDom IR  = layoutDom' IR (dom IR)
   fun layoutPdom IR = layoutDom' IR (pdom IR)
   fun layoutDoms IR = layoutDom' IR
       let val (dom,pdom) = doms IR
       in  GraphCombinations.sum(dom,ReversedGraphView.rev_view pdom)
       end
   fun layoutCDG IR = CFG.viewLayout(cdg IR)
   fun layoutLoop (IR as G.GRAPH cfg) = 
       let val loop   = loop IR
           val an     = !(CFG.annotations IR)
           fun mkNodes nodes =
              String.concat(map (fn i => Int.toString i^" ") nodes)
           fun mkEdges edges = 
              String.concat(map 
                (fn (i,j,_) => Int.toString i^"->"^Int.toString j^" ") edges)
           fun node(_,Loop.LOOP{nesting,header,loop_nodes,
                                backedges,exits,...}) =
               [L.LABEL
                ("nesting: "^Int.toString nesting^"\n"^
                 CFG.show_block an (#node_info cfg header)^
                 "entry edges: "^mkEdges(Loop.entryEdges loop header)^"\n"^
                 "loop_nodes: "^mkNodes loop_nodes^"\n"^
                 "backedges: "^mkEdges backedges^"\n"^
                 "exits: "^mkEdges exits^"\n"
                )]
       in  L.makeLayout {edge=defaultEdge,
                         graph=defaultGraph,
                         node=node} loop
       end
 
   (*
    *  Insert the layout methods here.
    *)
   val _ = addLayout "cfg" CFG.viewLayout
   val _ = addLayout "dom"  layoutDom
   val _ = addLayout "pdom" layoutPdom
   val _ = addLayout "doms" layoutDoms
   val _ = addLayout "cdg"  layoutCDG
   val _ = addLayout "loop" layoutLoop

end

