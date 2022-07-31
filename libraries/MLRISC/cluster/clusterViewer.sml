(*
 * This is a simple module for viewing a cluster graph graphically.
 * This is meant to be used only by those of you who don't want to 
 * migrate to the CFG data structure.
 *
 * -- Allen
 *)
functor ClusterViewer
   (structure ClusterGraph : CLUSTER_GRAPH
    structure GraphViewer  : GRAPH_VIEWER
    structure Asm          : INSTRUCTION_EMITTER
       sharing ClusterGraph.F.I = Asm.I 
   ) : CLUSTER_VIEWER =
struct

   structure ClusterGraph = ClusterGraph
   structure F = ClusterGraph.F
   structure W = ClusterGraph.W
   structure L = GraphLayout
   structure FMT = FormatInstruction(Asm)
   structure G = Graph

   val outline = MLRiscControl.getFlag "view-outline"

   fun view(clusterGraph as G.GRAPH cfg) = 
   let val F.CLUSTER{annotations,...} = ClusterGraph.cluster clusterGraph 
       val toString = FMT.toString (!annotations)
       fun graph _ = []

       val red = L.COLOR "red"
       val yellow = L.COLOR "yellow"
       val green = L.COLOR "green"
       val ENTRY = hd(#entries cfg ())
       val EXIT  = hd(#exits cfg ())

       fun edge(i,j,ref w) = 
       let val label = L.LABEL(W.toString w)
           val color =
               if i = ENTRY orelse j = EXIT then green (* special edge *)
               else if i+1 = j then yellow (* fallsthru *)
               else red
       in  [label, color] end

       fun title(blknum,ref freq) = 
           " "^Int.toString blknum^" ("^W.toString freq^")"

       fun ann(annotations) = 
            List.foldl(fn (a,l) => "/* "^Annotations.toString a^" */\n"^l) ""
                             (!annotations)

       fun node(_,F.ENTRY{blknum,freq,...}) = 
              [L.LABEL("entry"^title(blknum,freq)^"\n"^ann(annotations))]
         | node(_,F.EXIT{blknum,freq,...})  = 
              [L.LABEL("exit"^title(blknum,freq))]
         | node(_,F.BBLOCK{annotations,blknum,freq,insns,...}) =
              [L.LABEL(title(blknum,freq)^"\n"^
                 ann(annotations)^
                 (if !outline then "" else
                 List.foldl (fn (i,t) => 
                             let val text = toString i
                             in  if text = "" then t else text^"\n"^t end
                            ) "" (!insns)))]
         | node (_,_) = [L.LABEL "???"]
   in  GraphViewer.view
         (L.makeLayout{graph=graph, edge=edge, node=node} clusterGraph)
   end
          
end
