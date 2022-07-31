(*
 * View a scheduler DDG constructed for basic block scheduling
 *
 * -- Allen
 *)
functor BasicBlockSchedulerDDGViewer
   (structure GraphViewer : GRAPH_VIEWER
    structure DDG         : SCHEDULER_DDG
    structure FormatInsn  : FORMAT_INSTRUCTION
      sharing FormatInsn.I = DDG.I 
   ) : BASIC_BLOCK_SCHEDULER_DDG_VIEWER =
struct

   structure DDG = DDG
   structure I   = DDG.I
   structure L   = GraphLayout

   val edgeColor = L.COLOR "red"

   val toString = FormatInsn.toString [] 

   fun view ddg =
       GraphViewer.view 
         (GraphLayout.makeLayout
           {graph = fn _ => [],
            node  = fn (_,i) => [L.LABEL(toString i)],
            edge  = fn (_,_,lat) => [L.LABEL(Int.toString lat),edgeColor] 
           }
           ddg
         )

end
