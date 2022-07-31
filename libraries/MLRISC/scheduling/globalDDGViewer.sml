(*
 * View a scheduler DDG constructed for basic block scheduling
 *
 * -- Allen
 *)
functor GlobalSchedulerDDGViewer
   (structure GraphViewer : GRAPH_VIEWER
    structure IR          : MLRISC_IR
    structure DDG         : SCHEDULER_DDG
    structure FormatInsn  : FORMAT_INSTRUCTION
      sharing IR.I = FormatInsn.I = DDG.I 
   ) : GLOBAL_SCHEDULER_DDG_VIEWER =
struct

   structure IR  = IR
   structure DDG = DDG
   structure I   = DDG.I
   structure L   = GraphLayout

   val edgeColor = L.COLOR "red"

   val i2s = Int.toString


   fun view IR ddg =
   let val regmap = IR.CFG.regmap IR
       val toString = FormatInsn.toString [] (I.C.lookup regmap)
   in  GraphViewer.view 
         (GraphLayout.makeLayout
           {graph = fn _ => [],
            node  = fn (_,DDG.NODE{instr,b,...}) => 
                         [L.LABEL("["^i2s b^"] "^toString instr)],
            edge  = fn (_,_,e) => [L.LABEL(DDG.edgeToString e), edgeColor]
           }
           ddg
         )
   end

end
