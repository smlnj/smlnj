(* ml-yacc/export.sml *)

structure ExportTool =
struct

  val _ = SMLofNJ.exportFn ("ml-yacc", ExportParseGen.parseGen)

end (* structure ExportTool *)
