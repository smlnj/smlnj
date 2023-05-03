(* ml-antlr/export.sml *)

structure ExportTool =
struct

  val _ = SMLofNJ.exportFn ("ml-antlr", Main.main)

end (* structure ExportTool *)
