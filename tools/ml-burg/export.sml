(* ml-burg/export.sml *)

structure ExportTool =
struct

  val _ = SMLofNJ.exportFn ("ml-burg", Main.main)

end (* structure ExportTool *)
