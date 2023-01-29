(* ml-ulex/export.sml *)

structure ExportTool =
struct

  val _ = SMLofNJ.exportFn ("ml-ulex", Main.main)

end (* structure ExportMlulex *)
