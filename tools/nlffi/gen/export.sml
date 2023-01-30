(* nlffi/gen/export.sml *)

structure ExportTool =
struct

  val _ = SMLofNJ.exportFn ("ml-nlffigen", Main.main)

end (* structure ExportTool *)
