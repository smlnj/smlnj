(* ml-yacc/export.sml *)

structure ExportTool =
struct

  val _ = SMLofNJ.exportFn ("heap2asm", Main.main)

end (* structure ExportTool *)
