(* mllex-tool.sml
 *
 * Plugin for the "mllex" CM tool class that causes "legacy" ml-lex
 * input to be processed by "ml-ulex --ml-lex-mode".
 *
 * Copyright (c) 2007 by The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)
structure ULexMLLexTool = struct
    val _ = Tools.registerStdShellCmdTool
	{ tool = "ULex-ML-Lex",
	  class = "mllex",
	  cmdStdPath = fn () => ("ml-ulex", ["--ml-lex-mode"]),
	  template = NONE,
	  extensionStyle = Tools.EXTEND [("sml", SOME "sml", fn too => too)],
	  dflopts = [] }
end
