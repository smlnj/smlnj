(*
 * Running ML-Antlr from CM.
 *
 * (C) 2007 The Fellowship of SML/NJ.
 *)
structure AntlrTool = struct
    val _ = Tools.registerStdShellCmdTool {
	      tool = "ML-Antlr",
	      class = "ml-antlr",
	      cmdStdPath = fn () => ("ml-antlr", []),
	      template = NONE,
	      extensionStyle =
	        Tools.EXTEND [("sml", SOME "sml", fn too => too)],
	      dflopts = []
	    }
end
