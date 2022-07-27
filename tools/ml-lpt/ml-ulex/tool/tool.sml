(*
 * Running ML-ULex from CM.
 *
 * (C) 2007 The Fellowship of SML/NJ.
 *)
structure ULexTool = struct
    val _ = Tools.registerStdShellCmdTool {
	      tool = "ML-ULex",
	      class = "ml-ulex",
	      cmdStdPath = fn () => ("ml-ulex", []),
	      template = NONE,
	      extensionStyle =
	        Tools.EXTEND [("sml", SOME "sml", fn too => too)],
	      dflopts = []
	    }
end
