(* ml-ulex-tool.sml
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * The plugin library for ML-ULex
 *)

structure ULexTool =
  struct

    val _ = Tools.registerStdShellCmdTool {
	      tool = "ML-ULex",
	      class = "ml-ulex",
	      cmdStdPath = fn () => ("ml-ulex", []),
	      template = NONE,
	      extensionStyle = Tools.EXTEND [("sml", SOME "sml", fn too => too)],
	      dflopts = []
	    }

  end
