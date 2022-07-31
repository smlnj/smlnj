structure SMLCopyTool : sig end = struct
    val _ = Tools.registerStdShellCmdTool
	    { tool = "SML-Copy", class = "smlcopy", suffixes = ["cp"],
	      cmdStdPath = "cp", template = SOME "%c %s %1t",
	      extensionStyle = Tools.EXTEND [("sml", SOME "sml", fn x => x)],
	      dflopts = [] }
end
