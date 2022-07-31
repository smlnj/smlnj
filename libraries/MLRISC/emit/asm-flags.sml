structure AsmFlags = struct
    val show_cellset = MLRiscControl.mkFlag
			   ("asm-show-cellset",
			    "whether to show cellsets in asm")
    val show_region = MLRiscControl.mkFlag
			  ("asm-show-region",
			   "whether to show regions in asm")
    val show_cutsTo = MLRiscControl.mkFlag
			  ("asm-show-cutsto",
			   "whether to show cutsTo in asm")
    val indent_copies = MLRiscControl.mkFlag
			    ("asm-indent-copies",
			     "whether to indent copies in asm")
end
