(*
 * Running ML-Yacc from CM.
 *
 *   (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure YaccTool = struct
    local
	val tool = "ML-Yacc"
	val kw_sigopts = "sigoptions"
	val kw_smlopts = "smloptions"
	val kwl = [kw_sigopts, kw_smlopts]
	(* This is a bit clumsy because we call parseOptions twice.
	 * However, this is not really such a big deal in practice... *)
	fun get kw NONE = NONE
	  | get kw (SOME opts) =
	    #matches (Tools.parseOptions
			  { tool = tool, keywords = kwl, options = opts }) kw
    in
        val _ = Tools.registerStdShellCmdTool
		    { tool = tool,
		      class = "mlyacc",
		      cmdStdPath = fn () => ("ml-yacc", []),
		      template = NONE,
		      extensionStyle =
		      Tools.EXTEND [("sig", SOME "sml", get kw_sigopts),
				    ("sml", SOME "sml", get kw_smlopts)],
		      dflopts = [] }
    end
end
