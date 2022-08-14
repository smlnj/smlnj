(* ext.sml
 *
 *   Classifier plug-in for suffixes.
 *
 * Copyright (c) 2007 by The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)
structure LexLexExt = struct
    local
	val suffixes = ["lex", "l"]
	val class = "mllex"
	fun sfx s =
	    Tools.registerClassifier
		(Tools.stdSfxClassifier { sfx = s, class = class })
    in
        val _ = app sfx suffixes
    end
end
