(* ext.sml
 *
 *   Plugin for registering classifiers.
 *
 * Copyright (c) 2007 by The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)
structure YaccGrmExt = struct
    local
	val suffixes = ["grm", "y"]
	val class = "mlyacc"
	fun sfx s =
	    Tools.registerClassifier
		(Tools.stdSfxClassifier { sfx = s, class = class })
    in
        val _ = app sfx suffixes
    end
end
