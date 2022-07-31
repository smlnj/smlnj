(*
 * The classifier for the tool for processing filesystem directories that
 * contain other source files.
 * (We keep this classifier separate to be able to register it without
 * registering the tool itself.  This classifier does not rely on filename
 * suffixes, so we cannot rely on automatic loading of plugins.)
 *
 *   (C) 2000 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure DirToolClassify = struct
    val class = "dir"
    local
	open Tools
	fun classify { name, mkfname } =
	    (if OS.FileSys.isDir (mkfname ()) then SOME class
	     else NONE)
	    handle _ => NONE
    in
        val _ = registerClassifier (GEN_CLASSIFIER classify)
    end
end
