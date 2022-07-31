(*
 * Cleanup-code to be executed at start-up.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure Cleanup = struct
    local
	structure C = SMLofNJ.Internals.CleanUp
	val label = "CM-Cleanup"
	fun atInit initPaths _ =
	    (ControlRegistry.init BasicControl.topregistry;
	     initPaths ())
    in
	fun install initPaths =
	    ignore (C.addCleaner (label, [C.AtInit], atInit initPaths))
	fun uninstall () = ignore (C.removeCleaner label)
    end
end
