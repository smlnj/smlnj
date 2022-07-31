(*
 * The hook module for the dynamically-linked CMB "slave" stub.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure CMBSlaveHook = struct
    local
	type slave =
	    (string * string) option ->
	    (GroupGraph.group *
	     (DependencyGraph.sbnode -> bool) *
	     SrcPath.env) option
	val m = ref (StringMap.empty: slave StringMap.map)
    in
	fun init arch f = m := StringMap.insert (!m, arch, f)
	fun slave arch s =
	    case StringMap.find (!m, arch) of
		NONE => NONE
	      | SOME f => f s
    end
end
