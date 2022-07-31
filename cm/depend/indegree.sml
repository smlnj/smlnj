(*
 * Calculate a map of in-degrees (= # of SNODEs that depend on it) for each
 * SNODE.
 * (The in-degree of a node can serve as a hint for prioritizing
 *  compilations during parallel make.)
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
local
    structure GG = GroupGraph
    structure DG = DependencyGraph
    structure M = SmlInfoMap
in
    signature INDEGREE = sig
	val indegrees : GG.group -> int M.map
    end

    structure Indegree :> INDEGREE = struct
	
	fun indegrees GG.ERRORGROUP = M.empty
	  | indegrees (GG.GROUP { exports, ... }) = let
	    fun fsb sn ((_, DG.SB_SNODE n), m) = sn (n, m)
	      | fsb _ (_, m) = m

	    fun inc_sn (DG.SNODE { smlinfo = i, ... }, m) =
		M.insert (m, i, 1 + getOpt (M.find (m, i), 0))

	    fun snode (DG.SNODE n, m) = let
		val { smlinfo = i, localimports = li, globalimports = gi } = n
		val m =
		    case M.find (m, i) of
			SOME _ => m
		      | NONE => foldl snode
			              (foldl (fsb snode)
				             (M.insert (m, i, 0))
					     gi)
			              li
	    in
		foldl inc_sn (foldl (fsb inc_sn) m gi) li
	    end

	    fun impexp ((nth, _, _), m) = fsb snode (nth (), m)
	in
	    SymbolMap.foldl impexp M.empty exports
	end
    end
end
