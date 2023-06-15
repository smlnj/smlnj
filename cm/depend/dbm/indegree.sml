(* cm/depend/dbm/indegree.sml
 *
 * Calculate a map of in-degrees (= # of SNODEs that depend on it) for each SNODE.
 * (The in-degree of a node can serve as a hint for prioritizing compilations during
 * parallel make.)
 *
 * (C) 2023 The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (matthias.blume@gmail.com)
 *)

signature INDEGREE =
sig
  val indegrees : GroupGraph.group -> int SMLInfoMap.map
end (* signature INDEGREE *)

structure Indegree :> INDEGREE =
struct

local

  structure GG = GroupGraph
  structure DG = DependencyGraph
  structure SIM = SmlInfoMap

  type imap = int SIM.map

in

  (* indegrees : GG.group -> imap *)
  fun indegrees GG.ERRORGROUP = SIM.empty
    | indegrees (GG.GROUP { exports: DB.impexp SymbolMap.map, ... }) =
        let 
	    (* fsb : [sn:]((DG.snode * imap) -> imap)
	             -> (DG.farsbnode * imap -> imap *)
	    fun fsb sn ((_, DG.SB_SNODE n), m) = sn (n, m)
	      | fsb _ (_, m) = m

            (* inc_sn : DG.snode * imap -> imap *)
	    fun inc_sn (DG.SNODE { smlinfo = i, ... }, m) =
		SIM.insert (m, i, 1 + getOpt (SIM.find (m, i), 0))

	    (* DG.snode * imap -> imap *)
	    fun do_snode (DG.SNODE { smlinfo, localimports, globalimports }, m) =
		let val m' = case SIM.find (m, smlinfo)
			       of SOME _ => m
			        | NONE => foldl do_snode
						(foldl (fsb do_snode)
						       (SIM.insert (m, smlinfo, 0))
						       globalimports)
						localimports
		 in foldl inc_sn
			  (foldl (fsb inc_sn) m' globalimports)
			  localimports
		end

            (* impexp : DG.impexp * imap -> imap
	     * impexp : ([nth:](unit -> DG.farsbnode) * DAEnv.env * SymbolSet.set) * imap -> imap *)
	    fun impexp ((nth, _, _), m) = fsb do_snode (nth (), m)

	 in SymbolMap.foldl impexp SIM.empty exports
	end

end (* top local *)
end (* structure Indegree *)

(* Comments:

  * [DBM] Does this need to be so complicated?  Hard to believe so.
    The types in DependencyGraph and GroupGraph seem to be highly convoluted.
    I conjecture that simplifications are possible.

*)
