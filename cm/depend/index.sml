(* index.sml
 *
 * Generating indices mapping top-level defined symbols to files
 * where they are defined.
 *
 *   Copyright (c) 2001 by Lucent Technologies, Bell Laboratories
 *
 * author: Matthias Blume (blume@research.bell-labs.com)
 *)
signature INDEX = sig
    val mkIndex :
	GeneralParams.info *
	SrcPath.file *
	{ imports: DependencyGraph.impexp SymbolMap.map,
	  smlfiles: 'b,
	  localdefs: SmlInfo.info SymbolMap.map,
	  subgroups:
	       (SrcPath.file * GroupGraph.group * SrcPath.rebindings) list,
	  sources: 'c,
	  reqpriv: 'd }
	-> unit
end

structure Index :> INDEX = struct
    fun mkIndex (gp: GeneralParams.info, group, coll) =
	if #get StdConfig.generate_index () then
	    let val { imports, smlfiles, localdefs, subgroups,
		      sources, reqpriv } = coll
		val idxfile =
		    FilenamePolicy.mkIndexName (#fnpolicy (#param gp)) group
		fun localinfo i =
		    (SrcPath.osstring_relative (SmlInfo.sourcepath i), false)
		fun globalinfo (sy, _) = let
		    fun find [] =
			ErrorMsg.impossible "index.sml: globalinfo not found"
		      | find ((g, GroupGraph.GROUP { exports, ... }, _) :: r) =
			if SymbolMap.inDomain (exports, sy) then
			    (SrcPath.descr g, true)
			else find r
		      | find (_ :: r) = find r
		in
		    find subgroups
		end
		val l_idx = SymbolMap.map localinfo localdefs
		val g_idx = SymbolMap.mapi globalinfo imports
		fun combine ((l, _), (g, _)) =
		    (concat [l, " (overrides ", g, ")"], false)
		val idx = SymbolMap.unionWith combine (l_idx, g_idx)
		fun oneline (sy, (s, f), l) =
		    (concat [Symbol.nameSpaceToString
				 (Symbol.nameSpace sy),
				 " ", Symbol.name sy, ": ", s, "\n"], f)
		    :: l
		val l = SymbolMap.foldli oneline [] idx
		fun gt ((_, true), (_, false)) = true
		  | gt ((_, false), (_, true)) = false
		  | gt ((x: string, _), (y, _)) = x > y
		val sorted_l = ListMergeSort.sort gt l
		fun work str = let
		    fun out x = TextIO.output (str, x)
		    fun bottomhalf [] = ()
		      | bottomhalf ((x, _) :: r) = (out x; bottomhalf r)
		    fun tophalf [] = ()
		      | tophalf ((x, false) :: r) = (out x; tophalf r)
		      | tophalf ((x, true) :: r) =
			(out "--------------IMPORTS--------------\n";
			 out x;
			 bottomhalf r)
		in
		    out "---------LOCAL DEFINITIONS---------\n";
		    tophalf sorted_l
		end
	    in
		SafeIO.perform { openIt = fn () => AutoDir.openTextOut idxfile,
				 closeIt = TextIO.closeOut,
				 work = work,
				 cleanup = fn _ => OS.FileSys.remove idxfile }
	    end
	else ()
end
