(* cm/depend/dbm/reachable.sml
 *
 * Get the set of reachable SNODEs in a given dependency graph.
 *
 * (C) 2023 The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (matthias.blume@gmail.com)
 * Edited: DBM 2023.6
 *)

signature REACHABLE =
sig

    (* These two functions simply give you the set of (non-stable)
     * modules reachable from some root and the fringe of stable
     * modules that surrounds the non-stable portion. *)
    val reachable' : DependencyGraph.sbnode list -> SmlInfoSet.set * StableSet.set
    val reachable : GroupGraph.group -> SmlInfoSet.set * StableSet.set

    (* "snodeMap" gives us handles at arbitrary points within the (non-stable)
     * portion of a dependency graph.
     * This is used by "slave" mode compiler. *)
    val snodeMap : GroupGraph.group -> DependencyGraph.snode FileMap.map

    (* Given a library (or group) g, "groupsOf g" gets the set of
     * subgroups (but not sub-libraries) of that group.  The result
     * will include the argument itself. *)
    val groupsOf : GroupGraph.group -> FileSet.set

    (* Given an arbitrary group graph rooted at group g, "stableLibsOf g"
     * gets the set of stable libraries reachable from g. *)
    val stableLibsOf : GroupGraph.group -> GroupGraph.group FileMap.map

    (* Given a "closed" subset of (non-stable) nodes in a dependency graph,
     * "frontier" gives you the set of frontier nodes of that set.  The
     * closed set is given by its indicator function (first argument).
     * ("closed" means that if a node's ancestors are all in
     * the set, then so is the node itself.  A frontier node is a node that
     * is in the set but either not all of its ancestors are or the node
     * is an export node.) *)
    val frontier : (BinInfo.info -> bool) -> GroupGraph.group -> StableSet.set

end (* signature REACHABLE *)

structure Reachable :> REACHABLE =
struct

local (* top local *)

  structure DG = DependencyGraph
  structure GG = GroupGraph

in

  (* type of 1st argument of reach, the interface of a "collection" 'm (set or map) *)
  type 'm rcha =
       {add : 'm * SmlInfo.info * DG.snode -> 'm,
	member : 'm * SmlInfo.info -> bool,
	empty : 'm}

  (* return type of reach *)
  type 'c rchr = 'c * StableSet.set

  (* return type of snodeMap *)
  type snmapTy = DG.snode FileMap.map

  (* reach : 'm rcha -> DG.sbnode list -> 'm rchr *)
  (* m' is instantiated to
   *   DG.snode FileMap.map in snodeMap'
   *   SmlInfoSet.set in reachable' *)
  fun reach ({add, member, empty}: 'm rcha} (export_nodes: DG.sbnode list): 'm rchr =
      let
	  (* f_snode : DG.snode * 'm rchr -> 'm rchr *)
	  fun f_snode (x as DG.SNODE {smlinfo: SmlInfo.info,
				      localimports: DG.snode list,
				      globalimports: DG.farsbnode list},
		     (known: 'm, stabfringe: StableSet.set)) =
	      if member (known, smlinfo)
	      then (known, stabfringe)
	      else foldl (f_farsbnode : DG.farsbnode * 'm rchr -> 'm rchr)
		         (foldl f_snode (add (known, smlinfo, x), stabfringe) localimports)
			 (globalimports: farsbnode list)

	  (* f_farsbnode : DG.farsbnode * 'm rchr -> 'm rchr *)
	  and f_farsbnode ((_, n: DG.sbnode): DG.farsbnode, ksf: 'm rchr) : 'm rchr =
	        f_sbnode (n, ksf)

          (* f_sbnode : DG.sbnode * 'm rchr -> 'm rchr *)
	  and f_sbnode (DG.SB_BNODE (DG.BNODE {bininfo,...}, _, _), (known, stabfringe)) =
	        (known, StableSet.add (stabfringe, bininfo))
	    | f_sbnode (DG.SB_SNODE n, m) = f_snode (n, m)

       in foldl f_sbnode (empty, StableSet.empty) export_nodes
      end

  (* snodeMap' : (DG.impexp SymbolMap.map * snmapTy -> snmapTy rchr *)
  fun snodeMap' (exports: DG.impexp SymbolMap.map, acc: snmapTy) =
      let (* add : snmapTy * SmlInfo.info * DG.snode -> snmapTy *)
	  fun add (m: snmapTy, info, snode) = FileMap.insert (m, SmlInfo.file info, snode)
	  (* member : snmapTy * SmlInfo.info -> bool *)
	  fun member (m: snmapTy, info: SmlInfo.info) =
	      FileMap.inDomain (m, SmlInfo.file info)
       in #1 (reach { add = add, member = member, empty = acc }
		    (map (#2 o #1) (SymbolMap.listItems exports)))
      end

  (* reachable' : DG.sbnode list -> SmlInfoSet.set rchr *)
  val reachable' =
      reach { add = fn (s, i, _) => SmlInfoSet.add (s, i),
	      member = SmlInfoSet.member,                                (* set *)
	      empty = SmlInfoSet.empty }

  (* reachable : GG.group -> SmlInfoSet.set rchr *)
  fun reachable (GG.GROUP { exports, ... }) =
	reachable' (map (#2 o #1) (SymbolMap.listItems exports))
    | reachable GG.ERRORGROUP = (SmlInfoSet.empty, StableSet.empty)


  (* return type of snm *)
  type snmr = snmapTy * FileSet.set

  (* snodeMap : GG.group -> snmapTy *)
  fun snodeMap (group: GG.group) : snmapTy =
      let (* snm : GG.group * snmr -> snmr *)
	  fun snm (GG.ERRORGROUP, x: snmr) = x
	    | snm (GG.GROUP { file, exports, sublibs, ... },
		   (snodeMap, knownFiles): snmr) =
	        if FileSet.member (knownFiles, file)
		then (snodeMap, knownFiles)
		else foldl (fn ((_, g, _), x) => snm (g, x))
			   (snodeMap' (exports, knownFiles), FileSet.add (knownFiles, file))
			   sublibs

       in #1 (snm (group, (FileMap.empty, FileSet.empty)))
      end

  (* groupsOf : GG.group -> FileSet.set *)
  fun groupsOf (group: GG.group) : FileSet.set =
      let (* subgroups : GG.group -> GG.subgroup list *)
	  fun subgroups (GG.GROUP { kind = GG.NOLIB {subgroups, ...}, ... }) = subgroups
	    | subgroups (GG.GROUP { kind = GG.LIB { kind = GG.DEVELOPED subgroups, ... }, ... }) =
		subgroups
	    | subgroups _ = nil

	  (* go : GG.group * FileSet.set -> FileSet.set *)
	  fun go (GG.ERRORGROUP, fileset) = fileset
	    | go (g as GG.GROUP { file, ... }, fileset) =
		let val sgl = subgroups g
		    (* sl : DG.subgroup * FileSet.set -> FileSet.set *)
		    fun sl ((file', group', _): DG.subgroup, fileset) : fileset =
			(case group'
			   of GG.GROUP { kind = GG.NOLIB _, ... } =>
			        if FileSet.member (fileset, file')
			        then fileset
				else go (group', fileset)
			    | _ => fileset)

		 in FileSet.add (foldl sl fileset sgl, file)
		end

       in go (group, FileSet.empty)
      end

  (* stableLibsOf : GG.group -> snmapTy *)
  fun stableLibsOf GG.ERRORGROUP = FileMap.empty
    | stableLibsOf (g as GG.GROUP { grouppath, ... }) =
	let fun slo' ((_, GG.ERRORGROUP), x) = x
	      | slo' ((p, g as GG.GROUP grec), (seen, res)) =
		  let val { kind, sublibs, ... } = grec
		   in if FileSet.member (seen, p)
		      then (seen, res)
		      else let val (seen, res) = foldl slo (seen, res) sublibs
			       val seen = FileSet.add (seen, p)
			    in case kind
				 of GG.LIB { kind = GG.STABLE _, ... } =>
				    (seen, FileMap.insert (res, p, g))
				  | _ => (seen, res)
			   end
		  end
	    and slo ((p, gth, _), x) = slo' ((p, gth ()), x)
	 in #2 (slo' ((grouppath, g), (FileSet.empty, FileMap.empty)))
	end

  (* frontier : [inSet:](BinInfo.info -> bool) -> GG.group -> StableSet.set *)
  fun frontier _ GG.ERRORGROUP = StableSet.empty
    | frontier inSet (GG.GROUP { exports: DG.impexp SymbolMap.map, ... }) =
	let (* f_bnode : DG.bnode * (StableSet.set * StableSet.set)
	                 -> (StableSet.set *StableSet.set) *)
	    fun f_bnode (DG.BNODE {bininfo, localimports, ...}, (seen, f)) =
		  if StableSet.member (seen, bininfo)
		  then (seen, f)
		  else let val seen = StableSet.add (seen, bininfo)
		        in if inSet bininfo
			   then (seen, StableSet.add (f, bininfo))
			   else foldl f_bnode (seen, f) localimports
		       end

	    (* get_bn : impexp * DG.bnode list -> DG.bnode list *)
	    fun get_bn (((_, DG.SB_BNODE (n, _, _)), _, _): DG.impexp, bnl: DG.bnode list) =
		  n :: bnl
	      | get_bn (_, bln) = bln

	    val bnl: DG.bnode list = SymbolMap.foldl get_bn nil exports

	 in #2 (foldl f_bnode (StableSet.empty, StableSet.empty) bnl)
	end

end (* top local *)
end (* structure Reachable *)
