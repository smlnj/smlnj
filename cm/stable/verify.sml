(*
 * Verifying the validity of an existing stable file for a (non-stable)
 * library.
 *   - This is used for "paranoia" mode during bootstrap compilation.
 *     Normally, CM takes stable files and doesn't ask questions, but
 *     during bootstrap compilation it takes the stable file only if
 *     it is verified to be valid.
 *
 * (C) 2000 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
local
    structure DG = DependencyGraph
    structure GG = GroupGraph
    structure GP = GeneralParams
    structure TS = TStamp
in
signature VERIFY_STABLE = sig
    type exportmap = SmlInfo.info StableMap.map
    val verify' : GP.info -> exportmap
	-> SrcPath.file *		(* grouppath *)
	   DG.sbnode list *		(* export_nodes *)
	   GG.subgrouplist *		(* sublibs *)
	   SrcPathSet.set *		(* groups *)
	   Version.t option
	-> bool
    val verify : GP.info -> exportmap -> GG.group -> bool
end

functor VerStabFn (structure Stabilize: STABILIZE) :> VERIFY_STABLE = struct

    type exportmap = SmlInfo.info StableMap.map

    fun verify' (gp: GP.info) em args = let
	val (grouppath, export_nodes, sublibs, groups, version) = args
	val groups = SrcPathSet.add (groups, grouppath)
	val policy = #fnpolicy (#param gp)
	val stablename =
	    FilenamePolicy.mkStableName policy (grouppath, version)

	fun invalidMember stab_t i = let
	    val p = SmlInfo.sourcepath i
		    
	    val bn = SmlInfo.binname i
	in
	    case (SrcPath.tstamp p, TS.fmodTime bn) of
		(TS.TSTAMP src_t, TS.TSTAMP bin_t) =>
		    Time.compare (src_t, bin_t) <> EQUAL orelse
		    Time.compare (src_t, stab_t) = GREATER
	      | _ => true
	end

	fun nonstabSublib (_, gth, _) =
	    case gth () of
		GG.GROUP { kind = GG.LIB { kind = GG.STABLE _,
					   ... }, ... } => false
	      | _ => true

	fun invalidGroup stab_t p =
	    case SrcPath.tstamp p of
		TS.TSTAMP g_t => Time.compare (g_t, stab_t) = GREATER
	      | _ => true

	val validStamp = Stabilize.libStampIsValid gp

	val isValid =
	    case TS.fmodTime stablename of
		TS.TSTAMP st => let
		    val (m, i) = Reachable.reachable' export_nodes
		in
		    (* The group itself is included in "groups"... *)
		    not (SrcPathSet.exists (invalidGroup st) groups)
		    andalso
		    not (List.exists nonstabSublib sublibs)
		    andalso
		    validStamp ((grouppath, export_nodes, sublibs), version)
		    andalso
		    not (SmlInfoSet.exists (invalidMember st) m)
		end
	      | _ => false
    in
	if not isValid then
	    OS.FileSys.remove stablename handle _ => ()
	else ();
	isValid
    end

    fun verify _ _ GG.ERRORGROUP = false
      | verify gp em (group as GG.GROUP g) = let
	    val { exports, grouppath, sublibs, kind, ... } = g
	    val groups = Reachable.groupsOf group
	    val version =
		case kind of
		    GG.NOLIB _ => NONE
		  | GG.LIB { version, ... } => version
	    fun force f = f ()
	in
	    verify' gp em (grouppath,
			   map (#2 o force o #1) (SymbolMap.listItems exports),
			   sublibs, groups, version)
	end
end
end
