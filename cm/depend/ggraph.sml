(*
 * Internal data structure representing a CM dependency graph.
 * (coarse-grain: groups)
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure GroupGraph = struct

    type privileges = StringSet.set

    datatype libkind =
	STABLE of unit -> unit		(* pickle dropper *)
      | DEVELOPED of { wrapped: privileges, subgroups: subgrouplist }

    and kind =
	NOLIB of { owner: SrcPath.file option,
		   subgroups: subgrouplist }
      | LIB of { version: Version.t option, kind: libkind }

    (* the "required" field includes everything:
     *   1. privileges required by subgroups
     *   2. newly required privileges
     *   3. privileges that would be wrapped once the group is stabilized
     *)
    and group =
	GROUP of { exports: DependencyGraph.impexp SymbolMap.map,
		   kind: kind,
		   required: privileges,
		   grouppath: SrcPath.file,
		   sources: { class: string, derived: bool } SrcPathMap.map,
		   sublibs: subgrouplist }
      | ERRORGROUP

    withtype subgrouplist =
	(SrcPath.file * (unit -> group) * SrcPath.rebindings) list
    (* Note: "sublibs" consists of items where the SrcPath.file component
     * is equivalent -- but not necessarily identical -- to the "grouppath"
     * component of (the suspended) group.  The group might have
     * been known before -- in which case "grouppath" would carry the
     * path that was used back *then* to refer to the group.  But for
     * the purpose of stabilization we must know the abstract path
     * that was used *this* time. *)
end
