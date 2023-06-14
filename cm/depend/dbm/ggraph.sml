(* cm/depend/dbm/ggraph.sml
 *
 * Internal data structure representing a CM dependency graph.
 * (coarse-grain: groups)
 *
 * (C) 2023 The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (matthias.blume@gmail.com)
 *)
structure GroupGraph =
struct

  datatype libkind
    = STABLE of unit -> unit		(* pickle dropper *)
    | DEVELOPED of subgroup list

  and kind
    = NOLIB of { owner: File.file option, subgroups: subgroup list }
    | LIB of libkind

  and group =
      GROUP of { exports: DependencyGraph.impexp SymbolMap.map,
		 kind: kind,
		 grouppath: File.file,  (* or Path.path? *)
		 sources: { class: string, derived: bool } FileMap.map,
		 sublibs: subgroup list }
    | ERRORGROUP

  (* File.file or Path.path? *)
  withtype subgroup = File.file * (unit -> group) * AnchorEnv.anchorPathAlist

  (* Note: "sublibs" consists of items where the File.file component
   * is equivalent -- but not necessarily identical -- to the "grouppath"
   * component of (the suspended) group.  The group might have
   * been known before -- in which case "grouppath" would carry the
   * path that was used back *then* to refer to the group.  But for
   * the purpose of stabilization we must know the abstract path
   * that was used *this* time. *)

end (* structure GroupGraph *)
