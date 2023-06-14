(* cm/depend/dbm/from-portable.sml
 * 
 * (C) 2023 The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (matthias.blume@gmail.com)
 *)

(* UNFINISHED! -- hence not used *)

structure FromPortable :
  sig
    val import : PortableGraph.graph *
		 { grouppath: Path.path,  (* or File.file? *)
		   sublibs: GroupGraph.subgroup list} ->
		 GroupGraph.group
   end =

struct

local (* top local *)
    
  structure P = PortableGraph
  structure DG = DependencyGraph
  structure GG = GroupGraph

in (* top local *)

  fun import (P.GRAPH { imports, defs, export }, actuals) =
      let val { grouppath, sublibs } = actuals
	  val exports = xxx  (* ??? *)
	  val sources = xxx  (* ??? *)
       in GG.GROUP { exports = exports,
		     kind = GG.LIB (GG.DEVELOPED nil),
		     grouppath = grouppath,
		     sources = sources,
		     sublibs = sublibs }
  end

end (* top local *)
end (* structure FromPortable *)
