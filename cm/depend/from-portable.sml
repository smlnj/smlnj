structure FromPortable : sig
    val import : PortableGraph.graph *
		 { grouppath: SrcPath.t,
		   sublibs: GroupGraph.subgrouplist,
		   required: GroupGraph.privileges,
		   wrapped: GroupGraph.privileges,
		   version: Version.t option } ->
		 GroupGraph.group
end = struct

    structure P = PortableGraph
    structure DG = DependencyGraph
    structure GG = GroupGraph

    fun import (P.GRAPH { imports, defs, export }, actuals) = let
	val { grouppath, sublibs, required, wrapped, version } = actuals

	val exports = xxx
	val sources = xxx
    in
	GG.GROUP { exports = exports,
		   kind = GG.LIB { version = version,
				   kind = GG.DEVELOPED { wrapped = wrapped,
							 subgroups = [] },
				   required = required },
		   grouppath = grouppath,
		   sources = sources,
		   sublibs = sublibs }
    end
end
