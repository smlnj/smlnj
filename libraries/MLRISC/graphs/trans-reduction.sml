(*
 * Compute the (in place) transitive reduction of a graph.
 *
 * -- Allen
 *)

signature TRANSITIVE_REDUCTION =
sig
   val transitive_reduction : ('n,'e,'g) Graph.graph -> unit
end

structure TransitiveReduction : TRANSITIVE_REDUCTION =
struct
   structure G = Graph
   fun transitive_reduction (G.GRAPH G) =
   let
   in
   end

end

