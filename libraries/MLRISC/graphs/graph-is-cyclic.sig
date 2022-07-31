(*
 * Tests if a graph is cyclie
 *
 * -- Allen
 *)

signature GRAPH_IS_CYCLIC =
sig

      (* cyclic test *)

   val is_cyclic : ('n,'e,'g) Graph.graph -> bool

end

