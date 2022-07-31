(*
 * Interface of an actual graph implementation
 *
 * -- Allen
 *)

signature GRAPH_IMPLEMENTATION =
sig

   val graph : string * 'g * int -> ('n,'e,'g) Graph.graph

end

