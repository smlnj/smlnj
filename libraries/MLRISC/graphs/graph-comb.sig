(*
 * This module implements some combinators that joins two graphs
 * into a single view.
 *
 *)

signature GRAPH_COMBINATIONS = 
sig

   (* disjoint union *)
   val sum   : ('n,'e,'g) Graph.graph * ('n,'e,'g) Graph.graph ->
                  ('n,'e,'g) Graph.graph
   val union : ('n,'e,'g) Graph.graph list -> ('n,'e,'g) Graph.graph
   val sums  : ('n,'e,'g) Graph.graph list -> ('n,'e,'g) Graph.graph

end

