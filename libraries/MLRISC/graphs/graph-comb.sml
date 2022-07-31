(*
 * This module implements some combinators that joins two graphs
 * into a single view.
 *
 *)

structure GraphCombinations : GRAPH_COMBINATIONS = 
struct

   structure G       = Graph
   structure Union   = UnionGraphView
   structure Rename  = RenamedGraphView
   structure Rev     = ReversedGraphView

   (*
    * Disjoint union
    *)
   fun my_union (a,b) = Union.union_view (fn (x,y) => x) (a, b)
   fun sum (A as G.GRAPH a, B as G.GRAPH b) =
      my_union(A, Rename.rename_view (#capacity a ()) B)
   fun union []     = raise G.Graph "union"
     | union [a]    = a
     | union [a,b]  = my_union(a,b)
     | union (a::b) = my_union(a,union b)

   fun sums []     = raise G.Graph "sums"
     | sums [a]    = a
     | sums [a,b]  = sum(a,b)
     | sums (a::b) = sum(a,sums b)

end

