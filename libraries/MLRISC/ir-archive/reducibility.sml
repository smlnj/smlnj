(*
 * This module tests for reducibility of a loop
 *
 * -- Allen
 *)
functor Reducibility(Loop : LOOP_STRUCTURE) : REDUCIBILITY = 
struct
   structure Loop = Loop
   structure Dom  = Loop.Dom
   structure G    = Graph

   structure Derived = DerivedGraph(Dom)

   fun is_reducible(Loop) =
   let val Dom = Loop.dom Loop
       val headers = Loop.header Loop
       val Derived as G.GRAPH derived = Derived.derived_graph Dom   
       val N = #capacity derived ()
       val irreducible = BitSet.create N
       fun markIrreducible([_],_) = () (* simple cycles are reducible *)
         | markIrreducible(cycle,_) = 
           app (fn n => BitSet.set(irreducible,n)) cycle
       val _ = GraphSCC.scc Derived markIrreducible ()
       fun isReducible n =
       let val h = Array.sub(headers,n)
       in  not(BitSet.contains(irreducible,n)) end 
   in  isReducible 
   end
end
