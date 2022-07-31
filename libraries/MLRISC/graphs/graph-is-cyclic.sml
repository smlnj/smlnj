(*
 * Tests if a graph is cyclic
 *
 * -- Allen
 *)

structure GraphIsCyclic : GRAPH_IS_CYCLIC = 
struct

   structure G = Graph

   exception Cyclic

   (*
    * Cyclic test
    *)
   fun is_cyclic (G.GRAPH G) = 
   let val N       = #capacity G () 
       val visited = BitSet.create N
       val done    = BitSet.create N
       fun dfs i =
          if BitSet.markAndTest(visited,i) then
             if BitSet.contains(done,i) then ()
             else raise Cyclic
          else 
             (dfsSucc(#out_edges G i);
              BitSet.set(done,i))
       and dfs'(i,_) = dfs i
       and dfsSucc [] = ()
         | dfsSucc((_,j,_)::es) = (dfs j; dfsSucc es)
   in
       (#forall_nodes G dfs'; false) handle Cyclic => true
   end

end

