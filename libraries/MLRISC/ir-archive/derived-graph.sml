(*
 * Compute Tarjan's dominator derived graph from a dominator tree.
 * This is used partly to computing path expressions.  Alternatively,
 * it can also be used for testing for reducibility.  In particular,
 * cycles involving more than one node represent irreducible loops
 * in the flow graph.
 *
 * -- Allen
 *)

functor DerivedGraph(Dom : DOMINATOR_TREE): DERIVED_GRAPH =
struct
   structure Dom = Dom
   structure G   = Graph
   structure GI  = Dom.GI
   structure A   = Array

   type ('n,'e) derived_graph = ('n,'e Graph.edge,unit) Graph.graph

   fun derived_graph (Dom as G.GRAPH dom) =
   let val N              = #capacity dom ()
       val D as G.GRAPH d = GI.graph("derived graph",(),N) 
       val G.GRAPH cfg    = Dom.cfg Dom
       val ancestors      = A.array(Dom.max_levels Dom,0)
       val levelsMap      = Dom.levelsMap Dom
       fun dfs lvl i = 
       let val _ = A.update(ancestors,lvl,i)
           val _ = #add_node d (i,#node_info cfg i)
           fun add_edge (e as (i,j,_)) =
               let val level = A.sub(levelsMap,j)
               in if lvl < level then 
                     #add_edge d (i,j,e)  (* i idom j ! *)
                  else
                     #add_edge d (A.sub(ancestors,level),j,e)
               end
       in  app add_edge (#out_edges cfg i);
           app (dfs (lvl+1)) (#succ dom i)
       end
       
   in  app (dfs 0) (#entries dom ());
       #set_entries d (#entries dom ());
       D
   end
   
end

