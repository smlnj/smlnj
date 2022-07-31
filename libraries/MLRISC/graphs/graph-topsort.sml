(*
 * This module computes a topological sort of a graph
 *
 * -- Allen
 *)

structure GraphTopsort : GRAPH_TOPOLOGICAL_SORT = 
struct

   structure G = Graph

   (*
    * Topological sort
    *)
   fun topsort (G.GRAPH G) roots = 
   let val visited = Word8Array.array(#capacity G (),0w0)
       val succ    = #succ G
       fun dfs (n, list) =
          if Word8Array.sub(visited,n) <> 0w0 then list
          else (Word8Array.update(visited,n,0w1); dfs'(n,succ n,list))
       and dfs'(x,[],list)    = x::list
         | dfs'(x,n::ns,list) = dfs'(x,ns,dfs(n,list))
       and dfs''([], list)    = list
         | dfs''(n::ns, list) = dfs''(ns,dfs(n,list))
   in
       dfs''(roots,[])
   end

end

