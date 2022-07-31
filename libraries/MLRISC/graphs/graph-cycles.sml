(*
 * Enumerate all simple cycles in a graph with no duplicates.
 *
 * This module enumerates all simple cycles in a graph.
 * Each cycle is reprensented as a list of edges.  Adjacent edges
 * are adjacent in the list.  The function works like fold: all cycles
 * are ``folded'' together with a user supplied function.
 *
 * -- Allen
 *)

structure GraphCycles : GRAPH_SIMPLE_CYCLES =
struct

   structure G = Graph
   structure A = Array

   fun cycles (graph as G.GRAPH G) f x =
   let val N = #capacity G ()
       val inSCC = A.array(N,(~1,0))
       val inCycle = A.array(N,false)
       
       fun processSCC(scc,x) =
       let val witness = hd scc
               (* order each node in the scc *)
           fun init([],_) = ()
             | init(u::us,i) = (A.update(inSCC,u,(witness,i)); init(us,i+1))

           fun dfs(n,root,u,cycle,x) = dfsSucc(n,root,#in_edges G u,cycle,x)
           and dfsSucc(_,_,[],_,x) = x
             | dfsSucc(n,root,(e as (v,u,_))::es,cycle,x) =
               if root = v then dfsSucc(n,root,es,cycle,f(e::cycle,x))
               else let val (w,m) = A.sub(inSCC,v)
                    in  if w <> witness orelse m <= n orelse A.sub(inCycle,v) 
                        then dfsSucc(n,root,es,cycle,x)
                        else let val _ = A.update(inCycle,v,true)
                                 val x = dfs(n,root,v,e::cycle,x)
                                 val _ = A.update(inCycle,v,false)
                             in  dfsSucc(n,root,es,cycle,x)
                             end
                    end

           fun hasBackEdge([],n) = false
             | hasBackEdge((v,_,_)::es,n) =
                let val (w,m) = A.sub(inSCC,v)
                in  w = witness andalso m >= n orelse hasBackEdge(es,n) end 

           fun enumerateAll(_,[],x) = x
             | enumerateAll(n,u::us,x) =
               let val x = if hasBackEdge(#in_edges G u,n) 
                           then dfs(n,u,u,[],x) else x
               in  enumerateAll(n+1,us,x)
               end
       in  init(scc,0);
           enumerateAll(0,scc,x)
       end
       
   in  GraphSCC.scc graph processSCC x
   end

end

