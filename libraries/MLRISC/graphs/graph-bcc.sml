(*
 *  Tarjan's algorithm for computing biconnected components.
 *
 *  -- Allen
 *)
structure GraphBCC : GRAPH_BICONNECTED_COMPONENTS =
struct

   structure G = Graph
   structure A = Array

   fun biconnected_components (G.GRAPH G) process S =
   let val N      = #capacity G ()
       val dfsnum = A.array(N,~1)  
       val low    = A.array(N,~1)  
       fun dfsRoots([],stack,n,S) = (stack,n,S)
         | dfsRoots((r,_)::roots,stack,n,S) = 
           if A.sub(dfsnum,r) < 0 then
           let val (stack,n,S) = dfs(~1,r,stack,n,S)
           in  dfsRoots(roots,stack,n,S) end
           else dfsRoots(roots,stack,n,S)
       and dfs(p,v,stack,n,S) =
           let val _ = A.update(dfsnum,v,n)
               val _ = A.update(low,v,n)       
               fun min(k) = let val v' = A.sub(low,v)
                            in  if k < v' then A.update(low,v,k) else () end
               fun visit([],stack,n,S) = (stack,n,S)
                 | visit((e as (_,w,_))::es,stack,n,S) = 
                   let val d_w = A.sub(dfsnum,w)
                   in  if A.sub(dfsnum,w) < 0 then
                          let val (stack,n,S) = dfs(v,w,stack,n,S)
                          in  min(A.sub(low,w)); visit(es,stack,n,S) end
                       else (min d_w; visit(es,stack,n,S))
                   end
               fun visit'([],stack,n,S) = (stack,n,S)
                 | visit'((e as (w,_,_))::es,stack,n,S) = 
                   let val d_w = A.sub(dfsnum,w)
                   in  if A.sub(dfsnum,w) < 0 then
                          let val (stack,n,S) = dfs(v,w,stack,n,S)
                          in  min(A.sub(low,w)); visit'(es,stack,n,S) end
                       else (min d_w; visit'(es,stack,n,S))
                   end
               val (stack,n,S) = visit(#out_edges G v,v::stack,n+1,S)
               val (stack,n,S) = visit'(#in_edges G v,stack,n,S)
           in  if p >= 0 andalso A.sub(low,v) = A.sub(dfsnum,p) then
               let fun loop([],C) = ([],C)
                     | loop(w::stack,C) = 
                       let val d_w = A.sub(dfsnum,w)
                           val C   = foldr (fn (e as (_,w',_),C) => 
                               if d_w > A.sub(dfsnum,w') then e::C else C) 
                                     C (#out_edges G w)
                           val C   = foldr (fn (e as (w',_,_),C) => 
                               if d_w > A.sub(dfsnum,w') then e::C else C) 
                                     C (#in_edges G w)
                       in  if w <> v then loop(stack,C) else (stack,C) end
                   val (stack,C) = loop(stack,[])
               in  (stack,n,process(C,S)) end
               else (stack,n,S)
           end

       val (_,_,S) = dfsRoots(#nodes G (),[],0,S)
   in  S
   end

end
