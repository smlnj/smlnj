(*
 * Some simple functions for performing depth first search
 *
 * -- Allen
 *)
structure GraphDFS : GRAPH_DEPTH_FIRST_SEARCH = 
struct

   structure G = Graph
   structure A = Array
   structure S = BitSet

   (*
    * Depth first search
    *)
   fun dfs (G.GRAPH G) f g roots =
   let val visited = S.create(#capacity G ())
       fun traverse n =
           if S.markAndTest(visited,n) then ()
           else (f n; app traverse_edge (#out_edges G n))
       and traverse_edge (e as (_,n,_)) =
           if S.markAndTest(visited,n) then ()
           else (g e; f n; app traverse_edge (#out_edges G n))
   in  app traverse roots end

   (*
    * Depth first search fold
    *)
   fun dfsfold (G.GRAPH G) f g roots (x,y) =
   let val visited = S.create(#capacity G ())
       fun traverse(n,x,y) =
           if S.markAndTest(visited,n) then (x,y)
           else traverse_edges(#out_edges G n,f(n,x),y)
       and traverse_edges ([],x,y) = (x,y)
         | traverse_edges ((e as (_,n,_))::es,x,y) =
           if S.markAndTest(visited,n) then traverse_edges(es,x,y)
           else let val y = g(e,y)
                    val x = f(n,x)
                    val (x,y) = traverse_edges(#out_edges G n,x,y)
                in  traverse_edges(es,x,y) end
       and traverseAll([],x,y) = (x,y)
         | traverseAll(n::ns,x,y) = 
            let val (x,y) = traverse(n,x,y)
            in  traverseAll(ns,x,y) end
   in  traverseAll(roots,x,y) end


   fun dfsnum (G.GRAPH G) roots =
   let val N       = #capacity G ()
       val dfsnum  = A.array(N,~1)
       val compnum = A.array(N,~1)
       fun traverse([],d,c) = c
         | traverse(n::ns,d,c) =
           if A.sub(dfsnum,n) >= 0 then traverse(ns,d,c)
           else  let val _ = A.update(dfsnum,n,d); 
                     val c = traverse(#succ G n,d+1,c)
                 in  A.update(compnum,n,c);  
                     traverse(ns,d,c+1)
                 end
   in  traverse(roots,0,0); {dfsnum=dfsnum,compnum=compnum} end

   fun preorder_numbering (G.GRAPH G) root =
   let val N = #capacity G ()
       val P = A.array(N,~1)
       fun f(i,n) = 
           if A.sub(P,i) = ~1 then
              let fun g([],n) = n 
                    | g((_,j,_)::es,n) = g(es,f(j,n))
              in  A.update(P,i,n); g(#out_edges G i,n+1) end
           else n
   in  f(root,0); P end

   fun postorder_numbering (G.GRAPH G) root =
   let val N = #capacity G ()
       val P = A.array(N,~2)
       fun f (i,n) = 
           if A.sub(P,i) = ~2 then
              let fun g([],n) = n
                    | g((_,j,_)::es,n) = g(es,f(j,n))
                  val _ = A.update(P,i,~1)
                  val n =  g(#out_edges G i,n) 
              in  A.update(P,i,n); n+1
              end
           else n
   in  f(root,0); P end
end

