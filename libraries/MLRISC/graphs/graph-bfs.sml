(*
 * Breath first search.
 *
 * -- Allen
 *)

structure GraphBFS : GRAPH_BREATH_FIRST_SEARCH = 
struct

   structure G = Graph
   structure S = BitSet
   structure A = Array

   (*
    * Breath first search
    *)
   fun bfs (G.GRAPH G) f g roots =
   let val visited = S.create(#capacity G ())
       fun visit([],[])  = ()
         | visit([],R)   = visit(rev R,[])
         | visit(n::L,R) = (f n; visitSucc(#out_edges G n,L,R))
       and visitSucc([],L,R) = visit(L,R)
         | visitSucc((e as (i,j,_))::es,L,R) =
            if S.markAndTest(visited,j) then visitSucc(es,L,R)
            else (g e; visitSucc(es,L,j::R))
       and visitRoots([],L,R) = visit(L,R)
         | visitRoots(n::ns,L,R) = 
            if S.markAndTest(visited,n) then visitRoots(ns,L,R)
            else (f n; visitRoots(ns,L,n::R))
   in  visitRoots(roots,[],[])
   end

   fun bfsdist (G.GRAPH G) roots =
   let val N = #capacity G ()
       val dist = A.array(N,~1)
       fun visit([],[])  = ()
         | visit([],R)   = visit(rev R,[])
         | visit(n::L,R) = visitSucc(#out_edges G n,L,R)
       and visitSucc([],L,R) = visit(L,R)
         | visitSucc((e as (i,j,_))::es,L,R) =
            if A.sub(dist,j) >= 0 then visitSucc(es,L,R)
            else (A.update(dist,j,A.sub(dist,i)+1); visitSucc(es,L,j::R))
       and visitRoots([],L,R) = visit(L,R)
         | visitRoots(n::ns,L,R) = 
            if A.sub(dist,n) >= 0 then visitRoots(ns,L,R)
            else (A.update(dist,n,0); visitRoots(ns,L,n::R))
   in  visitRoots(roots,[],[]); dist end

end
