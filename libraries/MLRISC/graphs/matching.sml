(*
 *  This module implenents max cardinality matching.  
 *  Each edge of the matching are folded together with a user supplied
 *  function.
 *
 *  Note: The graph must be a bipartite graph.
 *  Running time is O(|V||E|)
 *  From the book by Aho, Hopcroft, Ullman
 *
 *  -- Allen
 *)

structure BipartiteMatching : BIPARTITE_MATCHING =
struct

   structure G = Graph
   structure A = Array

   fun matching (G.GRAPH G) f x =
   let val N     = #capacity G ()
       val mate  = A.array(N,~1)

       fun married i  = A.sub(mate,i) >= 0
       fun match(i,j) =
           ((* print("match "^Int.toString i^" "^Int.toString j^"\n"); *)
            A.update(mate,i,j); A.update(mate,j,i))
       (* 
        * Simple greedy algorithm to find an initial matching 
        *)
       fun compute_initial_matching() = 
       let fun edges [] = ()
             | edges((i,j,_)::es) = 
               if i = j orelse married j then edges es else match(i,j)
       in  #forall_nodes G (fn (i,_) =>  
              if married i then () else edges(#out_edges G i))
       end

       val visited = A.array(N,~1)  
       val pred    = A.array(N,~1)  (* bfs spanning tree *)

       (*
        * Build an augmenting path graph using bfs
        * Invariants: 
        *  (1) the neighbors of an unmarried vertex must all be married
        *  (2) unmarried vertices on the queue are the roots of BFS 
        * Returns true iff a new augmenting path is found
        *)
       fun build_augmenting_path(phase,unmarried) =
       let (* val _ = print("Phase "^Int.toString phase^"\n") *)
           fun neighbors u = #succ G u @ #pred G u
           fun marked u  = A.sub(visited,u) = phase
           fun mark u    = A.update(visited,u,phase)
           fun edge(u,v) = A.update(pred,v,u)
           fun bfsRoots [] = false
             | bfsRoots(r::roots) = 
               if marked r orelse married r then bfsRoots roots
               else (mark r; bfsEven(r,neighbors r,[],[],roots))

           and bfs([],[],roots)  = bfsRoots roots
             | bfs([],R,roots)   = bfs(rev R,[],roots)
             | bfs(u::L,R,roots) = bfsOdd(u,neighbors u,L,R,roots)

               (* u is married, find an unmatched neighbor v *)
           and bfsOdd(u,[],L,R,roots) = bfs(L,R,roots)
             | bfsOdd(u,v::vs,L,R,roots) = 
               if marked v then bfsOdd(u,vs,L,R,roots) else 
               let val w = A.sub(mate,v)
               in  if u = w then bfsOdd(u,vs,L,R,roots)
                   else if w < 0 then (edge(u,v); path v) (*v is unmarried!*)
                   else (mark v; mark w; edge(u,v); bfsOdd(u,vs,L,w::R,roots))
               end

               (* u is unmarried, all neighbors vs are married *)
           and bfsEven(u,[],L,R,roots) = bfs(L,R,roots)
             | bfsEven(u,v::vs,L,R,roots) = 
               if marked v then bfsEven(u,vs,L,R,roots)
               else let val w = A.sub(mate,v)
                    in  mark v; mark w; edge(u,v); bfsEven(u,vs,L,w::R,roots) 
                    end

               (* found a path, backtrack and update the matching edges *) 
           and path ~1 = true
             | path u  = let val v = A.sub(pred,u)
                             val w = A.sub(mate,v)
                         in  match(u,v); path w end
       in  bfsRoots(unmarried) end

       (*
        * Main loop
        *)
       fun iterate() =
       let val unmarried = List.foldr
             (fn ((u,_),L) => if married u then L else u::L) [] (#nodes G ())
           fun loop(phase) = 
             if build_augmenting_path(phase,unmarried) then 
                loop(phase+1) else ()
       in  loop(0) end

       (* fold result; make sure parallel and opposite edges are handled *)
       fun fold(f,x) =
       let val m = ref x
           val k = ref 0
       in  #forall_edges G (fn e as (i,j,_) =>
             if A.sub(mate,i) = j then 
                (A.update(mate,i,~1); A.update(mate,j,~1); 
                 k := !k + 1; m := f(e,!m)) 
             else ());
           (!m,!k)
       end

   in  compute_initial_matching();
       iterate();
       fold(f,x)
   end

end
