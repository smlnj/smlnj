(*
 * This module implements minimal (undirected) cut.
 * The algorithm is due to Mechtild Stoer and Frank Wagner.
 *
 * -- Allen
 *)

functor MinCut(Num : ABELIAN_GROUP) : MIN_CUT =
struct

   structure Num = Num
   structure G   = Graph
   structure A   = Array
   structure Q   = NodePriorityQueue(A)  
   structure L   = CatnetableList          (* for fast concatenation *)

   fun min_cut {graph=G.GRAPH G, weight} =
   let val N           = #capacity G ()
       val adj         = A.array(N,[])
       val group       = A.array(N,L.empty)
       val onQueue     = A.array(N,~1)
       val adjEdges    = A.array(N,(~1,ref Num.zero))
       val weights     = A.array(N,Num.zero)

       fun new_edge(i,j,w) =
          (A.update(adj,i,(j,w)::A.sub(adj,i));
           A.update(adj,j,(i,w)::A.sub(adj,j)))

       (* Initialize the adjacency and group arrays *)
       fun initialize(nodes) =
       let fun node(i) = A.update(group,i,L.unit i)
           fun edge(e as (i,j,_)) =
              if i <> j then new_edge(i,j,ref(weight e)) else ()
       in  app (fn i => (node i; app edge (#out_edges G i))) nodes
       end

       (* Priority queue ranked by non-decreasing cut weights *)
       val Q = Q.create N (fn (u,v) => Num.<(A.sub(weights,v),A.sub(weights,u)))

       (* Find a better cut (V-{t},{t}) *)
       fun find_cut(phase,a,nodes) =
       let fun mark v     = A.update(onQueue,v,phase)
           fun unmark v   = A.update(onQueue,v,~1)
           fun marked v   = A.sub(onQueue,v) = phase
           fun deleted v  = A.sub(onQueue,v) = ~2
           fun relax(v,w) = (A.update(weights,v,Num.+(A.sub(weights,v),!w)); 
                             Q.decreaseWeight(Q,v))
           fun loop(s,t) =
             if Q.isEmpty Q then (s,t,A.sub(weights,t))
             else let val t' = Q.deleteMin Q
                  in  unmark t';
                      app (fn (v,w) => if marked v then relax(v,w) else ())
                          (A.sub(adj,t'));
                      loop(t,t')
                  end  
        in app (fn u => if deleted u then () else
                         (A.update(weights,u,Num.zero); 
                          mark u; Q.insert(Q,u))) nodes;
           app relax (A.sub(adj,a));
           loop(~1,a)
        end

           (* Coalesce vertices s and t *)
       fun coalesce(s,t) =
          ( (* merge the group of s and t *)
           A.update(group,s,L.append(A.sub(group,s),A.sub(group,t)));
            (* mark neighbors of s *)
           app (fn (u,w) => A.update(adjEdges,u,(s,w))) (A.sub(adj,s));
            (* change t-v(w) and s-v(w') to s-v(w+w') 
             * change t-v(w) to s-v(w) 
             *)
           let fun rmv([],L) = L 
                 | rmv((x as (u,_))::L,L') = rmv(L,if t = u then L' else x::L')
           in app (fn (v,w) => 
                   let val (s',w') = A.sub(adjEdges,v)
                   in  if s = s' then w' := Num.+(!w',!w)
                       else if s <> v then new_edge(s,v,w)
                       else ();
                       A.update(adj,v,rmv(A.sub(adj,v),[]))
                   end) (A.sub(adj,t))
           end;
           A.update(adj,t,[]); 
           A.update(onQueue,t,~2) (* delete node t *)
          )

       fun iterate(n,a,best_group,best_cut,best_weight,nodes) = 
         if n >= 2 then 
           let val (s,t,w) = find_cut(n,a,nodes)
               val (best_group,best_cut,best_weight) = 
                 if best_group < 0 orelse Num.<(w,best_weight) 
                 then (t,A.sub(group,t),w)
                 else (best_group,best_cut,best_weight) 
           in  coalesce(s,t);
               iterate(n-1,a,best_group,best_cut,best_weight,nodes) 
           end
         else (L.toList(best_cut),best_weight)

       val nodes = map #1 (#nodes G ())
       
   in  case nodes of
         [] => ([],Num.zero)
       | [_] => ([],Num.zero)
       | a::L => (initialize(nodes); 
                  iterate(length nodes,a,~1,L.empty,Num.zero,L))
   end

end
