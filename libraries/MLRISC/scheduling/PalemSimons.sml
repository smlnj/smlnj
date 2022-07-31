(*
 * This is Krishna Palem's and Barbara Simons' algorithm.
 *
 * -- Allen
 *)
structure PalemSimons :> PALEM_SIMONS =
struct
   structure G = Graph
   structure A = Array

   fun rank{dag,l,d,m} =
   let val G.GRAPH G = dag
       val N         = #capacity G ()
       val d'        = A.array(N,0) (* modified deadlines *)
       val order     = A.array(N,0) (* node id -> rank order in swr *)
       val rank      = A.array(N,0) (* rank order -> rank in swr *)
       val tree      = A.array(N,0) (* rank order -> tree *)
       val content   = A.array(N,0) (* rank order -> filled slots *)
       val capacity  = A.array(N,0) (* rank order -> max slots *)


       fun backSchedule i =
       let (* p is the current rank order within succs *)
           fun initTrees([],_,_) = ()
             | initTrees((j,_,d_j)::succs,last_d_j,p) =
               if last_d_j = d_j then
                  (A.update(order,j,p); 
                   initTrees(succs,last_d_j,p))
               else
                  let val p = p+1
                  in  A.update(tree,p,p);  (* new tree *)
                      A.update(rank,p,d_j);
                      A.update(content,p,0);
                      A.update(capacity,p,(d_j - last_d_j)*m);
                      A.update(order,j,p);  
                      initTrees(succs,d_j,p)
                  end

           fun FIND p = 
               let val q = A.sub(tree,p)
               in  if q = p then p else
                   let val r = FIND q
                   in  A.update(tree,p,r); r end
               end

           fun UNION(p,q) = A.update(tree,p,q)
 
           fun insert([],d_i) = d_i
             | insert((j,l_j,d_j)::swr,d_i) = 
               let val ord  = A.sub(order,j)
                   val p    = FIND ord
                   val c    = A.sub(content,p) 
                   val _    = A.update(content,p,c + 1)
                   val D'   = A.sub(rank,p) - c div m
                   val d_i  = Int.min(D' - 1 - l_j,d_i)
               in  if c >= A.sub(capacity,p) then 
                      let val q = FIND(A.sub(order,A.sub(tree,p-1)))
                      in  UNION(p,q) 
                      end
                   else ();
                   insert(swr,d_i)
               end

           val succs = #out_edges G i
           val list = map (fn e as (_,j,_) => (j,l e,A.sub(d',j))) succs
           fun byRank((_,_,d_i),(_,_,d_j)) = d_i > d_j
           val _   = initTrees(ListMergeSort.sort byRank list,~123456789,~1) 
           fun byLatencyAndRank((_,l_i,d_i),(_,l_j,d_j)) =
               l_i < l_j orelse (l_i = l_j andalso d_i < d_j)
           val d_i = insert(ListMergeSort.sort byLatencyAndRank list,
                            d(i,#node_info G i))
       in  A.update(d',i,d_i)
       end

   in  (* backward scheduling in reverse topological order *)
       app backSchedule
          (GraphTopsort.topsort (ReversedGraphView.rev_view dag) 
            (map #1 (#nodes G ())));
       {d'=d'}
   end

end
