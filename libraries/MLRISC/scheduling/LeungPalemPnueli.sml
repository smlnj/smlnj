(*
 * This is my algorithm from PACT '98.
 *
 * -- Allen
 *)
structure LeungPalemPnueli :> LEUNG_PALEM_PNUELI =
struct

   structure G = Graph
   structure A = Array
   structure PQ = PriorityQueue

   exception Infeasible

   fun rank{dag,l,r,d,m} =
   let val G.GRAPH G = dag 
       val N         = #capacity G ()
       val r'        = A.array(N,0) (* modified release times *)
       val d'        = A.array(N,0) (* modified deadlines *)
       val r_hat     = A.array(N,0) (* backschedule modified release times *)
       val d_hat     = A.array(N,0) (* backschedule modified deadlines *)

       val node_ids  = map #1 (#nodes G ())

       fun initReleaseTimes() = 
       let fun update i =
               A.update(r',i,
                  foldr (fn (e as (j,_,_),r_i) => 
                           Int.max(A.sub(r',j) + l e + 1,r_i)) 
                        (r(i,#node_info G i)) (#in_edges G i))
       in  app update (GraphTopsort.topsort dag node_ids) end

       fun initDeadlines() = 
       let fun update i =
               A.update(d',i,
                  foldr (fn (e as (_,j,_),d_i) => 
                           Int.min(A.sub(d',j) - l e - 1,d_i)) 
                        (d (i,#node_info G i)) (#out_edges G i))
       in  app update (GraphTopsort.topsort (ReversedGraphView.rev_view dag) 
                       node_ids) 
       end


          (* unit time tasks, no-precedence constraints with
           * deadlines d_hat and release times r_hat.
           * I'm using an asymtotically slower (n log n) 
           * algorithm than the one described in the paper. 
           *)
       fun UET(S) =
       let fun byReleaseTimes(i,j) = A.sub(r_hat,i) > A.sub(r_hat,j)
           fun byDeadlines(i,j) = A.sub(d_hat,i) < A.sub(d_hat,j)
           val ready = PQ.create byDeadlines 
           val ins   = PQ.insert ready
           fun listSchedule(waiting,t,0) = listSchedule(waiting,t+1,m)
             | listSchedule(waiting,t,m) = 
               let val j = PQ.deleteMin ready
               in  t < A.sub(d_hat,j) andalso (* check for infeasbility! *)
                   listSchedule(waiting,t,m-1)
               end handle PQ.EmptyPriorityQueue =>
                   (* no more ready nodes *)
               let fun release(t,[]) = (t,[])
                     | release(t,l as j::waiting) = 
                        if A.sub(r_hat,j) > t then (t,l)
                        else (ins j; release(t,waiting))
               in  case waiting of
                     [] => true (* feasible *)
                   | waiting as j::_ => 
                     let val (t,waiting) = release(A.sub(r_hat,j),waiting)
                     in  listSchedule(waiting,t,m) end
               end
       in  listSchedule(ListMergeSort.sort byReleaseTimes S,0,m) end

       fun backSchedule(i,r'_i,S) = 
       let fun loop d'_i = 
           if r'_i >= d'_i then raise Infeasible
           else
           let val _ = A.update(d_hat,i,d'_i)
               val _ = A.update(r_hat,i,d'_i-1)
               val _ = app (fn e as (_,j,_) => 
                          A.update(r_hat,j,Int.max(d'_i + l e,A.sub(r',j))))
                            (#out_edges G i)
           in  if UET S then d'_i 
               else loop(d'_i-1)
           end
  
       in  app (fn j => (A.update(d_hat,j,A.sub(d',j));
                         A.update(r_hat,j,A.sub(r',j)))) S;
           loop(A.sub(d',i)) 
       end

       fun mainLoop([],_) = ()
         | mainLoop(i::U,S) = 
           let val r'_i = A.sub(r',i)
               val S = i::S
               val d'_i = backSchedule(i,r'_i,S)
           in  A.update(d',i,d'_i); 
               if d'_i <= r'_i then raise Infeasible 
               else mainLoop(U,S)
           end
       fun byNonIncreasingReleaseTimes(i,j) = A.sub(r',i) < A.sub(r',j)

   in  (* initialize the modified deadlines/release times *)
       initReleaseTimes();
       initDeadlines();
       mainLoop(ListMergeSort.sort byNonIncreasingReleaseTimes node_ids,[]);
       {r'=r',d'=d'}
   end

end
