(*
 * This module implements max (s,t) flow.
 *
 * -- Allen
 *)

functor MaxFlow(Num : ABELIAN_GROUP) : MAX_FLOW =
struct
   structure G   = Graph
   structure A   = Array
   structure Num = Num

   (*
    * Use Goldberg's preflow-push approach.
    * This algorithm is presented in the book by Cormen, Leiserson and Rivest.
    *)
   fun max_flow{graph=G.GRAPH G, s, t, capacity, flows} =
   let val _          = if s = t then raise G.Graph "maxflow" else ()
       val N          = #capacity G ()
       val M          = #order G ()
       val neighbors  = A.array(N,[])
       val zero       = Num.zero
       val dist       = A.array(N,0)
       val excess     = A.array(N,zero)
       val current    = A.array(N,[])

       fun min(a,b) = if Num.<(a,b) then a else b
       fun isZero a = Num.==(a,zero)
       val ~        = Num.~

       fun initialize_preflow() =
       let fun add_edge (e as (u,_,_)) = 
               A.update(neighbors,u,e::A.sub(neighbors,u))
       in  #forall_edges G (fn e as (u,v,e') => 
               let val c = capacity e 
               in  if u = s then
                      let val f = ref c and f' = ref(~ c)
                      in  add_edge (u,v,(f,c,f',true,e'));
                          add_edge (v,u,(f',zero,f,false,e'));
                          A.update(excess,v,Num.+(c,A.sub(excess,v)))
                      end
                   else 
                      let val f = ref zero and f' = ref zero
                      in  add_edge (u,v,(f,c,f',true,e'));
                          add_edge (v,u,(f',zero,f,false,e'))
                      end
               end);
           A.update(dist,s,M)
       end

       (* 
        * Push d_f(u,v) = min(e[u],c(u,v)) units of flow from u to v 
        * Returns the new e_u
        *)
       fun push(e_u,(u,v,(flow,cap,flow',x,_))) =
       let val c_f = Num.-(cap,!flow)
           val d_f = min(e_u,c_f) 
           val e_v = A.sub(excess,v)
       in  flow  := Num.+(!flow,d_f);
           flow' := ~(!flow);
           A.update(excess,v,Num.+(e_v,d_f));
           Num.-(e_u,d_f)
       end

       (* Lift a vertex
        * dist[v] := 1 + min{ dist[w] | (v,w) \in E_f } 
        * Returns the new dist[v]
        *)
       fun lift(v) =
       let fun loop([],d_v) = d_v
             | loop((v,w,(f,c,_,_,_))::es,d_v) =
               if Num.<(!f,c) then loop(es,Int.min(A.sub(dist,w),d_v))
               else loop(es,d_v)
           val d_v = loop(A.sub(neighbors,v),1000000000) + 1
       in  A.update(dist,v,d_v); 
           d_v
       end

       (*
        * Push all excess flow thru admissible edges to neighboring vertices 
        * until all excess flow has been discharged.
        *)
       fun discharge(v) =
       let val e_v = A.sub(excess,v)
       in  if isZero e_v then false
           else
           let fun loop(d_v,e_v,(e as (v,w,(f,c,_,_,_)))::es) = 
                   if Num.<(!f,c) andalso d_v = A.sub(dist,w) + 1 then
                      let val e_v = push(e_v,e) 
                      in  if isZero e_v then (d_v,es) 
                          else loop(d_v,e_v,es) 
                      end
                   else loop(d_v,e_v,es)
                | loop(_,e_v,[]) = loop(lift(v),e_v,A.sub(neighbors,v))
               val d_v       = A.sub(dist,v)
               val (d_v',es) = loop(d_v,e_v,A.sub(current,v))
           in  A.update(excess,v,zero);    (* e[v] must be zero *)
               A.update(current,v,es);  
               d_v <> d_v'
           end
       end

       fun lift_to_front() =
          (initialize_preflow();
           iterate([],
              List.foldr(fn ((u,_),L) => 
                  if u = s orelse u = t then L
                  else (A.update(current,u,A.sub(neighbors,u)); u::L)) [] 
                      (#nodes G ()))
          )

       and iterate(_,[]) = ()
         | iterate(F,u::B) = 
            if discharge(u) then iterate([u],rev F@B)
            else iterate(u::F,B)

   in  lift_to_front();
       #forall_nodes G (fn (i,_) => 
           app (fn (i,j,(f,_,_,x,e')) => 
                if x then flows((i,j,e'),!f) else ())
               (A.sub(neighbors,i)));
       List.foldr (fn ((_,_,(f,_,_,_,_)),n) => Num.+(!f,n)) zero
            (A.sub(neighbors,s))
   end

   fun min_cost_max_flow{graph=G.GRAPH G, s, t, capacity, cost, flows} = 
       raise Graph.Unimplemented

end
