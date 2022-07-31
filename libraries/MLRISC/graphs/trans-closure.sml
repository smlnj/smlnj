(*
 * In place transitive closures.
 *
 * -- Allen
 *)
signature TRANSITIVE_CLOSURE =
sig
   val acyclic_transitive_closure : 
         { + : 'e * 'e -> 'e,
           simple : bool
         } -> ('n,'e,'g) Graph.graph -> unit
   val acyclic_transitive_closure2 : 
         { +   : 'e * 'e -> 'e,
           max : 'e * 'e -> 'e
         } -> ('n,'e,'g) Graph.graph -> unit
   val transitive_closure : 
         ('e * 'e -> 'e) -> ('n,'e,'g) Graph.graph -> unit
end

structure TransitiveClosure : TRANSITIVE_CLOSURE =
struct

   structure G = Graph
   structure A = Array

   (* 
    * Transitive closure for an acyclic graph.
    * Should probably use a better algorithm.
    *)
   fun acyclic_transitive_closure {+,simple} (G' as G.GRAPH G) =
   let val N     = #capacity G ()
       val reach = A.array(N,~1)     (* reach[v] = u iff v -> u *)
       fun visit u =
       let fun visitEdge(v,u,e) =
           let fun trans(w,v,e') =
                   if A.sub(reach,w) = u then ()
                   else (A.update(reach,w,u); #add_edge G (w,u,e + e'))
           in  app trans (#in_edges G v)
           end
           val in_edges = #in_edges G u
       in  if simple then
              app (fn (v,_,_) => A.update(reach,u,v)) in_edges
           else ();
           app visitEdge in_edges
       end
       val list = GraphTopsort.topsort G' (map #1 (#nodes G ()))
   in  app visit list
   end

   fun acyclic_transitive_closure2 {+,max} (G' as G.GRAPH G) =
   let val N      = #capacity G ()
       val reach  = A.array(N,~1)  (* reach[v] = u iff v -> u *)
       val labels = A.array(N,[])  (* l in labels[v] iff v ->l u *)
       fun visit u =
       let fun ins(v,e,nodes) =
               if A.sub(reach,v) = u then 
                  (A.update(labels,v,e::A.sub(labels,v)); nodes)
               else (A.update(reach,v,u); A.update(labels,v,[e]); v::nodes)
           fun init([],nodes) = nodes
             | init((v,u,e)::es,nodes) = init(es,ins(v,e,nodes))
           fun addTrans([],nodes) = nodes
             | addTrans((v,u,e)::es,nodes) = 
               let fun trans([],nodes) = nodes
                     | trans((w,v,e')::es,nodes) = trans(es,ins(w,e + e',nodes))
               in  addTrans(es,trans(#in_edges G v,nodes)) end
           val in_edges = #in_edges G u
           val nodes = init(in_edges,[])        (* insert v -> u *)
           val nodes = addTrans(in_edges,nodes) (* insert w -> u if w -> v *)
           fun foldAll([],es) = es
             | foldAll(v::vs,es) =
                (case A.sub(labels,v) of
                   [] => raise Graph.Graph "acyclic_transitive_closure2"
                 | [e] => foldAll(vs,(v,u,e)::es)
                 | e'::es' => foldAll(vs,(v,u,foldr max e' es')::es)
                )
       in  #set_in_edges G (u,foldAll(nodes,[])) 
       end
       val list = GraphTopsort.topsort G' (map #1 (#nodes G ()))
   in  app visit list
   end

   fun transitive_closure f (G.GRAPH G) = raise Graph.Unimplemented

end

