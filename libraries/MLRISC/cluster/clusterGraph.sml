(*
 * This gives a cluster a graph view so that all graph based algorithms
 * can be applied on the cluster.  The view is readonly though.
 *
 * -- Allen
 *)
functor ClusterGraph(F : FLOWGRAPH) : CLUSTER_GRAPH =
struct

   structure F = F
   structure I = F.I
   structure W = F.W
   structure G = Graph
   structure A = Array

   datatype info = INFO of F.cluster * F.block A.array

   type block = F.block
   type edge_info = W.freq ref

   type cfg = (block,edge_info,info) Graph.graph

   fun error msg = MLRiscErrorMsg.error("ClusterGraph",msg)

   fun table(G.GRAPH{graph_info=INFO(_,table),...}) = table
   fun cluster(G.GRAPH{graph_info=INFO(cluster,_),...}) = cluster

   (* In a cluster the basic blocks are numbered consecutively.
    *)
   fun isTakenBranch(i,j,_) = i+1 <> j 

   fun annotations(G.GRAPH{graph_info=INFO(F.CLUSTER{annotations=a, ...},_),
                      ...}) = a
   (*
    * Extract the node frequency of a block
    *)
   fun freq(F.BBLOCK{freq,...}) = freq
     | freq(F.ENTRY{freq,...})  = freq
     | freq(F.EXIT{freq,...})   = freq
     | freq _ = error "freq"

   (*
    * Extract the instructions 
    *)
   fun insns(F.BBLOCK{insns, ...}) = insns
     | insns _ = error "insns"

   (*
    * Extract the liveOut set
    *)
   fun liveOut(F.BBLOCK{liveOut, ...}) = !liveOut
     | liveOut _ = I.C.empty

   fun clusterGraph(cluster as F.CLUSTER{blocks,blkCounter,exit,entry,...}) =
   let fun readonly _ = raise G.Readonly
       fun do_nothing _ = ()
       val table = A.array(!blkCounter,F.LABEL(Label.newLabel "dummy"))
       fun number(F.BBLOCK{blknum,...}) = blknum
         | number(F.ENTRY{blknum,...}) = blknum
         | number(F.EXIT{blknum,...}) = blknum
         | number _ = raise G.Graph "clusterGraph"
       fun fill([],size,order,entry,exit) = (size,order,entry,exit)
         | fill((b as F.ENTRY{blknum,succ,...})::rest,size,order,entry,exit) =
             (A.update(table,blknum,b); 
              fill(rest,size+length(!succ),order+1,blknum,exit)
             )
         | fill((b as F.EXIT{blknum,...})::rest,size,order,entry,exit) =
             (A.update(table,blknum,b); 
              fill(rest,size,order+1,entry,blknum)
             )
         | fill((b as F.BBLOCK{blknum,succ,...})::rest,size,order,entry,exit) =
             (A.update(table,blknum,b); 
              fill(rest,size+length(!succ),order+1,entry,exit)
             )
         | fill(_::rest,size,order,entry,exit) =
             fill(rest,size,order,entry,exit)
       val (size,order,entryId,exitId) = fill(entry::exit::blocks,0,0,~1,~1)
       fun nodes() = A.foldri(fn (_,F.LABEL _,rest) => rest
                               | (b,b',rest) => (b,b')::rest) [] (table,0,NONE)
       fun edges() = 
       let fun f(i,succ,es) = 
              foldr (fn ((j,e),es) => (i,number j,e)::es) es (!succ)
       in  A.foldri
           (fn (i,F.BBLOCK{succ,...},es) => f(i,succ,es)
             | (i,F.ENTRY{succ,...},es) => f(i,succ,es)
             | (_,_,es) => es) [] (table,0,NONE)
       end    

       fun out_edges i = 
       let fun f succ = map (fn (j,e) => (i,number j,e)) (!succ)
       in  case A.sub(table,i) of
               F.BBLOCK{succ,...} => f succ
            |  F.ENTRY{succ,...} => f succ
            |  _ => []
       end

       fun in_edges j = 
       let fun f pred = map (fn (i,e) => (number i,j,e)) (!pred)
       in  case A.sub(table,j) of
               F.BBLOCK{pred,...} => f pred
            |  F.EXIT{pred,...} => f pred
            |  _ => []
       end

       fun succ i =
       let fun f succ = map (fn (j,e) => number j) (!succ)
       in  case A.sub(table,i) of
               F.BBLOCK{succ,...} => f succ
            |  F.ENTRY{succ,...} => f succ
            |  _ => []
       end
          
       fun pred j = 
       let fun f pred = map (fn (i,e) => number i) (!pred)
       in  case A.sub(table,j) of
               F.BBLOCK{pred,...} => f pred
            |  F.EXIT{pred,...} => f pred
            |  _ => []
       end

       fun has_edge(i,j) =
       let fun find [] = false
             | find((k,_)::es) = j = number k orelse find es
       in  case A.sub(table,i) of
               F.BBLOCK{succ,...} => find(!succ)
            |  F.ENTRY{succ,...} => find(!succ)
            |  _ => false
       end handle _ => false

       fun has_node i =
           (case A.sub(table,i) of
              F.BBLOCK _ => true
            | F.ENTRY _ => true
            | F.EXIT _ => true
            | _ => false
           ) handle _ => false

       fun node_info i = A.sub(table,i) handle _ => raise G.NotFound

       fun empty _ = []
       fun forall_nodes f =
           A.appi (fn (i,i' as F.LABEL _) => ()
                    | (i,i') => f(i,i')) (table,0,NONE)
       fun forall_edges f =
       let fun g(_,[]) = ()
             | g(i,(j,e)::es) = (f(i,number j,e); g(i,es))
       in  A.appi (fn (i,F.BBLOCK{succ,...}) => g(i,!succ) 
                    | (i,F.ENTRY{succ,...}) => g(i,!succ)
                    | _ => ()) (table,0,NONE)
       end

   in  G.GRAPH
       {  name            = "cluster",
          graph_info      = INFO(cluster,table),
          new_id          = readonly,
          add_node        = readonly,
          add_edge        = readonly,
          remove_node     = readonly,
          set_out_edges   = readonly,
          set_in_edges    = readonly,
          set_entries     = readonly,
          set_exits       = readonly,
          garbage_collect = do_nothing,
          nodes           = nodes,
          edges           = edges,
          order           = fn _ => order,
          size            = fn _ => size,
          capacity        = fn _ => !blkCounter,
          succ            = succ,
          pred            = pred,
          out_edges       = out_edges,
          in_edges        = in_edges,
          has_edge        = has_edge,
          has_node        = has_node,
          node_info       = node_info,
          entries         = fn _ => [entryId],
          exits           = fn _ => [exitId],
          entry_edges     = empty,
          exit_edges      = empty,
          forall_nodes    = forall_nodes,
          forall_edges    = forall_edges
       }
   end

end
