(*
 * This module is responsible for locating loop structures (intervals).
 * All loops have only one single entry (via the header) but
 * potentially multiple exits, i.e. the header dominates all nodes.
 * Basically this is Tarjan's algorithm.  
 *
 * The old version is broken as reported by William Chen.
 * This is a rewrite.
 *)

functor LoopStructure (structure GraphImpl : GRAPH_IMPLEMENTATION
                       structure Dom       : DOMINATOR_TREE)
    : LOOP_STRUCTURE =
struct
 
   structure G   = Graph
   structure GI  = GraphImpl
   structure Dom = Dom
   structure A   = Array
   structure U   = URef

   datatype ('n,'e,'g) loop = 
      LOOP of { nesting    : int,
                header     : G.node_id,
                loop_nodes : G.node_id list,
                backedges  : 'e G.edge list,
                exits      : 'e G.edge list
              }

   datatype ('n,'e,'g) loop_info = 
       INFO of { dom : ('n,'e,'g) Dom.dominator_tree }

   type ('n,'e,'g) loop_structure = 
         (('n,'e,'g) loop, unit, ('n,'e,'g) loop_info) Graph.graph 

   fun dom(G.GRAPH{graph_info=INFO{dom,...},...}) = dom

   fun loop_structure DOM = 
   let
       val info               = INFO{ dom = DOM }
       val G.GRAPH cfg        = Dom.cfg DOM
       val G.GRAPH dom        = DOM
       val N                  = #capacity dom ()
       val dominates          = Dom.dominates DOM
       val LS as G.GRAPH ls   = GI.graph ("Loop structure",info,N) 
       val ENTRY              = case #entries cfg () of
   				   [ENTRY] => ENTRY
			        | _ => raise Graph.NotSingleEntry

       (* mapping from node id -> header *)
       val headers = A.array(N, ~1)

       (* mapping from header -> previous header in the loop *)
       val lastHeaders = A.array(N, ~1)

       (* mark all visited nodes during construction *)
       val visited = A.array(N, ~1)

       (* mapping from nodes id -> collapsed header during construction *)
       val P       = A.tabulate(N, U.uRef)

       (* walk the dominator tree and return a list of loops *)
       fun walk (X, loops) =
       let
           (* Look for backedges *)
           val backedges = List.filter 
               (fn (Y, X, _) => dominates(X, Y)) (#in_edges cfg X)
           (* X is a header iff it has backedges or X is the ENTRY *)
           val is_header = case backedges of [] => X = ENTRY | _ => true

           (* Walk the dominator tree first *)
           val loops = List.foldr walk loops (#succ dom X)
       in 
           (* If X is a header node then collaspe all the nodes within
            * the loop into the header.  The entry node has to be
            * treated specially, unfortunately.
            *)
           if is_header then
              let val L = mark(X, X, [])
                  val L = if X = ENTRY then find_entry_loop_nodes [] else L
                  val () = collapse(X, L)
                  val exits = find_exits(L, [])
              in  (* Create a new loop node *)
                  (X, backedges, L, exits)::loops
              end
           else
              loops
       end


          (* mark all the nodes that are within the loop identified
           * by the header.  Return a list of loop nodes.
           *)
       and mark(X, header, L) =
          if A.sub(visited, X) <> header then
          let
              (* mark X as visited *)
              val _ = A.update(visited, X, header) 

              (* header of X *)
              val H_X = A.sub(headers, X)

              val L = if H_X = ~1 then (* X has no header yet *)
                          X::L
                      else if H_X = X andalso A.sub(lastHeaders, X) = ~1 then
                          (* Add loop edge *)
                          (A.update(lastHeaders, X, header);
                           #add_edge ls (header, X, ());
                           L
                          )
                      else L
          in  List.foldr (fn ((Y, _, _), L) => 
                let val Y = U.!! (A.sub(P, Y))
                in  if dominates(header, Y) then mark(Y, header, L) else L
                end) L (#in_edges cfg X)
          end
          else L

          (* collapse all nodes in L to the header H *)
       and collapse(H, L) = 
           let val h = A.sub(P, H)
           in  List.app (fn X => 
                  (U.link (A.sub(P, X), h);
                   if A.sub(headers, X) = ~1 then
                      A.update(headers, X, H)
                   else ())) L
           end

          (* find all nodes that are not part of any loops *)
       and find_entry_loop_nodes L =
           List.foldr (fn ((X, _), L) => 
                 if A.sub(headers, X) = ~1 then
                     X::L
                 else if X <> ENTRY andalso 
                      A.sub(headers, X) = X andalso
                      A.sub(lastHeaders, X) = ~1 then
                      (#add_edge ls (ENTRY, X, ());
                       A.update(lastHeaders, X, ENTRY);
                       L
                      )
                 else 
                     L
                 ) L (#nodes cfg ())


           (* find all edges that can exit from the loop H *)
       and find_exits([],exits) = exits
         | find_exits(X::Xs,exits) =
           let fun f((e as (X,Y,_))::es,exits) =
                   if A.sub(headers,Y) = ~1 
                   then f(es,e::exits) 
                   else f(es,exits)
                 | f([], exits) = exits
           in  find_exits(Xs, f(#out_edges cfg X, exits))
           end

       (* walk tree and create edges *)
       val loops = walk (ENTRY, [])

       (* create nodes *)
       val () = List.app (fn (H, backedges, loop_nodes, exits) =>
             let val last = A.sub(lastHeaders, H)
                 val nesting = if last = ~1 then 0 
                               else 
                                  let val LOOP{nesting, ...} = 
                                          #node_info ls last
                                  in  nesting+1 end
             in  #add_node ls (H, LOOP{nesting    = nesting,
                                       header     = H,
                                       backedges  = backedges,
                                       loop_nodes = loop_nodes,
                                       exits      = exits})
             end) loops
   in
       LS
   end

   fun nesting_level(G.GRAPH L) = let
     val INFO{dom=G.GRAPH dom,...} = #graph_info L
     val N = #capacity dom ()
     val levels  = A.array(N,0)
     fun tabulate(_,LOOP{nesting,header,loop_nodes,...}) =
       (A.update(levels,header,nesting);
	app (fn i => A.update(levels,i,nesting)) loop_nodes)
   in
     #forall_nodes L tabulate;    levels
   end

   fun header(G.GRAPH L) = let
     val INFO{dom=G.GRAPH dom,...} = #graph_info L
     val N = #capacity dom ()
     val headers = A.array(N,0)
     fun tabulate(_,LOOP{header,loop_nodes,...}) =
       (A.update(headers,header,header);
	app (fn i => A.update(headers,i,header)) loop_nodes)
   in  
     #forall_nodes L tabulate;   headers
   end

   fun entryEdges(Loop as G.GRAPH L) = let
     val dom = dom Loop
     val G.GRAPH cfg = Dom.cfg dom
     val dominates = Dom.dominates dom
     fun entryEdges(header) = 
       if #has_node L header then 
	 List.filter (fn (i,j,_) => not(dominates(j,i)))
		     (#in_edges cfg header)
       else []
   in  entryEdges
   end

   fun isBackEdge(Loop as G.GRAPH L) = 
   let val dom = Dom.dominates(dom Loop)
   in  fn (v,w) => #has_node L w andalso dom(w,v)
   end
end    

