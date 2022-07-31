(* 
 * This is my E-compressed DJ-graph data structure
 * --Allen
 *)

functor E_DJGraph (Dom : DOMINATOR_TREE) : DJ_GRAPH =
struct

   structure G       = Graph
   structure Dom     = Dom
   structure A       = Array

   fun error msg = MLRiscErrorMsg.error("E-DJGraph",msg)

   val stats          = false (* collect statistics? *)
   val visitCount     = MLRiscControl.getCounter "dj-visit-count"
   val idfCount       = MLRiscControl.getCounter "dj-IDF-count"
   val idfSize        = MLRiscControl.getCounter "dj-IDF-size"
   val liveVisitCount = MLRiscControl.getCounter "dj-live-visit-count"

   datatype tree = NODE of int * tree list

   datatype ('n,'e,'g) dj_graph = 
      DJGRAPH of
        { dom    : ('n,'e,'g) Dom.dominator_tree,
          trees  : tree option A.array,
          jedges : int list A.array
        }

   fun DJ(Dom as G.GRAPH dom) = 
   let val G.GRAPH cfg = Dom.cfg Dom
       val L           = Dom.max_levels Dom
       val N           = #capacity dom ()
       val levelsMap   = Dom.levelsMap Dom
       val rank_J      = A.array(N, 0)
       val trees       = A.array(N, NONE)
       val jedges      = A.array(N, [])
       val buckets     = A.array(L, [])

       fun ExitTrees a =   
       let fun foreachDedge [] = ()
             | foreachDedge((_,b,_)::es) = (ExitTrees b; foreachDedge es)
           val _     = foreachDedge (#out_edges dom a)
           val lvl_a = A.sub(levelsMap, a)
           fun foreachJedge([], rank) = A.update(rank_J,a,rank)
             | foreachJedge((a,b,_)::es, rank) = 
               let val lvl_b = A.sub(levelsMap, b)  
               in  if lvl_b <= lvl_a then 
                      foreachJedge(es, if lvl_b < rank then lvl_b else rank)
                   else 
                      foreachJedge(es, rank)
               end
           val _ = foreachJedge (#out_edges cfg a, L+1)
           fun buildTree([], succ) = NODE(a,succ)
             | buildTree((_,b,_)::es, succ) = 
               (case A.sub(trees, b) of
                  NONE   => buildTree(es, succ)
                | SOME t => buildTree(es, t::succ)
               )
           val t_a = buildTree(#out_edges dom a, []) 
       in  A.update(trees, a, pruneTree(A.sub(levelsMap, a), t_a))
       end

       and pruneTree(lvl_a, NODE(x,succ)) =
           let fun foreachSucc([], subtrees) = subtrees
                 | foreachSucc(t::ts, subtrees) = 
                     foreachSucc(ts, 
                        case pruneTree(lvl_a, t) of 
                           NONE => subtrees 
                         | SOME t => t::subtrees
                     )
               val subtrees = foreachSucc(succ, []) 
           in  case (A.sub(rank_J,x) <= lvl_a, subtrees) of
                 (false,[])  => NONE
               | (false,[t]) => SOME t
               | (_,ts)      => SOME(NODE(x,ts))
           end

       fun fillJedges l =
           if l < 0 then () else
           let fun fill [] = ()
                 | fill ((a,b)::es) = 
                    (A.update(jedges, a, b::A.sub(jedges, a)); fill es)   
           in  fill(A.sub(buckets, l));
               fillJedges(l-1)
           end

       val [ENTRY] = #entries dom ()
   in  ExitTrees ENTRY;
       fillJedges(L-1); 
       DJGRAPH{dom=Dom, trees=trees, jedges=jedges}
   end

   (* Compute dominance frontier *)
   fun DF _ = error "DF"

   (* Compute iterated dominance frontier *)
   fun IDFs _ = error "IDFs"

   (* Compute iterated dominance frontier with liveness *)
   fun LiveIDFs(DJGRAPH{dom=Dom as G.GRAPH dom, jedges, trees}) = 
   let val G.GRAPH cfg = Dom.cfg Dom
       val L           = Dom.max_levels Dom
       val N           = #capacity dom ()
       val levels      = Dom.levelsMap Dom
       val in_phi      = A.array(N,0)  (* has appeared in the DF set? *)
       val liveIn      = A.array(N,0)
       val stamp       = ref 0
       fun new_stamp() = let val s = !stamp + 1 in stamp := s; s end

       val in_alpha  = A.array(N,0)  (* has appeared in N_alpha? *)
       val visited   = A.array(N,0)  (* has it been visited *)
       val piggybank = A.array(L,[]) (* nodes in the piggy bank *)

       fun LiveIDFs{defs=xs, localLiveIn=[]} = []
         | LiveIDFs{defs=xs, localLiveIn} =
       let val stamp = new_stamp()
           val _ = if stats then idfCount := !idfCount + 1 else ()
           fun init([],l) = l
             | init(x::xs,l) = 
               let val l_x = A.sub(levels,x)
               in  A.update(in_alpha,x,stamp);
                   A.update(piggybank,l_x,x::A.sub(piggybank,l_x));
                   init(xs,if l < l_x then l_x else l)
               end 

           fun markLiveIn(b) =
           let fun markPred [] = ()
                 | markPred((j,_,_)::es) =
                    (if A.sub(liveIn,j) <> stamp andalso
                        A.sub(in_alpha,j) <> stamp then
                       markLiveIn j
                     else ();
                     markPred es
                    )
           in  A.update(liveIn,b,stamp);
               if stats then liveVisitCount := !liveVisitCount + 1 else ();
               markPred(#in_edges cfg b)
           end

           fun initLiveIn [] = ()
             | initLiveIn(x::xs) = (markLiveIn x; initLiveIn xs)

           fun isLive b = A.sub(liveIn,b) = stamp 

           fun visit(x,S) = 
               case A.sub(trees,x) of
                 NONE => S
               | SOME t => walk(t,A.sub(levels,x),S)

           and walk(NODE(y,succ_y),level_x,S) = 
           if A.sub(visited,y) <> stamp then
           let val _ = A.update(visited,y,stamp)
               fun foreachJedge([],S) = S
                 | foreachJedge(z::zs,S) =
                   let val level_z = A.sub(levels,z)
                   in  if level_z <= level_x then 
                          if isLive z andalso A.sub(in_phi,z) <> stamp 
                           (* z is a new IDF^+ candidate; 
                            * make sure it is live.
                            *)
                          then (A.update(in_phi,z,stamp);
                                if A.sub(in_alpha,z) <> stamp
                                then A.update(piggybank,level_z,
                                              z::A.sub(piggybank,level_z))
                                else ();
                                foreachJedge(zs,z::S)
                               )
                          else foreachJedge(zs,S)
                       else S
                   end
               fun foreachEedge([], S) = S
                 | foreachEedge((t as NODE(z,_))::ts,S) = 
                     foreachEedge(ts,if isLive z then walk(t,level_x,S) else S)

               val _ = if stats then visitCount := !visitCount + 1 else ();
           in  foreachEedge(succ_y, foreachJedge(A.sub(jedges, y),S))
           end
           else S

           fun visitAll(~1,S) = S
             | visitAll(l,S) =
               case A.sub(piggybank,l) of
                 [] => visitAll(l-1,S)
               | x::xs => (A.update(piggybank,l,xs);
                           visitAll(l,visit(x,S)))

           val L   = init(xs,~1) 
           val _   = initLiveIn localLiveIn
           val IDF = visitAll(L,[])
       in  if stats then idfSize := !idfSize + length IDF else ();
           IDF
       end

   in  LiveIDFs
   end

end
