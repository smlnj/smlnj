(* 
 * This is my L-compressed DJ-graph data structure for optimal SSA 
 * construction. For the description of this algorithm, see: 
 *   http://www.cs.nyu.edu/leunga/my-papers/linear-ssa.ps
 * --Allen
 *)

functor L_DJGraph (Dom : DOMINATOR_TREE) : DJ_GRAPH =
struct

   structure G       = Graph
   structure Dom     = Dom
   structure A       = Array

   datatype exit_tree = 
      NODE of { name            : int,
                jedges          : int list, 
                eedges          : exit_tree list,
                minVisitedLevel : int ref,
                visited         : int ref
              }

   datatype ('n,'e,'g) dj_graph = 
      DJGRAPH of
        { dom      : ('n,'e,'g) Dom.dominator_tree,
          trees    : exit_tree list A.array,
          stamp    : int ref
        }

   fun error msg = MLRiscErrorMsg.error("L-DJGraph",msg)

   val stats          = false (* collect statistics? *)
   val visitCount     = MLRiscControl.getCounter "dj-visit-count"
   val idfCount       = MLRiscControl.getCounter "dj-IDF-count"
   val idfSize        = MLRiscControl.getCounter "dj-IDF-size"
   val liveVisitCount = MLRiscControl.getCounter "dj-live-visit-count"
   val debug          = true

   fun DJ(Dom as G.GRAPH dom) = 
   let val G.GRAPH cfg = Dom.cfg Dom 
       val N           = #capacity dom ()
       val [ENTRY]     = #entries dom ()
       val levelsMap   = Dom.levelsMap Dom
       val L           = Dom.max_levels Dom 
       val trees       = A.array(N, [])
       val levels'     = A.array(L, ~1) 
       val exitLevels' = A.array(L, ~1) 
       val T           = A.array(L, [])
       val J           = A.array(L, [])

       fun LTrees a = 
       let (* recurse *)
           fun foreachDedge([]) = () 
             | foreachDedge((_,b,_)::es) = (LTrees b; foreachDedge es)

           val _ = foreachDedge(#out_edges dom a)

           val lvl_a = A.sub(levelsMap, a)

           (* partition J-edges *)
           fun foreachJedge([], levels) = levels
             | foreachJedge((_,b,_)::es, levels) = 
               let val lvl_b = A.sub(levelsMap, b) 
               in  if lvl_b > lvl_a then (* non-J-edge *)
                     foreachJedge(es, levels)
                   else
                     let val _ = 
                             if A.sub(exitLevels',lvl_b) = a then 
                                A.update(J, lvl_b, b::A.sub(J, lvl_b))
                             else
                                (A.update(exitLevels',lvl_b,a);
                                 A.update(J, lvl_b, [b])
                                )
                         val levels =
                             if A.sub(levels',lvl_b) = a then 
                                levels
                             else
                                (A.update(T, lvl_b, []);
                                 A.update(levels',lvl_b,a); 
                                 lvl_b::levels
                                )
                     in  foreachJedge(es, levels)
                     end
               end

           val levels = foreachJedge(#out_edges cfg a, [])

           (* partition subtrees *)
           fun foreachDedge([], levels) = levels
             | foreachDedge((_,b,_)::es, levels) =
               let fun foreachTree([], levels) = levels
                     | foreachTree((l,t)::ts, levels) =
                       let val levels =
                           if l <= lvl_a then
                              if A.sub(levels',l) = a then 
                                 (A.update(T, l, t::A.sub(T, l)); levels)
                              else (A.update(levels',l,a); 
                                    A.update(T, l, [t]); 
                                    l::levels)
                           else levels
                       in  foreachTree(ts, levels) end
                   val levels = foreachTree(A.sub(trees, b), levels)
               in  foreachDedge(es, levels)
               end
           val levels = foreachDedge(#out_edges dom a, levels)

           (* Build Trees(a) *)
           fun buildTrees([], trees_a) = trees_a
             | buildTrees(l::levels, trees_a) = 
               let fun makeNode(succs) =
                   let val jedges = if A.sub(exitLevels',l) = a then
                                       A.sub(J,l) else []
                   in  (l,NODE{name=a, jedges=jedges, eedges=succs,
                               minVisitedLevel=ref 0, visited=ref 0})
                   end
                   val T_l = 
                      case A.sub(T,l) of
                        []        => makeNode []  
                      | ts as [t] => if A.sub(exitLevels',l) <> a then (l,t)
                                     else makeNode ts 
                      | ts        => makeNode ts
               in  buildTrees(levels, T_l::trees_a) 
               end
           val trees_a = buildTrees(levels, [])
       in  A.update(trees, a, trees_a)
       end

       fun bucketSort(trees) =
       let val buckets = A.array(L, [])
           val _ = 
              #forall_nodes dom 
              (fn (a,_) =>
               let fun insert [] = ()
                     | insert((l,t)::ts) = 
                       (A.update(buckets,l,(a,t)::A.sub(buckets,l)); insert ts)
               in  insert(A.sub(trees,a)) 
               end)
           val trees = A.array(N, [])
           fun collect(l) =  
               if l >= L then ()
               else let fun dist [] = ()
                          | dist((a,t)::ts) =
                            (A.update(trees,a,t::A.sub(trees,a)); dist ts)
                    in  dist(A.sub(buckets, l));
                        collect(l+1)
                    end
       in  collect 0;
           trees
       end

       val _     = LTrees ENTRY (* build Trees *)
       val trees = bucketSort(trees) (* sort trees *)
   in  DJGRAPH{dom=Dom, trees=trees, stamp=ref 0}
   end

   (* Compute dominance frontier *)
   fun DF _ = error "DF"
   fun IDFs _ = error "IDFs"

   fun LiveIDFs (DJGRAPH{trees, dom, stamp, ...}) =  
   let val G.GRAPH cfg = Dom.cfg dom
       val levelsMap   = Dom.levelsMap dom
       val N           = #capacity cfg ()
       val inphi       = A.array(N, 0)
       val inalpha     = A.array(N, 0)
       val live        = A.array(N, 0)
       fun newStamp() = 
           let val s = Word.toIntX(Word.fromInt(!stamp) + 0w1) 
           in stamp := s; s 
           end
       fun LiveIDFs{defs, localLiveIn=[]} = []
         | LiveIDFs{defs, localLiveIn} =
       let val stamp = newStamp()

           fun initDefs([]) = ()
             | initDefs(x::xs) = (A.update(inalpha, x, stamp); initDefs xs)

           fun markLiveIn(b) =
           let fun markPred [] = ()
                 | markPred((j,_,_)::es) =
                    (if A.sub(live,j) <> stamp andalso
                        A.sub(inalpha,j) <> stamp then
                       markLiveIn j
                     else ();
                     markPred es
                    )
           in  (* m := !m + 1; *)
               A.update(live,b,stamp);
               if stats then liveVisitCount := !liveVisitCount + 1 else ();
               markPred(#in_edges cfg b)
           end

           fun initLiveIn [] = ()
             | initLiveIn(x::xs) = (markLiveIn x; initLiveIn xs)

           fun isLive b = A.sub(live, b) = stamp 

           fun unmarked(X,b) = A.sub(X,b) <> stamp
           fun mark(X,b) = A.update(X,b,stamp)

           fun visit(x, queue, IDF) =
           let val level_x = A.sub(levelsMap, x)

               fun walk(NODE{name=y, jedges, eedges, visited, minVisitedLevel},
                        queue, IDF) = 
               let fun foreachJedge([], queue, IDF) = (queue, IDF)
                     | foreachJedge(z::es, queue, IDF) = 
                       if isLive z andalso unmarked(inphi,z)
                       then (mark(inphi, z);
                             foreachJedge
                               (es, 
                                if unmarked(inalpha,z) then z::queue else queue,
                                z::IDF)
                            )
                       else foreachJedge(es, queue, IDF)
                   fun foreachEEdge([], queue, IDF) = (queue, IDF)
                     | foreachEEdge((z as NODE{name=z',...})::es, queue, IDF) =
                       if isLive z' then
                          let val (queue, IDF) = walk(z, queue, IDF)
                          in  foreachEEdge(es, queue, IDF) end
                       else 
                          foreachEEdge(es, queue, IDF)
               in  if !visited = stamp then (* visited before *)
                      (minVisitedLevel := Int.min(!minVisitedLevel, level_x);
                       (queue, IDF)
                      )
                   else 
                     (minVisitedLevel := level_x; (* non-visited *)
                      visited := stamp;
                      let val (queue, IDF) = foreachJedge(jedges, queue, IDF)
                      in  foreachEEdge(eedges, queue, IDF) end 
                     )
               end

               fun ancestorHasBeenProcessed
                     (NODE{visited, minVisitedLevel, ...}) = 
                   !visited = stamp andalso !minVisitedLevel < level_x

               fun foreachTree([], queue, IDF) = (queue, IDF)
                 | foreachTree(t::ts, queue, IDF) =
                   if ancestorHasBeenProcessed t then (queue, IDF)
                   else let val (queue, IDF) = walk(t, queue, IDF)
                        in  foreachTree(ts, queue, IDF) end
           in  foreachTree(A.sub(trees, x), queue, IDF)
           end

           fun visitAll([], IDF) = IDF
             | visitAll(x::queue, IDF) = 
               let val (queue, IDF) = visit(x, queue, IDF)
               in  visitAll(queue, IDF) end

       in  initDefs defs;
           initLiveIn localLiveIn;
           visitAll(defs, [])
       end
   in  LiveIDFs
   end

end
