(* 
 * The algorithm for computing iterated dominance
 * frontier is my own algorithm which uses the $k$-compressed DJ-graph,
 * which is a variant of DJ-graph due to Sreedhar, Gao and Lee.   Here, 
 * I've set k=2.  The algorithm using $k$-compressed DJ-graph is significantly
 * faster than the DJ-graph version when |DF(x)| <= k.
 *
 * The write up will be in my thesis.
 * 
 * --Allen
 *)

functor K_DJGraph (Dom : DOMINATOR_TREE) : DJ_GRAPH =
struct

   structure G       = Graph
   structure Dom     = Dom
   structure A       = Array

   type ('n,'e,'g) dj_graph = ('n,'e,'g) Dom.dominator_tree

   fun error msg = MLRiscErrorMsg.error("K_DJGraph",msg)

   val stats          = true (* collect statistics? *)
   val levelPrune     = true
   val domPrune       = true
   val pathPrune      = true
   val visitCount     = MLRiscControl.getCounter "dj-visit-count"
   val liveVisitCount = MLRiscControl.getCounter "dj-live-visit-count"
   val debug          = true
   val K_max = 2

   fun DJ x = x

   (* Compute dominance frontier *)
   fun DF (D as G.GRAPH dom) =
   let val G.GRAPH cfg = Dom.cfg D
       val L           = Dom.max_levels D
       val N           = #capacity dom ()
       val levels      = Dom.levelsMap D
       val in_DF       = A.array(N,0)  (* has appeared in the DF set? *)
       val stamp       = ref 0
       fun new_stamp() = let val s = !stamp + 1 in stamp := s; s end

       fun unmarked(marked,i,stamp : int) =
           let val s = A.sub(marked,i)
           in  if s = stamp then false else (A.update(marked,i,stamp); true)
           end

       (* 
        * Compute the dominance frontiers of a node
        * Dominance frontier of x: 
        *   The set of all nodes y such that x dominates a predecessor 
        *   of y but x doesn't strictly dominates y.
        *)
       fun DF x =
       let val stamp = new_stamp()
           val level_x = A.sub(levels,x)
           fun walk(z, S) = 
               let fun scan((_,y,_)::es,S) =
                       if A.sub(levels,y) <= level_x andalso
                           unmarked(in_DF,y,stamp) then scan(es,y::S)
                       else scan(es,S)
                     | scan([],S) = S
                   val S = scan(#out_edges cfg z,S)
                   fun walkList([],S) = S
                     | walkList((_,z,_)::es,S) = walkList(es,walk(z,S))
               in  walkList(#out_edges dom z,S)
               end
       in  walk(x,[])
       end

   in  DF end

   (* Compute iterated dominance frontier *)
   fun IDFs (D as G.GRAPH dom) = 
   let val G.GRAPH cfg = Dom.cfg D
       val L           = Dom.max_levels D
       val N           = #capacity dom ()
       val levels      = Dom.levelsMap D
       val in_DF       = A.array(N,0)  (* has appeared in the DF set? *)
       val stamp       = ref 0
       fun new_stamp() = let val s = !stamp + 1 in stamp := s; s end

       fun unmarked(marked,i,stamp : int) =
           let val s = A.sub(marked,i)
           in  if s = stamp then false else (A.update(marked,i,stamp); true)
           end

       val in_alpha  = A.array(N,0)  (* has appeared in N_alpha? *)
       val visited   = A.array(N,0)  (* has it been visited *)
       val piggybank = A.array(L,[]) (* nodes in the piggy bank *)

       (* 
        * This algorithm is described in POPL 95 
        *)
       fun IDFs xs =
       let val stamp = new_stamp()
           fun init([],l) = l
             | init(x::xs,l) = 
               let val l_x = A.sub(levels,x)
               in  A.update(in_alpha,x,stamp);
                   A.update(piggybank,l_x,x::A.sub(piggybank,l_x));
                   init(xs,if l < l_x then l_x else l)
               end 
           fun visit(y,level_x,S) =
           let fun scan([],S) = S
                 | scan((_,z,_)::es,S) =
                   let val level_z = A.sub(levels,z)
                   in  if level_z <= level_x andalso unmarked(in_DF,z,stamp) 
                       then (if A.sub(in_alpha,z) <> stamp 
                             then A.update(piggybank,level_z,
                                           z::A.sub(piggybank,level_z)) 
                             else ();
                             scan(es,z::S))
                       else scan(es,S)  
                   end
               fun visitSucc([],S) = S
                 | visitSucc((_,z,_)::es,S) = 
                   visitSucc(es,if unmarked(visited,z,stamp)
                                then visit(z,level_x,S) else S)
               val S = scan(#out_edges cfg y,S)
           in  visitSucc(#out_edges dom y,S) 
           end 

           fun visitAll(~1,S) = S
             | visitAll(l,S) =
               case A.sub(piggybank,l) of
                 [] => visitAll(l-1,S)
               | x::xs => (A.update(visited,x,stamp);
                           A.update(piggybank,l,xs);
                           visitAll(l,visit(x,A.sub(levels,x),S)))

           val L = init(xs,~1) 
       in  visitAll(L,[])
       end

   in  IDFs
   end


   (* Compute iterated dominance frontier intersected with liveness.
    * This is my special algorithm!  The idea is that when we find a
    * new node b in IDF^+(S) we first check whether b is liveIn.  If not,
    * we can prune the search right there.  If so, we continue as normal.
    * Checking whether something is liveIn triggers the incremental liveness 
    * routine.
    *
    * -- Allen
    *)
   datatype kind = JOIN | DOM

   fun LiveIDFs(D as G.GRAPH dom) = 
   let val G.GRAPH cfg = Dom.cfg D
       val L           = Dom.max_levels D
       val N           = #capacity dom ()
       val levels      = Dom.levelsMap D

       val in_phi      = A.array(N,0)  (* has appeared in the DF set? *)
       val stamp       = ref 0
       fun new_stamp() = let val s = !stamp + 2 in stamp := s; s end

       val in_alpha   = A.array(N,0)  (* has appeared in N_alpha? *)
       val piggybank  = A.array(L,[]) (* nodes in the piggy bank *)
       val minJLevels = A.array(N,10000000)  
       val djGraph    = A.array(N,[]) (* path compressed dj graph *)
       val liveIn     = A.array(N,0) (* is a variable live in *)
       val visited    = A.array(N,0)
       val strictly_dominates = Dom.dominates D

       val K_inf = 255

       fun compressDJGraph(X, lvl) =
       let val nextLvl = lvl + 1
           val stamp   = ~X

           (* merge join list, make sure there are no duplicates *)
           fun mergeJoin(Z, E, n) = 
               if A.sub(visited, Z) = stamp orelse
                  A.sub(levels, Z) >= lvl then (E, n)
               else (A.update(visited, Z, stamp);
                     (Z::E, n+1))
 
           fun mergeJoins([], E, n) = (E, n)
             | mergeJoins(Z::Zs, E, n) = 
               let val (E, n) = mergeJoin(Z, E, n)
               in  mergeJoins(Zs, E, n)
               end

           fun appendJoins([], E) = E
             | appendJoins(Z::Zs, E) = appendJoins(Zs, (JOIN,Z)::E)

           fun collapse([], DJ_X) = DJ_X
             | collapse((e as (DOM,_))::Zs, DJ_X) = collapse(Zs, e::DJ_X) 
             | collapse((e as (JOIN,Z))::Zs, DJ_X) = 
               if A.sub(levels, Z) <= lvl then collapse(Zs, e::DJ_X) 
               else collapse(Zs, DJ_X)

           (* L_X   -- min level of all join edges in SubTree(X)
            * DJ_X  -- all dj-graph edges of X
            * E_X   -- all J-edges in SubTree(X) to level < lvl.
            * K_X   -- |E_X|
            *)
           fun walkDomSucc([], L_X, DJ_X, E_X, K_X) = (L_X, DJ_X, E_X, K_X)
             | walkDomSucc((_,Y,_)::es, L_X, DJ_X, E_X, K_X) =
               let val (L_Y, E_Y, K_Y) = compressDJGraph(Y, nextLvl)
                   val L_X = Int.min(L_X, L_Y)
               in  if pathPrune then
                      if L_Y >= nextLvl then
                         (* disconnect dom edge! *)
                          walkDomSucc(es, L_X, DJ_X, E_X, K_X)
                      else if K_Y <= K_max then
                         (* path compress! *)
                       let val (E_X, K_X) = mergeJoins(E_Y, E_X, K_X)
                       in walkDomSucc(es, L_X, appendJoins(E_Y, DJ_X), E_X, K_X)
                       end
                      else 
                       let val Zs = A.sub(djGraph, Y)
                       in  if length Zs <= K_max then
                             walkDomSucc(es, L_X, collapse(Zs,DJ_X), [], K_inf)
                           else
                             walkDomSucc(es, L_X, (DOM,Y)::DJ_X, [], K_inf)
                       end
                   else    
                      walkDomSucc(es, L_X, (DOM,Y)::DJ_X, [], K_inf)
               end
           fun walkCFGSucc([], L_X, DJ_X, E_X, K_X) = (L_X, DJ_X, E_X, K_X)
             | walkCFGSucc((_,Y,_)::es, L_X, DJ_X, E_X, K_X) = 
               let val L_X = Int.min(L_X, A.sub(levels, Y))
                   val (E_X, K_X) = mergeJoin(Y, E_X, K_X)
               in  walkCFGSucc(es, L_X, (JOIN,Y)::DJ_X, E_X, K_X)
               end
 
           val (L_X, DJ_X, E_X, K_X) = 
                 walkDomSucc(#out_edges dom X, 10000000, [], [], 0)
           val (L_X, DJ_X, E_X, K_X) = 
                 walkCFGSucc(#out_edges cfg X, L_X, DJ_X, E_X, K_X)

       in  A.update(minJLevels, X, L_X);
           A.update(djGraph, X, DJ_X);
           (L_X, E_X, K_X)
       end

       val [ENTRY] = #entries dom () 
       val _ = compressDJGraph(ENTRY, 0)


       fun LiveIDFs {defs, localLiveIn=[]} = [] (* special case *)
         | LiveIDFs {defs=xs, localLiveIn} = 
       let val stamp = new_stamp()
           (* val n = ref 0
           val m = ref 0 *)

           fun initDefs([],maxLvl) = maxLvl
             | initDefs(x::xs,maxLvl) =
               let val lvl_x = A.sub(levels,x)
               in  A.update(in_alpha,x,stamp);
                   A.update(piggybank,lvl_x,x::A.sub(piggybank,lvl_x));
                   initDefs(xs,if maxLvl < lvl_x then lvl_x else maxLvl)
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
           in  (* m := !m + 1; *)
               A.update(liveIn,b,stamp);
               if stats then liveVisitCount := !liveVisitCount + 1 else ();
               markPred(#in_edges cfg b)
           end

           fun initLiveIn [] = ()
             | initLiveIn(x::xs) = (markLiveIn x; initLiveIn xs)

           fun isLive b = A.sub(liveIn,b) = stamp

           fun visit(y,level_x,S) =
           let fun foreach([],S) = S
                 | foreach((JOIN,z)::zs,S) = 
                   let val level_z = A.sub(levels,z)
                   in  if level_z <= level_x andalso
                          A.sub(in_phi,z) <> stamp andalso
                          isLive z
                           (* z is a new IDF^+ candidate; 
                            * make sure it is live.
                            *)
                       then (A.update(in_phi,z,stamp);
                             if A.sub(in_alpha,z) <> stamp 
                             then A.update(piggybank,level_z,
                                           z::A.sub(piggybank,level_z)) 
                             else ();
                             foreach(zs,z::S)
                            )
                       else foreach(zs,S)  
                   end
                 | foreach((DOM,z)::zs,S) = 
                   foreach(zs,if isLive z andalso 
                                   A.sub(visited,z) <> stamp andalso
                                   (not levelPrune orelse 
                                    A.sub(minJLevels,z) <= level_x) 
                                then (A.update(visited,z,stamp);
                                      visit(z,level_x,S)
                                     ) 
                                else S)
           in  if stats then visitCount := !visitCount + 1 else ();
               foreach(A.sub(djGraph, y),S) 
           end 

           fun visitAll(~1,S) = S
             | visitAll(l,S) =
               case A.sub(piggybank,l) of
                 [] => visitAll(l-1,S)
               | x::xs => 
                  let val _ = A.update(piggybank,l,xs)
                      val _ = A.update(visited,x,stamp);
                      val S = visit(x, A.sub(levels, x), S)
                  in  
                      visitAll(l,S)
                  end

           fun domTest([x],uses) = 
               let fun loop [] = true
                     | loop(y::ys) = strictly_dominates(x,y) andalso loop ys    
               in  loop uses end
             | domTest _ = false

       in  if domPrune andalso domTest(xs,localLiveIn) then []
           else 
             let val L = initDefs(xs, ~1) 
             in  initLiveIn(localLiveIn);
                 visitAll(L, [])
             end
       end

   in  LiveIDFs
   end

end

