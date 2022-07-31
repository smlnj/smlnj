(* 
 * The algorithm for computing iterated dominance frontier.
 * This is the algorithm by Sreedhar, Gao and Lee.   
 * 
 * --Allen
 *)

functor DJGraph (Dom : DOMINATOR_TREE) : DJ_GRAPH =
struct

   structure G       = Graph
   structure Dom     = Dom
   structure A       = Array

   type ('n,'e,'g) dj_graph = ('n,'e,'g) Dom.dominator_tree

   fun error msg = MLRiscErrorMsg.error("DJGraph",msg)

   val stats          = false (* collect statistics? *)
   val visitCount     = MLRiscControl.getCounter "dj-visit-count"
   val idfCount       = MLRiscControl.getCounter "dj-IDF-count"
   val idfSize        = MLRiscControl.getCounter "dj-IDF-size"
   val liveVisitCount = MLRiscControl.getCounter "dj-live-visit-count"
   val maxBlockSize   = MLRiscControl.getCounter "dj-max-block-size"
   val totalBlockSize = MLRiscControl.getCounter "dj-total-block-size"
   val debug          = false

   fun DJ x = x 

   (* Compute dominance frontier *)
   fun DF (D as G.GRAPH dom) =
   let val G.GRAPH cfg = Dom.cfg D
       val L           = Dom.max_levels D
       val N           = #capacity dom ()
       val levels      = Dom.levelsMap D
       val in_phi      = A.array(N,0)  (* has appeared in the DF set? *)
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
                           unmarked(in_phi,y,stamp) then scan(es,y::S)
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
       val in_phi      = A.array(N,0)  (* has appeared in the DF set? *)
       val stamp       = ref 0
       fun new_stamp() = let val s = !stamp + 1 in stamp := s; s end

       fun unmarked(marked,i,stamp : int) =
           let val s = A.sub(marked,i)
           in  if s = stamp then false else (A.update(marked,i,stamp); true)
           end

       val in_alpha  = A.array(N,0)  (* has appeared in N_alpha? *)
       val visited   = A.array(N,0)  (* has it been visited *)
       val piggybank = A.array(L,[]) (* nodes in the piggy bank *)

       val n = ref 0
       (* 
        * This algorithm is described in POPL 95 
        *)
       fun IDFs xs =
       let val stamp = new_stamp()
           val _ = if stats then (idfCount := !idfCount + 1; n := !visitCount) 
                   else ()
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
                   in  if level_z <= level_x andalso unmarked(in_phi,z,stamp) 
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
           in  if stats then visitCount := !visitCount + 1 else ();
               visitSucc(#out_edges dom y,S) 
           end 

           fun visitAll(~1,S) = S
             | visitAll(l,S) =
               case A.sub(piggybank,l) of
                 [] => visitAll(l-1,S)
               | x::xs => (A.update(visited,x,stamp);
                           A.update(piggybank,l,xs);
                           visitAll(l,visit(x,A.sub(levels,x),S)))

           val L = init(xs,~1) 
           val IDF = visitAll(L,[])
       in  if stats then
               (idfSize := !idfSize + length IDF;
                maxBlockSize := Int.max(!maxBlockSize, N);
                totalBlockSize := !totalBlockSize + N
               )
           else ();
           if debug then print("N="^Int.toString N^" visits="^
                               Int.toString(!visitCount - !n)^"\n") else ();
           IDF
       end

   in  IDFs
   end

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
       val liveIn     = A.array(N,0) (* is a variable live in *)
       val visited    = A.array(N,0)

       fun unmarked(marked,i,stamp : int) =
           let val s = A.sub(marked,i)
           in  if s = stamp then false else (A.update(marked,i,stamp); true)
           end

       fun LiveIDFs {defs, localLiveIn=[]} = [] (* special case *)
         | LiveIDFs {defs=xs, localLiveIn} = 
       let val stamp = new_stamp()
           val _ = if stats then idfCount := !idfCount + 1 else ()
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
           let fun scan([],S) = S
                 | scan((_,z,_)::es,S) =
                   let val level_z = A.sub(levels,z)
                   in  if level_z <= level_x andalso 
                          isLive z andalso
                          unmarked(in_phi,z,stamp) 
                       then (if A.sub(in_alpha,z) <> stamp 
                             then A.update(piggybank,level_z,
                                           z::A.sub(piggybank,level_z)) 
                             else ();
                             scan(es,z::S))
                       else scan(es,S)  
                   end
               fun visitSucc([],S) = S
                 | visitSucc((_,z,_)::es,S) = 
                   visitSucc(es,if isLive z andalso unmarked(visited,z,stamp)
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

           val L = initDefs(xs, ~1) 
       in  initLiveIn(localLiveIn);
           visitAll(L, [])
       end

   in  LiveIDFs
   end

end

