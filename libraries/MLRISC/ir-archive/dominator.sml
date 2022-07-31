(* 
 * Computation of the dominator tree representation from the
 * control flow graph.  I'm using the old algorithm by Lengauer and Tarjan.
 *
 * Note: to deal with CFG with endless loops,
 * by default we assume instructions are postdominated by STOP. 
 *
 * -- Allen
 *)

functor DominatorTree (GraphImpl : GRAPH_IMPLEMENTATION
                      ) : DOMINATOR_TREE =
struct

   structure GI      = GraphImpl
   structure G       = Graph
   structure Rev     = ReversedGraphView
   structure A       = Array 
   structure NodeSet = BitSet

   exception Dominator

   fun singleEntryOf (G.GRAPH g) =
       case #entries g () of
	   [e] => e
	 | _ => raise Dominator
   
   type node = G.node_id

   datatype ('n,'e,'g) dom_info = 
       INFO of
       { cfg : ('n,'e,'g) G.graph, 
         edge_label : string,
         levelsMap  : int Array.array,
         preorder   : int Array.array option ref,
         postorder  : int Array.array option ref,
         entryPos   : int Array.array option ref,
         max_levels : int ref
       }
   type ('n,'e,'g) dominator_tree     = ('n,unit,('n,'e,'g) dom_info) G.graph
   type ('n,'e,'g) postdominator_tree = ('n,unit,('n,'e,'g) dom_info) G.graph 

   fun graph_info (G.GRAPH dom) : ('n,'e,'g) dom_info = #graph_info dom 

   fun cfg(G.GRAPH dom) = let val INFO{cfg,...} = #graph_info dom in cfg end
   fun max_levels(G.GRAPH dom) = 
   let val INFO{max_levels,...} = #graph_info dom in !max_levels end

   (*
    * This is the main Lengauer/Tarjan algorithm
    *)
   fun tarjan_lengauer (name,edge_label) (origCFG,CFG as (G.GRAPH cfg)) =
   let val N           = #order cfg ()
       val M           = #capacity cfg ()
       val r           = singleEntryOf CFG
       val in_edges    = #in_edges cfg
       val succ        = #succ cfg
       val dfnum       = A.array (M, ~1)
       val vertex      = A.array (N, ~1) 
       val parent      = A.array (M, ~1)  
       val bucket      = A.array (M, []) : node list array
       val semi        = A.array (M, r)  
       val ancestor    = A.array (M, ~1) 
       val idom        = A.array (M, r) 
       val samedom     = A.array (M, ~1)
       val best        = A.array (M, ~1)
       val max_levels  = ref 0
       val levelsMap   = A.array(M,~1000000)
       val dom_info    = INFO{ cfg        = origCFG, 
                               edge_label = edge_label,
                               levelsMap  = levelsMap,
                               preorder   = ref NONE,
                               postorder  = ref NONE,
                               entryPos   = ref NONE,
                               max_levels = max_levels 
                             }
       val Dom as G.GRAPH domtree = GI.graph(name, dom_info, N)

       (* step 1 
        * Initialize semi dominators and parent map
        *)
       fun dfs(p,n,N) =
         if A.sub(dfnum,n) = ~1 then
            (A.update(dfnum,n,N);
             A.update(vertex,N,n);
             A.update(parent,n,p);
             dfsSucc(n,succ n,N+1)
            )
         else N
       and dfsSucc(p,[],N)    = N
         | dfsSucc(p,n::ns,N) = dfsSucc(p,ns,dfs(p,n,N))

       and dfsAll(n::ns,N) = dfsAll(ns,dfs(~1,n,N))
         | dfsAll([],N)    = ()
       val nonRoots = List.foldr 
                        (fn ((r',_),l) => if r <> r' then r'::l else l) []
                          (#nodes cfg ())
       val _ = dfsAll(nonRoots,dfs(~1,r,0))

       (*
       fun pr s = print (s ^ "\n")
       fun dumpArray title a = 
          pr(title ^ ": " ^
                      String.concat(A.foldr 
                         (fn (i,s) => Int.toString i::" "::s) [] a))

       val _ = pr("root = " ^ Int.toString r)
       val _ = dumpArray "vertex" vertex
       val _ = dumpArray "dfnum" dfnum
       val _ = dumpArray "parent" parent
       val _ = Msg.printMessages(fn _ => CFG.G.printGraph (!Msg.outStream) cfg)
       *)

       fun link(p,n) = (A.update(ancestor,n,p); A.update(best,n,n))

       fun ancestorWithLowestSemi v =
       let val a = A.sub(ancestor,v)
       in  if a <> ~1 andalso A.sub(ancestor,a) <> ~1 then
              let val b = ancestorWithLowestSemi a
              in  A.update(ancestor,v,A.sub(ancestor,a));
                  if A.sub(dfnum,A.sub(semi,b)) <
                     A.sub(dfnum,A.sub(semi,A.sub(best,v))) then
                     A.update(best,v,b)
                  else ()
              end
           else ();
           let val u = A.sub(best,v) 
           in  if u = ~1 then v else u
           end
       end

       (* steps 2 and 3
        * Compute vertex, bucket and semi maps 
        *)
       fun compute 0 = ()
         | compute i = 
           let val n = A.sub(vertex,i)
               val p = A.sub(parent,n)
               fun computeSemi ((v,n,_)::rest,s) =
                  if v = n then computeSemi(rest,s)
                  else
                  let val s' = if A.sub(dfnum,v) < A.sub(dfnum,n) then v
                               else A.sub(semi,ancestorWithLowestSemi(v))
                      val s  = if A.sub(dfnum,s') < 
                                  A.sub(dfnum,s) then s'
                               else s
                  in  computeSemi(rest,s) 
                  end
                | computeSemi ([], s) = s
           in  if p <> ~1 then
               let val s = computeSemi(in_edges n, p)
               in  A.update(semi,n,s);
                   A.update(bucket,s,n::A.sub(bucket,s));
                   link(p,n);
                   app (fn v => 
                      let val y = ancestorWithLowestSemi(v)
                      in  if A.sub(semi,y) = A.sub(semi,v) then
                             A.update(idom,v,p) else A.update(samedom,v,y)
                      end) (A.sub(bucket,p));
                   A.update(bucket,p,[])
               end else ();
               compute(i-1)
           end
       val _ = compute (N-1)

       (*
       val _ = dumpArray "semi" idom
       val _ = dumpArray "idom" idom
        *)

       (* step 4 update dominators *)
       fun updateIdoms i = 
         if i < N then
            let val n = A.sub(vertex, i)
            in  if A.sub(samedom, n) <> ~1 
                then A.update(idom, n, A.sub(idom, A.sub(samedom, n)))
                else ();
                updateIdoms (i+1)   
            end 
         else ()
       val _ = updateIdoms 1

       (*
       val _ = dumpArray "idom" idom
        *)

       (* Create the nodes/edges of the dominator tree *)
       fun buildGraph(i,maxLevel) =
           if i < N then
           let val v = A.sub(vertex,i)
           in  #add_node domtree (v,#node_info cfg v);
               if v <> r then  
                  let val w = A.sub(idom,v)
                      val l = A.sub(levelsMap,w)+1
                  in  A.update(levelsMap,v,l);
                      #add_edge domtree (w,v,());
                      buildGraph(i+1,if l >= maxLevel then l else maxLevel)  
                  end
               else 
                 (A.update(levelsMap,v,0);
                  buildGraph(i+1,maxLevel)
                 )
           end
           else maxLevel

       val max = buildGraph(0,1)
   in  
       max_levels := max+1;
       #set_entries domtree [r];
       (* Msg.printMessages(fn _ => G.printGraph (!Msg.outStream) domtree); *)
       Dom
   end

 
   (* The algorithm specialized to making dominators and postdominators *)
   fun makeDominator cfg = tarjan_lengauer("Dom","dom") (cfg,cfg)
   fun makePostdominator cfg = 
        tarjan_lengauer("PDom","pdom") (cfg,Rev.rev_view cfg)

   (* Methods *)

   (* Does i immediately dominate j? *)
   fun immediately_dominates (G.GRAPH D) (i,j) =
        case #in_edges D j of
           (k,_,_)::_ => i = k
        |  _ => false

   (* immediate dominator of n *)
   fun idom (G.GRAPH D) n = 
       case #in_edges D n of
          (n,_,_)::_ => n
       |  _          => ~1

   (* nodes that n immediately dominates *) 
   fun idoms (G.GRAPH D) = #succ D

   (* nodes that n dominates *)
   fun doms (G.GRAPH D) = 
   let fun subtree ([],S) = S
         | subtree (n::ns,S) = subtree(#succ D n,subtree(ns,n::S))
   in  fn n => subtree([n], []) 
   end


   fun prePostOrders(g as G.GRAPH dom) =
   let val INFO{ preorder,postorder,...} = #graph_info dom
       (* Compute the preorder/postorder numbers *)
       fun computeThem() =
       let val N   = #capacity dom ()
           val r = singleEntryOf g
           val pre  = A.array(N,~1000000)
           val post = A.array(N,~1000000)
           fun computeNumbering(preorder,postorder,n) = 
           let val _ = A.update(pre,n,preorder)
               val (preorder',postorder') =
                     computeNumbering'(preorder+1,postorder,#out_edges dom n)
           in  A.update(post,n,postorder');
               (preorder',postorder'+1)
           end

           and computeNumbering'(preorder,postorder,[]) =
                    (preorder,postorder)
             | computeNumbering'(preorder,postorder,(_,n,_)::es) =
                 let val (preorder',postorder') = 
                       computeNumbering(preorder,postorder,n)
                     val (preorder',postorder') =
                       computeNumbering'(preorder',postorder',es)
                 in  (preorder',postorder')
                 end
       in  computeNumbering(0,0,r) ;
           preorder := SOME pre;
           postorder := SOME post;
           (pre,post)
       end
   in  case (!preorder,!postorder) of
         (SOME pre,SOME post) => (pre,post)
       | _ => computeThem()
   end

   (* Level *)
   fun level (G.GRAPH D) = 
   let val INFO{levelsMap,...} = #graph_info D
   in  fn i => A.sub(levelsMap,i) end

   (* Entry position *) 
   fun entryPos(g as G.GRAPH D) =
   let val INFO{entryPos,...} = #graph_info D
   in  case !entryPos of
         SOME t => t
       | NONE => 
         let val entry = singleEntryOf g
             val N       = #capacity D ()
             val t       = A.array(N, entry)
             fun init(X,Y) = 
               (A.update(t,X,Y);
                app (fn Z => init(Z,Y)) (#succ D X)
               )
         in  entryPos := SOME t;
             app (fn Z => init(Z,Z)) (#succ D entry);
             t
         end
   end
    
   (* Least common ancestor *)
   fun lca (Dom as G.GRAPH D) (a,b) =
   let val l_a = level Dom a 
       val l_b = level Dom b
       fun idom i = case #in_edges D i of
			(j,_,_)::_ => j
		      | [] => raise Fail "DominatorTree:lca:idom: []"
       fun up_a(a,l_a) = if l_a > l_b then up_a(idom a,l_a-1) else a
       fun up_b(b,l_b) = if l_b > l_a then up_b(idom b,l_b-1) else b
       val a = up_a(a,l_a)
       val b = up_b(b,l_b)
       fun up_both(a,b) = if a = b then a else up_both(idom a,idom b)
   in  up_both(a,b) end

   (* is x and ancestor of y in D?
    * This is true iff PREORDER(x) <= PREORDER(y) and
    *                  POSTORDER(x) >= POSTORDER(y)
    *)
   fun dominates Dom =
   let val (pre,post) = prePostOrders Dom
   in  fn (x,y) =>
       let val a = A.sub(pre,x)
           val b = A.sub(post,x)
           val c = A.sub(pre,y)
           val d = A.sub(post,y)
       in  a <= c andalso b >= d
       end
   end

   fun strictly_dominates Dom = 
   let val (pre,post) = prePostOrders Dom
   in  fn (x,y) =>
       let val a = A.sub(pre,x)
           val b = A.sub(post,x)
           val c = A.sub(pre,y)
           val d = A.sub(post,y)
       in  a < c andalso b > d
       end
   end

   fun control_equivalent (Dom,PDom) =
   let val dom  = dominates Dom
       val pdom = dominates PDom
   in  fn (x,y) => dom(x,y) andalso pdom(y,x) orelse dom(y,x) andalso pdom(x,y)
   end

   (* control equivalent partitions 
    * two nodes a and b are control equivalent iff
    *    a dominates b and b postdominates a (or vice versa) 
    * We use the following property of dominators to avoid wasteful work:
    *    If i dom j dom k and j not pdom i then
    *          k not pdom i
    * This algorithm runs in O(n)  
    *)
   fun control_equivalent_partitions (G.GRAPH D,PDom) =
   let val postdominates = dominates PDom
       fun walkDom([],S) = S
         | walkDom(n::waiting,S) =
            let val (waiting,S,S') = 
                    findEquiv(n,#out_edges D n,waiting,S,[n])
            in  walkDom(waiting,S'::S)
            end
       and findEquiv(i,[],waiting,S,S') = (waiting,S,S')
         | findEquiv(i,(_,j,_)::es,waiting,S,S') =
             if postdominates(j,i) then
                let val (waiting,S,S') = findEquiv(i,es,waiting,S,j::S')
                in  findEquiv(i,#out_edges D j,waiting,S,S')
                end
             else
                findEquiv(i,es,j::waiting,S,S')

       val equivSets = walkDom(#entries D (),[])
   in
       equivSets
   end

   fun levelsMap(G.GRAPH dom) =
   let val INFO{levelsMap,...} = #graph_info dom
   in  levelsMap end

   fun idomsMap(G.GRAPH dom) =
   let val idoms = A.array(#capacity dom (),~1)
   in  #forall_edges dom (fn (i,j,_) => A.update(idoms,j,i));
       idoms
   end 

end

