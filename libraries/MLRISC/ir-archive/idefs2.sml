(*
 * This is Reif and Tarjan's algorithm (SIAM J Computing 1981) 
 * for computing approximate birthpoints for expressions.   
 * For each basic block B,
 *   idef(x) = { defs(v_i) | i = 1 ... n in all paths 
 *                           idom(x) v_1 v_2 ... v_n x where n >= 1 and
 *                                   v_i <> idom(x) for all 1 <= i <= n
 *             }
 * -- Allen
 *)

structure IDefs : IDEFS =
struct

   structure G    = Graph
   structure SL   = SortedList
   structure A    = Array
   structure Rev  = ReversedGraphView

   type var = int

   fun compute_idefs {def_use, cfg} =
   let val CFG as G.GRAPH cfg  = cfg
       val N                   = #capacity cfg ()
       val DU                  = A.array(N,([],[]))
       val _ = #forall_nodes cfg 
            (fn (b,b') => let val (d,u) = def_use(b,b')
                          in  A.update(DU,b,(SL.uniq d,SL.uniq u))
                          end)
       fun dump(name,a) =
          (print(name^"=");
           A.appi (fn (i,v) => 
               print(Int.toString i ^ "=" ^Int.toString v^" "))
                  (a,0,NONE);
           print "\n")

       fun tarjan_lengauer(G.GRAPH cfg) =
       let val [ENTRY]    = #entries cfg ()
           val vertex     = A.array(N,~1)
           val parent     = A.array(N,~1)
           val semi       = A.array(N,~1)
           val bucket     = A.array(N,[])
           val dom        = A.array(N,~1)
           val sdefuse    = A.array(N,([],[]))
           val idefuse    = A.array(N,([],[]))
           val ancestor   = A.array(N,~1)
           val treeparent = A.array(N,~1)
           val label      = A.array(N,~1)
           fun dfs(p,n,i) =
               if A.sub(semi,i) <> ~1 then n 
               else
               (A.update(parent,i,p);
                A.update(semi,i,n);
                A.update(vertex,n,i);
                A.update(label,i,i);
                dfs'(i,n+1,#succ cfg i)
               ) 
           and dfs'(p,n,[])    = n
             | dfs'(p,n,i::is) = dfs'(p,dfs(p,n,i),is) 
           val n = dfs(~1,0,ENTRY)

           fun COMPRESS v =
               if A.sub(ancestor,A.sub(ancestor,v)) <> ~1 then
                 (COMPRESS(A.sub(ancestor,v));
                  let val label_ancestor_v = A.sub(label,A.sub(ancestor,v))
                      val label_v          = A.sub(label,v)
                  in  if A.sub(semi,label_ancestor_v) <
                         A.sub(semi,label_v) then
                         A.update(label,v,label_ancestor_v)
                      else ()
                  end;
                  A.update(ancestor,v,A.sub(ancestor,A.sub(ancestor,v)))
                 )
              else ()
                    
           fun LINK(v,w) = (A.update(ancestor,w,v);
                            A.update(treeparent,w,v))
           fun EVAL v =
               if A.sub(ancestor,v) = ~1 then v
               else (COMPRESS v; A.sub(label,v))
           fun EVALDEFUSE v = 
               let fun up(v,D,U) =
                   let val p = A.sub(treeparent,v)
                   in  if p = ~1 then (D,U)
                       else let val (d,u)   = A.sub(DU,v)
                                val (d',u') = A.sub(sdefuse,v)
                            in  up(p,SL.merge(d,SL.merge(d',D)),
                                     SL.merge(u,SL.merge(u',U)))
                            end
                   end
               in
                   up(v,[],[])
               end
           fun step2_3 0 = ()
             | step2_3 i = 
               let val w = A.sub(vertex,i)
                   val parent_w = A.sub(parent,w)
                   fun step2 [] = ()
                     | step2 ((v,_,_)::vs) =
                       let val u      = EVAL v
                           val semi_u = A.sub(semi,u)
                       in  if semi_u < A.sub(semi,w) then 
                              A.update(semi,w,semi_u) 
                           else ();
                           let val (d,u) = EVALDEFUSE v
                               val (d',u') = A.sub(sdefuse,w)
                           in  A.update(sdefuse,w,(SL.merge(d,d'),
                                                   SL.merge(u,u')))
                           end;
                           step2 vs
                       end
                   val _      = step2(#in_edges cfg w)
                   val vertex_semi_w = A.sub(vertex,A.sub(semi,w))
                   val _      = A.update(bucket,vertex_semi_w,
                                  w::A.sub(bucket,vertex_semi_w))
                   val _      = LINK(parent_w,w)
                   fun step3 [] = ()
                     | step3 (v::vs) =
                       let val u = EVAL v
                       in  A.update(dom,v,if A.sub(semi,u) < A.sub(semi,v)
                                          then u else parent_w);
                           let val (d,u) = A.sub(sdefuse,v)
                               val (d',u') = EVALDEFUSE(A.sub(parent,v))
                           in  A.update(idefuse,v,(SL.merge(d,d'),
                                                   SL.merge(u,u')))
                           end;
                           step3 vs
                       end
                   val _ = step3 (A.sub(bucket,parent_w))
                   val _ = A.update(bucket,parent_w,[])
               in  step2_3(i-1)
               end
           val _ = step2_3(n-1)
          (*
           val _ = print("n = "^Int.toString n^"\n")
           val _ = dump("vertex",vertex)
           val _ = dump("parent",parent)
           val _ = dump("semi",semi)
           val _ = dump("dom",dom)
           val _ = dump("ancestor",ancestor)
           val _ = dump("label",label)
           *)
           fun step4 i = 
               if i = n then ()
               else let val w = A.sub(vertex,i)
                    in  if A.sub(dom,w) <> A.sub(vertex,A.sub(semi,w)) then
                           let val (d,u) = A.sub(idefuse,A.sub(dom,w))
                               val (d',u') = A.sub(idefuse,w)
                           in  A.update(idefuse,w,(SL.merge(d,d'),
                                                   SL.merge(u,u')));
                               A.update(dom,w,A.sub(dom,A.sub(dom,w)))
                           end
                        else ();
                        step4(i+1)
                    end
           val _ = step4 1
       in  idefuse
       end
   in 
       {idefuse     = fn _ => tarjan_lengauer(CFG),
        ipostdefuse = fn _ => tarjan_lengauer(Rev.rev_view CFG)
       }
   end

end

