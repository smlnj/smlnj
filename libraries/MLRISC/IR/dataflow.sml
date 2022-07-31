(*
 * This is just a very simple worklist based data analyzer.
 * Parameterized by the dataflow problem.
 *
 * -- Allen
 *)

functor Dataflow(P : DATAFLOW_PROBLEM) : DATAFLOW_ANALYZER =
struct
   structure G   = Graph
   structure A   = Array
   structure CFG = P.CFG

   type dataflow_info = P.dataflow_info

   fun analyze (CFG, info) =
   let val G' as G.GRAPH cfg = if P.forward then CFG
                               else ReversedGraphView.rev_view CFG
       val N         = #capacity cfg ()
       val ENTRY     = case #entries cfg () of
                         [ENTRY] => ENTRY
                       | _ => raise Graph.NotSingleEntry
       val inputs    = A.array(N, P.bot)
       val outputs   = A.array(N, P.bot)
       val transfers = A.array(N, fn _ => P.bot)
       val prologue  = P.prologue(CFG,info)
       val _         = #forall_nodes cfg  
                          (fn (n,n') => 
                           let val {input,output,transfer} = prologue(n,n')
                           in  A.update(inputs,n,input);
                               A.update(outputs,n,output);
                               A.update(transfers,n,transfer)
                           end
                          )

       abstype worklist = WL of int list * int list
       with 
            exception EmptyQueue
            val on_queue = A.array(N,false)
            val empty = WL([],[])
            val empty = WL([],[])
            fun enque(wl as WL(b,f),i)  = 
                if A.sub(on_queue,i) then wl else
                (A.update(on_queue,i,true); WL(i::b,f))
            fun deque(WL([],[]))  = raise EmptyQueue
              | deque(WL(b,[]))   = deque(WL([],rev b))
              | deque(WL(b,i::f)) = (A.update(on_queue,i,false); (WL(b,f),i))
           fun insert(wl,[]) = wl
             | insert(wl,(_,n,_)::es) = insert(enque(wl,n),es)
           fun insert'(wl,[]) = wl 
             | insert'(wl,n::ns) = insert'(enque(wl,n),ns)
       end

       fun propagate worklist =
       let val (worklist, i) = deque worklist
           val new_input   = 
               case #in_edges cfg i of
                 [(p,_,_)] => if p = ENTRY then A.sub(inputs,i)
                              else A.sub(outputs,p)
               | x => P.join(map (fn(i,_,_) => A.sub(outputs,i)) x)
           val old_output  = A.sub(outputs,i)
           val f           = A.sub(transfers,i)
           val new_output  = f new_input
       in  A.update(inputs,i,new_input);
           if P.==(old_output, new_output) then propagate worklist
           else (A.update(outputs,i,new_output);
                 propagate(insert(worklist,#out_edges cfg i)))
       end
      
       val nodes    = GraphTopsort.topsort G' (#entries cfg ())
       val _        = propagate(insert'(empty,nodes)) handle EmptyQueue => ()
       val epilogue = P.epilogue(CFG,info)
       val _        = #forall_nodes cfg
                         (fn (n,n') => epilogue{input  = A.sub(inputs,n),
                                                output = A.sub(outputs,n),
                                                node   = (n,n')})
   in
       info
   end 

end

