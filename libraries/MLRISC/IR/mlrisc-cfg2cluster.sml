(*
 *  Convert the new control flow graph format back into the old cluster format
 *
 *  -- Allen
 *)

signature CFG2CLUSTER =
sig
   structure CFG : CONTROL_FLOW_GRAPH
   structure F   : FLOWGRAPH
      sharing CFG.I = F.I
      sharing CFG.P = F.P

   (* 
    * If relayout is true, then always use the layout algorithm.
    * Otherwise, try to preserve the original layout if possible.
    *)
   val cfg2cluster : { cfg      : CFG.cfg,
                       relayout : bool   
                     } -> F.cluster

end 

functor CFG2Cluster
   (structure CFG       : CONTROL_FLOW_GRAPH
    structure Flowgraph : FLOWGRAPH
       sharing CFG.I = Flowgraph.I
       sharing CFG.P = Flowgraph.P
   ) : CFG2CLUSTER =
struct

    structure CFG      = CFG
    structure W        = CFG.W
    structure F        = Flowgraph
    structure G        = Graph
    structure Q        = PriorityQueue
    structure Set      = BitSet
    structure A        = Array

    fun error msg = MLRiscErrorMsg.error("CFG2Cluster",msg)

    val dummyNode = F.LABEL(Label.Label{id= ~1, addr=ref ~1, name=""})

    fun pseudo_op (CFG.LABEL l) = F.LABEL l
      | pseudo_op (CFG.PSEUDO p) = F.PSEUDO p

        (* create a new BBLOCK with id i *)
    fun bblock M (i,b as 
                 CFG.BLOCK{kind,freq,annotations,insns,labels,data,...}) =
    let val labels = map F.LABEL (!labels)
    in  case kind of
           CFG.STOP => map pseudo_op (!data)
        |  _ =>
        let val block = F.BBLOCK{blknum      = i,
                                 freq        = freq,
                                 annotations = ref(#rmv CFG.LIVEOUT 
                                                    (!annotations)),
                                 insns       = insns,
                                 liveIn      = ref F.C.empty,
                                 liveOut     = ref (CFG.liveOut b),
                                 pred        = ref [],
                                 succ        = ref []
                                }
       in  A.update(M,i,block); 
           map pseudo_op (!data) @ labels @ [block]
       end
    end

    fun bblock' (M,M',M'') =
    let val bblock = bblock M
    in  fn (i,b as CFG.BLOCK{id,...}) =>
           let val block = bblock(i,b) 
           in  A.update(M',i,id); A.update(M'',id,i); block end
    end

        (* create a new ENTRY with id i *)
    fun entry(M,i,freq) =
    let val entry = F.ENTRY{succ=ref [], blknum=i, freq=freq}
    in  A.update(M,i,entry); 
        entry
    end

    fun entry'(M,M',M'',i,id,freq) =
    let val entry = entry(M,i,freq)
    in  A.update(M',i,id); A.update(M'',id,i); entry
    end

        (* create a new EXIT with id i *)
    fun exit(M,i,freq) = 
    let val exit = F.EXIT{pred=ref [], blknum=i, freq=freq}
    in  A.update(M,i,exit); 
        exit
    end

    fun exit'(M,M',M'',i,id,freq) =
    let val exit = exit(M,i,freq)
    in  A.update(M',i,id); A.update(M'',id,i); exit
    end

    fun id_of(F.BBLOCK{blknum,...}) = blknum
      | id_of(F.ENTRY{blknum,...})  = blknum
      | id_of(F.EXIT{blknum,...})   = blknum

    fun remove_entry_to_exit (ENTRY,EXIT,CFG) =
        Graph.remove_edge CFG (ENTRY,EXIT)

    fun freqOf (G.GRAPH cfg) id =
        let val CFG.BLOCK{freq,...} = #node_info cfg id in freq end

       (*
        * Convert cfg -> cluster, assuming the layout is unchanged
        *)
    fun computeOldLayout (CFG as G.GRAPH cfg) =
    let val M       = #capacity cfg ()
        val ENTRY   = case #entries cfg () of
                        [ENTRY] => ENTRY
                      | _ => raise Graph.NotSingleEntry
        val EXIT    = case #exits cfg () of
                        [EXIT] => EXIT
                      | _ => raise Graph.NotSingleExit
        val CFG.INFO{annotations,...} = #graph_info cfg
        val _       = remove_entry_to_exit(ENTRY,EXIT,CFG)
        val A       = A.array(M,dummyNode)
        val nodes   = List.filter(fn (i,CFG.BLOCK{kind,...}) => 
                           i <> ENTRY andalso i <> EXIT) 
                                 (#nodes cfg ())
        val blocks  = List.concat(
                        map (bblock A) (nodes @ [(EXIT,#node_info cfg EXIT)]))
        val entry   = entry (A,ENTRY,freqOf CFG ENTRY)
        val exit    = exit (A,EXIT,freqOf CFG EXIT)
        fun succs i = map (fn (_,i,CFG.EDGE{w,...}) => (A.sub(A,i),w)) 
                            (#out_edges cfg i)
        fun preds i = map (fn (i,_,CFG.EDGE{w,...}) => (A.sub(A,i),w)) 
                            (#in_edges cfg i)
        fun set_links(F.BBLOCK{blknum,pred,succ,insns,...}) = 
                  (pred := preds blknum; succ := succs blknum)
          | set_links(F.ENTRY{blknum,succ,...}) = succ := succs blknum
          | set_links(F.EXIT{blknum,pred,...})  = pred := preds blknum
          | set_links _ = ()
        val _ = A.app set_links A
    in  F.CLUSTER{ blkCounter  = ref M,
                   blocks      = blocks,
                   entry       = entry,
                   exit        = exit,
                   annotations = annotations
                 }
    end

       (*
        * Convert cfg -> cluster, while computing a new code layout.
        *)
    fun computeNewLayout (CFG as G.GRAPH cfg) =
    let val M        = #capacity cfg ()
        val ENTRY   = case #entries cfg () of
                        [ENTRY] => ENTRY
                      | _ => raise Graph.NotSingleEntry
        val EXIT    = case #exits cfg () of
                        [EXIT] => EXIT
                      | _ => raise Graph.NotSingleExit
        val CFG.INFO{firstBlock,annotations,...} = 
               #graph_info cfg
        val A        = A.array(M,dummyNode)    (* new id -> F.block *)
        val A'       = A.array(M,~1)           (* new id -> old id *)
        val A''      = A.array(M,~1)           (* old id -> new id *)
        val min_pred = A.array(M,10000000)
        val in_degs  = A.tabulate(M,fn i => length(#in_edges cfg i))
        val nodes    = GraphTopsort.topsort CFG (ENTRY::map #1 (#nodes cfg ()))

        fun higher_freq(i,j) =
            let val CFG.BLOCK{freq=w1,...} = #node_info cfg i
                val CFG.BLOCK{freq=w2,...} = #node_info cfg j
            in  !w1 > !w2
            end

        fun older(i,j) = A.sub(min_pred,i) < A.sub(min_pred,j)

        val marked  = Set.create M
        val node_queue = Q.create (* older *) higher_freq
        val insert_node = Q.insert node_queue

        fun node b = (b,#node_info cfg b)
        
        val make_a_block = bblock' (A,A',A'')
        fun make_block(id,B as CFG.BLOCK{id=i,
                               insns=ref [],data,labels,...}) = 
              (case #in_edges cfg i of
                  [] => map pseudo_op (!data) @ map F.LABEL (!labels)
               |  _  => make_a_block(id,B) 
              )
          | make_block(id,B) = make_a_block(id,B)

        fun update_succs (id,[])      = ()
          | update_succs (id,((i,j,_)::es)) = 
            let val count = A.sub(in_degs,j) - 1
            in  A.update(min_pred,j,Int.min(id,A.sub(min_pred,j)));
                A.update(in_degs,j,count);
                if count = 0 andalso
                   j <> EXIT andalso
                   (case CFG.fallsThruFrom(CFG,j) of SOME _ => false 
                                                   | NONE => true) then
                   insert_node j
                else ();
                update_succs(id,es)
            end
         
        fun layout(id,(i,B),waiting,blocks) =
            if Set.markAndTest(marked,i) then
                 layout_all(id,waiting,blocks)
            else let val blocks = make_block(id,B)::blocks
                 in  update_succs(id,#out_edges cfg i);
                     case CFG.fallsThruTo(CFG,i) of
                        SOME j => layout(id+1,node j,waiting,blocks)
                     |  NONE   => layout_all(id+1,waiting,blocks)
                 end

        and layout_all(id,waiting,blocks) =
          if Q.isEmpty node_queue then
             layout_waiting(id,waiting,blocks) 
          else
             let val b = Q.deleteMin node_queue
             in  layout(id,node b,waiting,blocks)
             end

        and layout_waiting(id,[],blocks) = 
               (id,List.concat(rev blocks))
          | layout_waiting(id,n::waiting,blocks) =  
              case CFG.fallsThruFrom(CFG,n) of
                 SOME _ => layout_waiting(id,waiting,blocks)
              |  NONE   => layout(id,node n,waiting,blocks)

        val _ = Set.set(marked,ENTRY)
        val _ = Set.set(marked,EXIT)
        val (id,blocks) = layout_all(0,(!firstBlock)::nodes,[])
        (*val _ = print("M="^Int.toString M^ " id="^Int.toString id^"\n")*)

        val exit    = exit'(A,A',A'',id,EXIT,freqOf CFG EXIT)
        val entry   = entry'(A,A',A'',id+1,ENTRY,freqOf CFG ENTRY)
        val blocks  = blocks @ bblock A (EXIT,#node_info cfg EXIT)
        fun succs i = map (fn (_,i,CFG.EDGE{w,...}) => 
                               (A.sub(A,A.sub(A'',i)),w))
                                 (#out_edges cfg (A.sub(A',i)))
        fun preds i = map (fn (i,_,CFG.EDGE{w,...}) => 
                               (A.sub(A,A.sub(A'',i)),w)) 
                                 (#in_edges cfg (A.sub(A',i)))
        fun set_links(F.BBLOCK{blknum,pred,succ,insns,...}) = 
            let fun isBackwardBranch((F.BBLOCK{blknum=next,...},_)::bs) =
                      next <= blknum orelse isBackwardBranch bs
                  | isBackwardBranch(_::bs) = isBackwardBranch bs
                  | isBackwardBranch []     = false
            in  pred := preds blknum; 
                succ := succs blknum
            end
          | set_links(F.ENTRY{blknum,succ,...}) = succ := succs blknum
          | set_links(F.EXIT{blknum,pred,...})  = pred := preds blknum
          | set_links _ = ()
        val _ = A.app set_links A
    in  F.CLUSTER{ blkCounter  = ref(id+2),
                   blocks      = blocks,
                   entry       = entry,
                   exit        = exit,
                   annotations = annotations
                 }
    end

    fun cfg2cluster {cfg=CFG as G.GRAPH cfg,relayout} =
    let val CFG.INFO{reorder,...} = #graph_info cfg
    in  if !reorder orelse relayout then computeNewLayout CFG
        else computeOldLayout CFG
    end

end

