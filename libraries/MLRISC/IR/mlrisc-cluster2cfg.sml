(*
 *  Convert the old cluster format into the new control flow graph format
 *
 * -- Allen
 *)

signature CLUSTER2CFG =
sig
   structure CFG : CONTROL_FLOW_GRAPH
   structure F   : FLOWGRAPH
      sharing CFG.I = F.I
      sharing CFG.P = F.P

   val cluster2cfg : F.cluster -> CFG.cfg

end 

functor Cluster2CFG
   (structure CFG       : CONTROL_FLOW_GRAPH 
    structure Util      : CFG_UTIL
    structure Flowgraph : FLOWGRAPH
    structure InsnProps : INSN_PROPERTIES
       sharing Util.CFG = CFG
       sharing CFG.I = Flowgraph.I = InsnProps.I
       sharing CFG.P = Flowgraph.P
   ) : CLUSTER2CFG =
struct

    structure CFG  = CFG
    structure I    = CFG.I
    structure F    = Flowgraph
    structure G    = Graph
    structure W    = CFG.W

    fun error msg = MLRiscErrorMsg.error("Cluster2CFG",msg)

    fun cluster2cfg
         (F.CLUSTER{blocks, entry, exit, blkCounter, annotations,...})=
    let fun id(F.BBLOCK{blknum, ...}) = blknum
          | id(F.ENTRY {blknum, ...}) = blknum
          | id(F.EXIT {blknum, ...})  = blknum
          | id _                      = error "id"

        fun idfreq(blk,freq) = (id blk,freq)

        fun first_block (F.BBLOCK{blknum,...}::_) = blknum
          | first_block (_::bs)                   = first_block bs
          | first_block []                        = error "first_block"

        val reorder = ref false
        val info = CFG.INFO { firstBlock   = ref(first_block blocks),
                              reorder      = reorder,
                              annotations  = annotations
                             }

        val CFG as G.GRAPH cfg = CFG.cfg(info)

        val F.ENTRY{ blknum = ENTRY, ... } = entry
        val F.EXIT{ blknum = EXIT, ... }   = exit

            (* Add a list of blocks into the CFG *) 
        fun add(F.ENTRY e::rest,Ps,Ls)     = add_entry(e,Ps,Ls,rest)
          | add(F.EXIT e::rest,Ps,Ls)      = add_exit(e,Ps,Ls,rest)
          | add(F.BBLOCK b::rest,Ps,Ls)    = add_block(b,rev Ps,Ls,rest)
          | add((F.PSEUDO p)::rest,Ps,Ls)  = 
              add(rest,CFG.PSEUDO p::map CFG.LABEL Ls@Ps,[])
          | add((F.LABEL l)::rest,Ps,Ls)   = add(rest,Ps,l::Ls)
          | add([],Ps,Ls)                  = finish(Ps,Ls)

            (* Insert an entry node *)
        and add_entry({blknum, succ, freq}, [], [], rest) =
              ( #add_node cfg (blknum,CFG.newStart(blknum,freq));
                #set_entries cfg [blknum];
                app (fn (blk,w) => add_edge(blknum, id blk, CFG.JUMP, w)) 
                     (!succ);
                add(rest, [], [])
              )
          | add_entry _ = error "add_entry"

            (* Insert an exit node *)
        and add_exit({blknum, pred, freq}, [], [], rest) = 
              ( #add_node cfg (blknum, CFG.newStop(blknum,freq));
                #set_exits cfg [blknum];
                add(rest, [], [])
              )
  
            (* Insert an normal basic block *)
       and add_block({blknum,annotations,
                      freq,liveIn,liveOut,succ,pred,insns},
                     Ps,Ls,rest) =
           let val an = !annotations
               val an = 
                   case !succ of
                     ([(F.EXIT _,_)] | []) => #create CFG.LIVEOUT(!liveOut)::an
                   | _ => an
               val bb = CFG.BLOCK{id    = blknum,
                                  kind  = CFG.NORMAL,
                                  labels= ref Ls,
                                  freq  = freq,
                                  data  = ref Ps,
                                  insns = insns,
                                  annotations=ref an
                                 }
           in  #add_node cfg (blknum,bb);
               add_edges(blknum, !succ, !insns);
               (*
               add_call_edges(blknum, !callSucc);
               add_return_edge(blknum, !returnSucc);
                *)
               add(rest, [], [])
           end

            (* Finished insertion *)
       and finish([],[]) = ()
         | finish(Ps,[]) = 
               let val CFG.BLOCK{data,labels,...} = #node_info cfg EXIT
               in  data := Ps @ !data
               end
         | finish _ = error "finish"

            (* Insert one edge into the flowgraph *)
       and add_edge(i,j,k,w) =
           let val k = if ENTRY = i then CFG.ENTRY
                       else if EXIT = j then CFG.EXIT
                       else k
           in  #add_edge cfg (i,j,CFG.EDGE{k=k,w=w,a=ref []})
           end

            (* Add edges into the flowgraph *)
       and add_edges (i, succs, insns) = 
           let fun is_fallsthru (j,yes,no) =
                   if j = i + 1 then
                      (case insns of
                         jmp::_ => (case InsnProps.instrKind jmp of
                                      InsnProps.IK_JUMP => no
                                    | _                 => yes
                                   )
                      |  _ => yes)
                   else no
               fun add_branch(i,(j,jw),(k,kw)) =
               let val j = id j and k = id k
               in  if j = i + 1 then
                     ( add_edge(i,j,CFG.BRANCH false,jw);
                       add_edge(i,k,CFG.BRANCH true,kw))
                   else if k = i + 1 then
                     ( add_edge(i,k,CFG.BRANCH false,kw);
                       add_edge(i,j,CFG.BRANCH true,jw))
                   else error "add_branch"
               end
               fun add_switch(i,_,[]) = ()
                 | add_switch(i,k,(j,jw)::js) =
                   (add_edge(i,id j,CFG.SWITCH k,jw); add_switch(i,k+1,js))
           in  case succs of
                 []      => ()
               | [(j,w)] => 
                   let val j = id j
                   in  add_edge(i,j,is_fallsthru(j,CFG.FALLSTHRU,CFG.JUMP),w)
                   end
               | [j,k] => add_branch(i,j,k)
               | js    => add_switch(i,0,js)
           end

           fun check_for_bad_entries() =
               app (fn (i,j,e) =>
                     if j = EXIT then ()
                     else
                     case #in_edges cfg j of 
                        [_] => () (* only edge from ENTRY, okay *)
                     |  _ => 
                        error("entry "^Int.toString j^" has internal edges") 
                   ) (#out_edges cfg ENTRY)

           (* add edge from entry to exit *)
           fun insert_entry_to_exit () = 
               if #has_edge cfg (ENTRY,EXIT) then ()
               else add_edge (ENTRY,EXIT,CFG.JUMP,ref 0)
    in 
        add(entry::exit::blocks,[],[]);
        check_for_bad_entries();
        insert_entry_to_exit(); 
        CFG
    end

end

