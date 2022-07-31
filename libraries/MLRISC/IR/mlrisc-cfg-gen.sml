(*
 * This module takes a stream of instructions and build a CFG.
 * The building can be incremental.
 *
 * -- Allen
 *)
functor ControlFlowGraphGen
   (structure CFG     : CONTROL_FLOW_GRAPH
    structure Stream  : INSTRUCTION_STREAM
    structure InsnProps : INSN_PROPERTIES
        sharing CFG.I = InsnProps.I
        sharing CFG.P = Stream.P
   ) : CONTROL_FLOW_GRAPH_GEN =
struct

   structure CFG     = CFG
   structure I       = CFG.I
   structure P       = CFG.P
   structure G       = Graph
   structure W       = CFG.W
   structure S       = Stream

   fun error msg = MLRiscErrorMsg.error("ControlFlowGraphGen",msg)

   fun builder(CFG) = 
   let val NOBLOCK      = CFG.newBlock(~1,ref 0)
       val currentBlock = ref NOBLOCK 
       val newBlocks    = ref [] : CFG.block list ref 
       val blockNames   = ref [] : Annotations.annotations ref
       val entryLabels  = ref [] : Label.label list ref
       fun can'tUse _   = error "unimplemented"
       exception NotFound
       val labelMap = IntHashTable.mkTable(32,NotFound)
       val newLabel = IntHashTable.insert labelMap
       val lookupLabel = IntHashTable.lookup labelMap
       val CFG    = ref CFG

       (* Initialization *)
       fun init _ =
       let val G.GRAPH cfg = !CFG
       in  IntHashTable.clear labelMap;
           #forall_nodes cfg 
             (fn (blockId,CFG.BLOCK{labels, ...}) =>
                  app (fn Label.Label{id, ...} => newLabel(id,blockId))
                      (!labels));
           currentBlock := NOBLOCK;
           newBlocks := [];
           blockNames := [];
           entryLabels := []
       end   

       val _ = init()

       fun next cfg = CFG := cfg

       fun newBlock() = 
       let val G.GRAPH cfg = !CFG
           val id = #new_id cfg ()
           val b as CFG.BLOCK{annotations,...} = CFG.newBlock(id,ref 0)
       in  currentBlock := b; 
           annotations := !blockNames;
           newBlocks := b :: !newBlocks;
           #add_node cfg (id,b);
           b 
       end

       fun getBlock() = 
           case !currentBlock of
              CFG.BLOCK{id= ~1,...} => newBlock()
           |  b => b

       fun newPseudoOpBlock() =
            (case !currentBlock of
                CFG.BLOCK{id= ~1,...} => newBlock()
             |  b as CFG.BLOCK{insns=ref [],...} => b
             |  _ => newBlock()
            )  

       fun insertOp p = 
       let val CFG.BLOCK{data,...} = newPseudoOpBlock()
       in  data := !data @ [p] end

       (* Add a new label *)
       fun defineLabel(l as Label.Label{id=labelId,...}) = 
       let val id = lookupLabel labelId
           val G.GRAPH cfg = !CFG
           val blk as CFG.BLOCK{insns, ...} = #node_info cfg id
       in  currentBlock := blk;
           newBlocks := blk :: !newBlocks;
           insns := []; (* clear instructions *)
           #set_out_edges cfg (id,[]) (* clear edges *)
       end handle _ =>
       let val CFG.BLOCK{id,labels,...} = newPseudoOpBlock()
       in  labels := l :: !labels;
           newLabel(labelId, id)
       end

       (* Add a new entry label *)
       fun entryLabel l = (defineLabel l; entryLabels := l :: !entryLabels)

       (* Add a new pseudo op *)
       fun pseudoOp p = insertOp(CFG.PSEUDO p)

       fun nextBlock() =
           case !currentBlock of
              CFG.BLOCK{id= ~1,...} => ()
           |  b => currentBlock := NOBLOCK

       (* Add a new annotation *)
       fun annotation a = 
           case a of
             MLRiscAnnotations.BLOCKNAMES names =>
                (blockNames := names;
                 nextBlock()
                )
           | MLRiscAnnotations.EMPTYBLOCK => nextBlock()
           | a => 
              let val CFG.BLOCK{annotations,...} = getBlock()
              in  annotations := a :: !annotations
              end

       (* Mark current block as exit *)
       fun exitBlock liveOut = 
       let fun setLiveOut(CFG.BLOCK{annotations,...}) = 
                 annotations := #create CFG.LIVEOUT liveOut :: !annotations
       in  case !currentBlock of
              CFG.BLOCK{id= ~1,...} => 
                (case !newBlocks of
                   [] => error "exitBlock"
                 | b::_ => setLiveOut b
                )
            | b => setLiveOut b
       end

       (* Add a new comment *)
       fun comment msg = annotation(#create MLRiscAnnotations.COMMENT msg)

       (* Emit an instruction *)
       fun emit i =
       let val CFG.BLOCK{insns,...} = getBlock()
       in  insns := i :: !insns;
           case InsnProps.instrKind i of
             (InsnProps.IK_JUMP | InsnProps.IK_CALL_WITH_CUTS) =>
                currentBlock := NOBLOCK
           | _ => () 
       end

       (* End current cluster *)
       fun endCluster(annotations) =
       let val G.GRAPH cfg = !CFG
           val _ = CFG.init(!CFG) (* create entry/exit *)

           val ENTRY = hd(#entries cfg ())
           val EXIT = hd(#exits cfg ())

           fun next(CFG.BLOCK{id,data=ref [],...}::_) = id
             | next _ = error "next"

           val lookupLabelMap = IntHashTable.find labelMap
           val lookupLabelMap = 
                fn l => case lookupLabelMap l of SOME b => b | NONE => EXIT 
           val TRUE = CFG.BRANCH true
           val FALSE = CFG.BRANCH false
           val addEdge = #add_edge cfg

           fun target(Label.Label{id,...}) = lookupLabelMap id

           fun addEdges [] = ()
             | addEdges(CFG.BLOCK{id,insns,...}::blocks) =
               (case !insns of
                  [] => fallsThru(id,blocks)
                | instr::_ =>
                   (case InsnProps.instrKind instr of 
                      (InsnProps.IK_JUMP | InsnProps.IK_CALL_WITH_CUTS) =>
                        jump(id,InsnProps.branchTargets instr,blocks)
                   | _ => fallsThru(id,blocks)
                   );
                addEdges blocks
               )
           and fallsThru(i,CFG.BLOCK{id=j,data,...}::_) =
                 (case !data of
                     [] => ()
                  |  _  => error("falls thru into pseudo ops: "^
                                 Int.toString i^" -> "^Int.toString j)
                  ;
                  addEdge(i,j,CFG.EDGE{k=CFG.FALLSTHRU,w=ref 0, a=ref []})
                 )
             | fallsThru(i,[]) =
                  (* error("missing return in block "^Int.toString i) *)
                  addEdge(i,EXIT,CFG.EDGE{k=CFG.EXIT,w=ref 0,a=ref []})
           and jump(i,[InsnProps.ESCAPES],_) =
                  addEdge(i,EXIT,CFG.EDGE{k=CFG.EXIT,w=ref 0,a=ref []})
             | jump(i,[InsnProps.LABELLED L],_) =
                  addEdge(i,target L,CFG.EDGE{k=CFG.JUMP,w=ref 0,a=ref []})
             | jump(i,[InsnProps.LABELLED L,InsnProps.FALLTHROUGH],bs) =
                  (addEdge(i,target L,CFG.EDGE{k=TRUE,w=ref 0,a=ref[]});
                   addEdge(i,next bs,CFG.EDGE{k=FALSE,w=ref 0,a=ref []})
                  )
             | jump(i,[InsnProps.FALLTHROUGH,InsnProps.LABELLED L],bs) =
                  (addEdge(i,target L,CFG.EDGE{k=TRUE,w=ref 0,a=ref []});
                   addEdge(i,next bs,CFG.EDGE{k=FALSE,w=ref 0,a=ref []})
                  )
             | jump(i,targets,_) =
               let fun loop(n,[]) = ()
                     | loop(n,InsnProps.LABELLED L::targets) =
                        (addEdge(i,target L, 
                           CFG.EDGE{k=CFG.SWITCH n,w=ref 0,a=ref []});
                       loop(n+1,targets))
                     | loop _ = error "jump"
               in  loop(0,targets) end
          in  addEdges(rev(!newBlocks));
              app (fn l => addEdge(ENTRY,target l,
                              CFG.EDGE{k=CFG.ENTRY,a=ref [],w=ref 0})) 
                     (!entryLabels);
              let val an = CFG.annotations(!CFG);
              in  an := annotations @ (!an) end;
              init()
          end

       (* Start a new cluster *)
       fun beginCluster _ = init()
  
       fun getAnnotations() = CFG.annotations(!CFG)

    in  {stream=S.STREAM
           {  beginCluster  = beginCluster,
              endCluster    = endCluster,
              defineLabel   = defineLabel,
              entryLabel    = entryLabel,
              pseudoOp      = pseudoOp,
              emit          = emit,
              exitBlock     = exitBlock,
              comment       = comment,
              annotation    = annotation,
              getAnnotations= getAnnotations
           },
         next = next
        }
    end  

end
