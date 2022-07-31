(*
 * This module builds the data dependence graph for acyclic scheduling.
 *
 * Notes:
 * 1. Special source and sink nodes are added to each basic block.
 *    These nodes anchor live-in and live-out values.
 * 2. If a block has a branch, then it is control dependent on the live-in 
 *    node.
 *
 * -- Allen
 *)

functor SchedulerDDGBuilder
     (structure DDG : SCHEDULER_DDG
      structure CFG : CONTROL_FLOW_GRAPH
      structure RTLProps  : RTL_PROPERTIES
      structure InsnProps : INSN_PROPERTIES
         sharing InsnProps.I = RTLProps.I = DDG.I = CFG.I
     ) : SCHEDULER_DDG_BUILDER =
struct

   structure DDG        = DDG
   structure CFG        = CFG
   structure RTL        = RTLProps.RTL
   structure G          = Graph
   structure I          = CFG.I
   structure C          = I.C
   structure SchedProps = DDG.SchedProps 
   structure HA         = HashArray
   structure A          = Array
   structure W8A        = Word8Array
   structure SL         = SortedList

   exception BuildDDG

   fun error msg = MLRiscErrorMsg.error("BuildDDG",msg)

   val i2s = Int.toString

   (* Zero register magic! *)
   val zeroTbl = W8A.array(C.firstPseudo, 0w0)
   val _       = List.app (fn k =>
                    case C.zeroReg k of
                      SOME r => W8A.update(zeroTbl, r, 0w1)
                    | NONE   => ()
                 ) C.cellkinds
   fun isZero r = W8A.sub(zeroTbl, r) <> 0w0 handle _ => false

   exception Nothing

   fun buildDDG{cpu_info, cfg, numberOfInstructions, blockIdTbl} = 
   let val CFG as G.GRAPH cfg = cfg
       (* The number of nodes <= instructions + livein + liveout per block *)
       val M = numberOfInstructions + #order cfg () * 2
       val DDG as G.GRAPH ddg = DDG.newDDG M
       val globalInfo = DDG.globalInfo DDG

       (* Extract instruction properties *)
       val SchedProps.CPU_INFO{defUse, ...} = cpu_info

       (* Regmap magic! *)
       val regmap = C.lookup(CFG.regmap CFG)
       val regmapDefs = map (fn (r,l) => (regmap r,l))
       val regmapUses = map regmap
       fun simplifyCopy(instr, dst, src) = 
       let fun loop([], [], dst', src') = (dst', src')
             | loop((d,l)::dst, s::src, dst', src') = 
               let val d = regmap d and s = regmap s
               in  if d = s then loop(dst, src, dst', src')
                   else loop(dst, src, (d,l)::dst', s::src')
               end
             | loop _ = error "simplifyCopy"
           val (dst, src) = loop(dst, src, [], []) 

           (* add the copy temporary! *)
           val dst = case dst of
                       [] => dst
                     | _  => case InsnProps.moveTmpR instr of
                               SOME r => (regmap r,~1)::dst
                             | _      => dst
       in  (dst, src)
       end

       (* Edge constructors *)  
       (* memory *)
       fun m_flow(m,l) = DDG.EDGE{l=l, r=m, d=DDG.MEM_FLOW}
       val m_anti      = DDG.EDGE{l= ~1, r= ~1, d=DDG.MEM_ANTI}
       val m_output    = DDG.EDGE{l=0, r= ~1, d=DDG.MEM_OUTPUT}
       (* register *)
       fun flow(r,l)   = DDG.EDGE{l=l, r=r, d=DDG.FLOW}
       val output      = DDG.EDGE{l=0, r= ~1, d=DDG.OUTPUT}
       val anti        = DDG.EDGE{l= ~1, r= ~1, d=DDG.ANTI}
       (* control dependence *)
       fun c_flow(r,l) = DDG.EDGE{l=l, r=r, d=DDG.CTRL}
       val c_dep       = DDG.EDGE{l= ~1, r= ~1, d=DDG.CTRL}
       val c_output    = DDG.EDGE{l=0, r= ~1, d=DDG.CTRL}
       val c_anti      = DDG.EDGE{l= ~1, r= ~1, d=DDG.CTRL_ANTI}

       (* How to make a new edge *)
       val newEdge = #add_edge ddg 
       (* val newEdge = fn (i,j,e) => (print(i2s i^"->"^i2s j^" "^DDG.edgeToString e^"\n"); newEdge(i,j,e)) handle e => raise e *)

       (* A table of definitions and uses indexed by block *)
       val defUseTbl = HA.array'(13, fn _ => raise BuildDDG)

       (* Create nodes for block b *)
       fun createNodes(id, b,  [], ops) = (id, ops)
         | createNodes(id, b, instr::instrs, ops) = 
           let val (d, u) = defUse instr
               fun newNode(defs, uses) =  
               let val node = DDG.NODE{b=b,instr=instr,defs=defs,uses=uses}
               in  #add_node ddg (id, node);
                   createNodes(id+1, b, instrs, (id, node)::ops)
               end
           in  case InsnProps.instrKind instr of
                 InsnProps.IK_COPY => 
                  (case simplifyCopy(instr, d, u) of
                    ([], []) => createNodes(id, b, instrs, ops)
                  | (d,  u)  => newNode(d, u)
                  )
               | _ => newNode(regmapDefs d, regmapUses u)
           end

       (* Scan one block; ops are in forward order *)
       fun scanBlock{ops,liveIn=(liveIn,_),defTbl,useTbl} =
       let fun addOutputAndAnti j (r,_) =  
               (app (fn i => newEdge(i,j,anti)) (HA.sub(useTbl,r));
                app (fn (i,e) => newEdge(i,j,output)) (HA.sub(defTbl,r))
               )
           
           fun addFlow j r =
               app (fn (i,e) => newEdge(i,j,e)) (HA.sub(defTbl,r))

           (* Update def/use *)
           fun addDef i (r,l) = 
               if isZero r then () 
               else
                 (HA.update(defTbl,r,[(i,flow(r,l))]); HA.update(useTbl,r,[]))

           fun addUse i r = 
               if isZero r then ()
               else HA.update(useTbl,r,i::HA.sub(useTbl,r))

           fun scan [] = ()
             | scan((i,DDG.NODE{instr, defs, uses,...})::rest) =
               let val rtl = RTLProps.rtl instr
               in  if RTL.can'tMoveUp rtl then
                      newEdge(liveIn, i, c_dep)
                   else ();
                   app (addOutputAndAnti i) defs;
                   app (addFlow i) uses;
                   (* update defs/uses *)
                   app (addUse i) uses;
                   app (addDef i) defs;
                   scan rest
               end 
       in  scan ops
       end


       val blockId    = ref 0 
       val nodeId     = ref 0 
       val blockMap   = A.array(#order cfg (), 0) 
       val liveInMap  = IntHashTable.mkTable(13, Nothing)
       val liveOutMap = IntHashTable.mkTable(13, Nothing)
       val specialMap = IntHashTable.mkTable(32, Nothing)
       val addSpecial = IntHashTable.insert specialMap
       val isSpecial  = IntHashTable.find specialMap
       val isSpecial  = fn b => case isSpecial b of SOME _ => true 
                                                  | NONE => false

       (* Process a basic block in topological order of the region:
        *  1. create all the nodes in the DDG
        *  2. add the edges 
        *)  
       fun processBlock(b,b' as CFG.BLOCK{insns,...}) = 
       let val bid = !blockId (* block id *)
           val _   = A.update(blockMap, bid, b) 
           val _   = A.update(blockIdTbl, b, bid)
           val _   = blockId := bid + 1

           fun createNode(instr, defs, uses) =  
           let val node = (!nodeId, 
                           DDG.NODE{instr=instr,b=bid,defs=defs,uses=uses})
           in  nodeId := !nodeId + 1;
               #add_node ddg node;
               node
           end 

           (* Create the nodes *)
           val (newNodeId, ops) = createNodes(!nodeId, bid, !insns, []) 
           val _ = nodeId := newNodeId

           val revAppend = List.revAppend

           val defs = HA.array(13, [])  
           val uses = HA.array(13, [])    

              (* edge Y->X is an internal region edge 
               * merge definition and uses from Y => X
               *)
           fun mergeDefUse(Y,X,_) = 
               let val {defTbl, useTbl} = HA.sub(defUseTbl, Y)
               in  HA.appi (fn (r,es) => 
                           HA.update(defs, r, revAppend(es, HA.sub(defs, r))))
                       (defTbl, 0, NONE);
                   HA.appi (fn (r,is) =>
                           HA.update(uses, r, revAppend(is, HA.sub(uses, r))))
                          (useTbl, 0, NONE)
               end

           fun addCtrlDepEdge(i, j) = newEdge(i,j,c_dep)

              (* Add a live-in node for a block that summarizes the
               * values that are coming live-in from side-exits
               *)
           fun addLiveIn X = 
               let val entry_edges = #entry_edges cfg X 
                   val liveIn = 
                        SL.uniq(foldr (fn ((Y,_,_),S) =>
                        let val CFG.BLOCK{annotations,...} = #node_info cfg Y
                        in  case #get DDG.LIVENESS (!annotations) of
                              SOME{liveOut, ...} => revAppend(liveOut,S)
                            | NONE => S
                        end) [] entry_edges)
                   val liveInNode as (i,_) = 
                          createNode(SchedProps.source, 
                             map (fn r => (r,~1)) liveIn, [])
                   val _ = IntHashTable.insert liveInMap (bid, liveInNode)
                   val _ = addSpecial(i, true)
                   fun addOutputAndAnti j r =  
                        (app (fn i => if isSpecial j then () 
                                      else newEdge(i,j,anti)) (HA.sub(uses,r));
                         app (fn (i,e) => 
                               if isSpecial i then ()
                               else newEdge(i,j,output)) (HA.sub(defs,r))
                        )
               in  app (addOutputAndAnti i) liveIn;
                   app (fn r => HA.update(defs, r, 
                        (i,DDG.EDGE{l= ~1,r=r,d=DDG.LIVEIN})::HA.sub(defs, r))) 
                      liveIn;
                   liveInNode
               end           

           val _ = app mergeDefUse (#in_edges cfg b)
           val liveInNode = addLiveIn b

              (* Add a live-out node for a block that summarizes the
               * values that are going live-out from side-exits
               *)
           fun addLiveOut X = 
               (case #exit_edges cfg X of
                 exit_edges =>
                 let fun createLiveOutNode(liveOut) =
                     let val node as (i, _) = 
                            createNode(SchedProps.sink, [], liveOut)
                     in  IntHashTable.insert liveOutMap (bid, node);
                         addSpecial(i, true);
                         node
                     end
                     val liveOut = 
                        if List.exists 
                             (fn (_,_,CFG.EDGE{k,...}) => k = CFG.EXIT) 
                                exit_edges 
                        then
                          let val CFG.BLOCK{annotations,...} = #node_info cfg X
                          in  case #get DDG.LIVENESS (!annotations) of
                                SOME{liveOut, ...} => liveOut
                              | NONE => error "missing live out"
                          end
                        else
                           SL.uniq(foldr (fn ((_,Y,_),S) =>
                           let val CFG.BLOCK{annotations,...} = #node_info cfg Y
                           in  case #get DDG.LIVENESS (!annotations) of
                                 SOME{liveIn, ...} => revAppend(liveIn,S)
                               | NONE => S
                           end) [] exit_edges)

                     val liveOutNode as (i,_) = 
                          case !insns of
                             [] => createLiveOutNode(liveOut)
                           | jmp::_ =>
                             case InsnProps.instrKind jmp of
                               InsnProps.IK_JUMP => 
                               (* add a control dependence edge to the liveIn *)
                               let val jmpNode as (j,_) = List.last ops
                               in  addCtrlDepEdge(#1 liveInNode, j);
                                   jmpNode
                               end
                             | _ => createLiveOutNode(liveOut)
                     fun addUse i r = 
                         if isZero r then ()
                         else HA.update(uses,r,i::HA.sub(uses,r))
                     fun addLiveOut j r =
                         app (fn (i,DDG.EDGE{l,r,...}) => 
                            newEdge(i,j,DDG.EDGE{l=l,r=r,d=DDG.LIVEOUT}))
                               (HA.sub(defs,r))

                 in  app (addLiveOut i) liveOut;
                     addLiveOutCtrlDep i; 
                     app (addUse i) liveOut
                 end
               ) 

              (* Add control dependences edges from all the instructions 
               * to the live-out node
               *)
           and addLiveOutCtrlDep(j) =
               app (fn node as (i,_) => 
                    if i = j then () else addCtrlDepEdge(i,j)
                   ) ops

           val _ = scanBlock{ops=ops, liveIn=liveInNode, 
                             defTbl=defs, useTbl=uses}; 

       in  addLiveOut b;
           HA.update(defUseTbl, b, {defTbl=defs, useTbl=uses})
       end

       (* Build the entire dag *)
       fun buildDag() =
       let val allNodes = #nodes cfg () (* must be in topological order! *)
       in  app processBlock allNodes 
       end

   in  buildDag();
       globalInfo := 
          SOME{blockMap=blockMap, liveInMap=liveInMap, liveOutMap=liveOutMap};
       DDG
   end 
end
