(** Graph coloring register allocation. graph
 ** Implements the 'iterated register coalescing' scheme described 
 ** in POPL'96, and TOPLAS v18 #3, pp 325-353. 
 **)
(*
 * This is a reorganization of the old iterated coalescing 
 * register allocator using a more modular implementation.
 * 
 * --- Allen
 *)
functor OldRegAllocator
    (structure RaArch : RA_ARCH_PARAMS)
    (structure RaUser : RA_USER_PARAMS
       where I = RaArch.I
       where B = RaArch.Liveness.F.B
    ) : RA =
struct

   structure F = RaArch.Liveness.F
   structure Core = OldRACore
   structure G = Core.G
   structure A = Array
   structure I = RaArch.I
   structure C = I.C
   structure P = RaArch.InsnProps
   structure SL = SortedList

   datatype mode = REGISTER_ALLOCATION | COPY_PROPAGATION

   open G

   fun error msg = MLRiscErrorMsg.error("IteratedCoalescing",msg)

   (*
    * Debugging flags
    *)
   val cfg_before_ra   = MLRiscControl.getFlag "dump-cfg-before-ra"
   val cfg_after_ra    = MLRiscControl.getFlag "dump-cfg-after-ra"
   val cfg_after_spill = MLRiscControl.getFlag "dump-cfg-after-spilling"
   val dump_graph      = MLRiscControl.getFlag "dump-interference-graph"
   val ra_count        = MLRiscControl.getCounter "ra-count"
   val rewrite         = MLRiscControl.getCounter "ra-rewrites"

   (*  
    * Set of dedicated registers.
    * Note: I'm using an array for testing for dedicated registers.
    * Hopefully this is now a bit faster than before. -- Allen
    *)
   val spillRegSentinel = ~1 (* what is this? *)
   val dedicated     = SL.uniq(RaUser.dedicated)
   val firstPseudoR  = RaArch.firstPseudoR
   val dedicatedRegs = A.array(firstPseudoR,false)
   val _  = app (fn r => if r >= 0 andalso r < firstPseudoR then
                         A.update(dedicatedRegs,r,true) else ()) dedicated
   fun isDedicated r = r < 0 orelse r < firstPseudoR andalso 
                                    Unsafe.Array.sub(dedicatedRegs,r) 
   (* Note: The following is no long necessary!
    * Note: This function maintains the order of members in rset
    * which is important when dealing with parallel copies.
    *)
   fun rmvDedicated rset = 
   let fun loop(x::xs, xs') = loop(xs, if isDedicated x then xs' else x::xs')
         | loop([], xs') = xs'
   in  loop(rset,[])
   end


   (* register mapping functions *)
   fun uniqMap(f, l) = SL.uniq(map f l)

   fun prList (l:int list,msg:string) = 
   let fun pr [] = print "\n"
         | pr (x::xs) = (print (Int.toString x ^ " "); pr xs)
   in  print msg; pr l
   end

   (* debugging *)
   fun printBlocks(blks, regmap, annotations) = 
   let val regmap = C.lookup regmap
       val RaArch.AsmEmitter.S.STREAM{emit,...} =
              AsmStream.withStream TextIO.stdOut 
                 RaArch.AsmEmitter.makeStream annotations
        val emit = emit regmap
        fun prBlks([]) = print"\n"
          | prBlks(F.BBLOCK{blknum,insns,liveOut,liveIn,
                            succ,pred,...}::blocks)=
            let
              fun regset cellset = map regmap (RaArch.regSet(cellset))
              fun pr [] = prList(regset(!liveOut), "liveOut: ")
                | pr (instr::rest) = (emit instr; pr rest)
              fun blkNum(F.BBLOCK{blknum, ...},_) = blknum
                | blkNum(F.ENTRY{blknum, ...},_) = blknum
                | blkNum(F.EXIT{blknum, ...},_) = blknum
                | blkNum _ = error "printBlocks.prBlks.blkNum"
            in
              print("BLOCK" ^ Int.toString blknum ^ "\n");
              prList(regset (!liveIn), "LiveIn :");
              prList(map blkNum (!pred),"predecessors: ");
              case !insns of [] => print "empty instruction sequence\n"
                           |  l  => pr(rev l)
              (*esac*);
              prList(map blkNum (!succ),"successors: ");
              prBlks(blocks)
            end
          | prBlks(F.LABEL lab::blocks) =
              (print(Label.nameOf lab^":\n"); prBlks(blocks))
          | prBlks(F.PSEUDO pOp::blocks) = (print (F.P.toString pOp); prBlks(blocks))
          | prBlks(_::blocks) = prBlks(blocks)
   in prBlks blks
   end

   fun debug(flag, msg, blocks, regmap, annotations) =
   if !flag then
      (print ("------------------" ^ msg ^ " ----------------\n");
       printBlocks(blocks,regmap,annotations))
   else ()


   (* Utility functions *)
   fun newNode(num, col) =
       NODE{number=num,
            color=ref col,
            degree=ref 0,
            adj=ref [],
            movecnt = ref 0,
            movelist = ref []}

   fun nodeNumber(NODE{number, ...}) = number

   fun chase(NODE{color=ref(ALIASED r), ...}) = chase r
     | chase x = x

   fun nodeMember(_, []) = false
     | nodeMember(node as NODE{number=x, ...}, NODE{number=y,...}::rest) =
        x = y orelse nodeMember(node, rest)


   fun isMoveRelated(NODE{movecnt=ref 0, ...}) = false
     | isMoveRelated _ = true

   exception PrevSpills 
   val prevSpills      = Intmap.new(32,PrevSpills) : bool Intmap.intmap
   val isSpilled       = Intmap.mapWithDefault(prevSpills,false)
   val enterSpilled    = Intmap.add prevSpills 
   fun markAsSpilled r = enterSpilled(r,true)

   (* 
    * This is the new register allocator!
    *)
   fun ra mode prohibit 
       (cluster as F.CLUSTER{regmap,blocks,annotations=ref an,...}) =
   if RaArch.numRegs() = 0 then cluster
   else
   let
       val _ = Intmap.clear prevSpills
       val _ = app (fn (i,j) => 
                    let fun loop(i) = 
                            if i <= j then (markAsSpilled i; loop(i+1)) else ()
                    in  loop i end) prohibit

          (* number of blocks *)
       val numBlocks = foldr (fn (F.BBLOCK _,n) => n + 1 | (_,n) => n) 0 blocks

       val blockDU = A.array(numBlocks,[] : (node list * node list) list)
       val cblocks = A.array(numBlocks,F.LABEL(Label.newLabel ""))
       val numOfBlocks = A.length cblocks

           (* remainInfo: blocks where spill nodes are defined and used. *)
       type info  = int list Intmap.intmap
       val remainInfo : (info * info) option ref   = ref NONE

       fun cleanupSpillInfo() = remainInfo := NONE

       (** 
        ** Build blockDU and cblocks.
        ** This is done once per RA.
        **)
       fun initialize() = 
       let val nodes = Intmap.new(32,Nodes)
           fun mkNode i =
                newNode(i, if i < firstPseudoR then COLORED(i) else PSEUDO)
           val lookupNodes = Intmap.map nodes
           val enterNodes = Intmap.add nodes
           fun newnode n =
                 lookupNodes n
                 handle _ =>
                   let val node = mkNode n
                   in  enterNodes (n, node); node
                   end
           fun blockDefUse((b as F.BBLOCK{insns,liveOut,succ, ...})::blks, n) = 
               let fun insnDefUse insn = 
                   let val (d,u) = RaArch.defUse insn
                       fun rmv [] = []
                         | rmv (l as [x]) =
                             if isDedicated x then [] else [newnode x]
                         | rmv set = map newnode (rmvDedicated set)
                   in (rmv d, rmv u) end
               in  Unsafe.Array.update(cblocks, n, b);
                   Unsafe.Array.update(blockDU, n, map insnDefUse (!insns));
                   case !succ of 
                      [(F.EXIT _,_)] =>
                         app (fn i => (newnode i; ()))
                             (rmvDedicated(RaArch.regSet(!liveOut)))
                     | _  => ();
                   blockDefUse(blks, n+1)
               end
             | blockDefUse(_::blks, n) = blockDefUse(blks, n)
             | blockDefUse([], _) = ()

            (* if copy propagation was done prior to register allocation
             * then some nodes may already be aliased. 
             *)
           fun updateAliases() = 
           let val alias = Intmap.mapInt regmap
               fun fixup(num, NODE{color, ...}) =
                   if num < firstPseudoR then ()
                   else let val reg = alias num
                        in  if reg=num then () else 
                            color := ALIASED(newnode reg)
                        end 
           in  Intmap.app fixup nodes end
       in  blockDefUse(blocks,0);
           updateAliases();
           nodes
       end

       (** 
        ** Run liveness analysis 
        **)
       fun liveness(nodes,blocks) = 
       let val getnode = Intmap.map nodes
           fun regmap i = 
           let val node = getnode i
           in  case node
                of NODE{color= ref (COLORED r), ...} => r
                 | NODE{color=ref PSEUDO, ...} => nodeNumber node
                 | NODE{color=ref(ALIASED r), ...} => nodeNumber(chase node)
                 | _ => error "liveness.regmap"
           end handle _ => i                 (* XXX *)
       in  RaArch.Liveness.liveness(blocks, regmap)
       end

       (* 
        * Given a set of registers, remove all spilled and dedicated nodes.
        * NOTE: we assume that dedicated registers are NEVER entered into
        *       nodes Intmap. 
        *)
       fun collectNodes(getnode,regs) =  
       let fun loop([],xs) = xs
             | loop(r::rs,xs) = 
               (case chase(getnode r) of
                  NODE{color=ref(COLORED ~1),...} => loop(rs,xs) 
                | x => loop(rs,x::xs)
               ) handle _ => loop(rs,xs) (* dedicated *)
       in  loop(regs,[]) end


       (** 
        ** Builds the interference graph and initialMove list 
        **)
       fun build(graph as GRAPH{bitMatrix,nodes,...}) = 
       let (* The movecnt field is used to (lazily) record members in the 
            * live set. Deleted members are removed during an 
            * addEdgeForallLive operation.
            *)
           val getnode = Intmap.map nodes
           val chaseReg = chase o getnode
           val chaseRegs = map chaseReg
           val addEdge = Core.addEdge graph
           val member = BM.member bitMatrix
           fun memBitMatrix(NODE{number=x,...}, NODE{number=y,...}) =
               member (if x<y then (x, y) else (y, x))

           fun delete(NODE{movecnt, ...}) = movecnt:=0
           fun insert((node as NODE{movecnt as ref 0, ...})::rest, live) = 
                (movecnt:=1; insert(rest, node::live))
             | insert(_::rest, live) = insert(rest, live)
             | insert([], live) = live
           fun addEdgeForallLive([], live) = live
             | addEdgeForallLive(d::ds, live) = 
               let fun f ([], pruned) = pruned
                     | f ((n as NODE{movecnt as ref 1, ...})::rest, pruned) =
                         (addEdge(d, n); f(rest, n::pruned))
                     | f (_::rest, pruned) = f(rest, pruned)
               in addEdgeForallLive(ds, f(live, []))
               end
           fun forallBlocks(~1, mvs) = mvs
             | forallBlocks(n, mvs) = 
               let val F.BBLOCK{insns, liveOut, ...} = A.sub(cblocks, n)
                   val bdu = A.sub(blockDU, n)
                   fun doBlock([], _, live, mvs) = 
                         (app (fn NODE{movecnt, ...} => movecnt := 0) live;
                          forallBlocks(n-1, mvs))
                     | doBlock(instr::rest, (def',use')::bdu, live', mvs) = 
                       let val def = map chase def'
                           val use = map chase use'
                          (* move instructions are treated specially *)
                          (* There  is a subtle interaction between parallel
                              moves and interference graph construction. When we
                              have {d1, ... dn} <- {s1, ... sn} and liveOut we 
                              should make di interfere with:
        
                                  liveOut U {d1, ... dn} U ({s1, ... sn} \ {si})

                              This is not currently done.
                           *)
                          fun zip(d::defs, u::uses) = 
                              if isDedicated d orelse 
                                 isDedicated u then zip(defs, uses) 
                              else 
                              let val d as NODE{number=x,...} = chaseReg d
                                  val u as NODE{number=y,...} = chaseReg u 
                              in  if x = y then zip(defs,uses)
                                  else
                                   MV{dst=d, src=u, status=ref WORKLIST}::
                                    zip(defs, uses)
                              end 
                            | zip([],[]) = mvs

                         (* Assumes that the move temporary 
                          * if present is always the
                          * first thing on the definition list.
                          *)
                         val moves = 
                           if P.moveInstr instr then 
                           let val (defs,uses) = RaArch.defUse instr
                               val defs =
                                   case defs of 
                                     [] => []
                                   | _::rest => case P.moveTmpR instr of
                                                   SOME _ => rest
                                                |  NONE => defs
                           in  zip(defs,uses)
                           end
                           else mvs
                        val live = 
                           if length def > 1 then
                             addEdgeForallLive(def, insert(def, live'))
                           else addEdgeForallLive(def, live')
                     in app delete def;
                        doBlock(rest, bdu, insert(use,live), moves)
                     end  
                   val lout = collectNodes(getnode,RaArch.regSet(!liveOut))
                in doBlock(!insns, bdu, insert(lout, []), mvs)
                end
             (* Filter moves that already have an interference.
              * Also initialize the movelist and movecnt fields at this time.
              *)
             fun filter [] = []
               | filter (MV{src=NODE{color=ref(COLORED _), ...}, 
                            dst=NODE{color=ref(COLORED _), ...}, ...}::rest) = 
                   filter rest
               | filter ((mv as MV{src, dst, ...})::rest) = 
                 if memBitMatrix(src, dst) then filter rest
                 else let 
                  fun info(u as NODE{color=ref PSEUDO, movecnt, movelist,...}) =
                      (movelist := mv :: !movelist;   movecnt := 1 + !movecnt)
                    | info _ = ()
                in info src;  info dst;  mv::filter rest
                end
        in filter(forallBlocks(numOfBlocks-1, []))
        end (* build *)

        (** 
         ** select a spill node 
         **)
        fun selectSpill (GRAPH{nodes,spillFlag,K,...}, 
              {simplifyWkl, spillWkl, stack, moveWkl, freezeWkl}) =
        let (* duCount: compute the def/use points of spilled nodes. *)
            val getnode  = Intmap.map nodes
            val chaseReg = chase o getnode
            fun duCount spillable = 
            let val size = length spillable
                exception Info
                val defInfo : info = Intmap.new(size,Info)
                val useInfo : info = Intmap.new(size,Info)
                val addDef = Intmap.add defInfo 
                val addUse = Intmap.add useInfo
                val getDefs = Intmap.mapWithDefault (defInfo,[]) 
                val getUses = Intmap.mapWithDefault (useInfo,[])

                (* doblocks --- 
                 * updates the defInfo and useInfo tables to indicate
                 * the blocks where spillable live ranges are defined and used.
                 *)
                fun doblocks ~1 = ()
                  | doblocks blknum = 
                    let val bdu = A.sub(blockDU,blknum)
                        fun iter [] = ()
                          | iter((def',use')::rest) = 
                            let val def = uniqMap(nodeNumber o chase, def')
                                val use = uniqMap(nodeNumber o chase, use')
                                fun updateDef n = addDef(n, blknum::getDefs n)
                                fun updateUse n = addUse(n, blknum::getUses n)
                            in  app updateDef (SL.intersect(def,spillable));  
                                app updateUse (SL.intersect(use,spillable));   
                                iter rest
                            end
                    in iter(bdu);
                       doblocks(blknum-1)
                    end

                (* If a node is live going out of an block terminated by 
                 * an escaping branch, it may be necessary to reload the
                 * the node just prior to taking the branch. We will therefore
                 * record this as a definition of the node.
                 *)
                fun doBBlocks n = 
                let val F.BBLOCK{blknum,liveIn,liveOut,succ,...} = 
                         A.sub(cblocks,n)
                    val liveout = 
                     uniqMap (nodeNumber,
                               collectNodes(getnode,RaArch.regSet(!liveOut)))
                in  case !succ of 
                      [(F.EXIT _,_)] => 
                       (case SL.intersect(spillable,liveout) 
                          of [] => doBBlocks(n+1)
                           | some =>
                             (app (fn n => addDef(n, blknum::getDefs n)) some;
                              doBBlocks (n+1))
                       (*esac*))
                    | _ => doBBlocks(n+1)
                    (*esac*)
                end (* doBBlocks *) 
            in  doblocks (numOfBlocks - 1);
                doBBlocks 0 handle _ => ();
                (defInfo,useInfo)
            end (* duCount *)

            (* Since the spillWkl is not actively maintained, the set of
             * spillable nodes for which def/use info is needed is a subset
             * of spillWkl.
             *)
            fun remainingNodes() = 
            let fun prune [] = []
                  | prune((n as NODE{color=ref PSEUDO, ...}) ::ns) =  
                      n::prune ns
                  | prune((n as NODE{color=ref(ALIASED _), ...})::ns) = 
                      prune(chase n::ns)
                  | prune(_::ns) = prune ns
            in  case !remainInfo of 
                  SOME info => prune spillWkl
                | NONE => 
                  let (* first time spilling *)
                      val spillable = prune ( spillWkl)
                  in remainInfo := 
                       (case spillable 
                         of [] => NONE
                          | _ => SOME(duCount(uniqMap(nodeNumber, spillable)))
                        (*esac*));
                     spillable
                  end
            end

            (** apply the Chaitin heuristic to find the spill node **)
            fun chaitinHeuristic(spillable) = 
            let val infinity = 1000000.0
                val infinityi= 1000000
                val SOME(dinfo,uinfo) = !remainInfo
                val getdInfo = Intmap.map dinfo
                val getuInfo = Intmap.map uinfo
                fun coreDump [] = ()
                  | coreDump ((node as NODE{number, degree, adj, ...})::rest) = 
                    (print(concat
                      ["number =", Int.toString number,
                       " node =", Int.toString(nodeNumber (chase node)),
                       " degree = ", Int.toString (!degree),
                       " adj = "]);
                     prList(map (nodeNumber o chase) (!adj), "");
                     print "\n";
                     coreDump rest)
                fun iter([],node,cmin) = 
                       if node <> ~1 then 
                          (if !cfg_after_spill then 
                              print("Spilling node "^Int.toString node^
                                    " cost="^Real.toString cmin^"\n") else ();
                           getnode node
                          ) 
                       else (coreDump spillable; 
                             prList(Intmap.keys prevSpills,"PrevSpills: ");
                             error "chaitinHeuristic.iter")
                  | iter((node as NODE{number, degree, ...})::rest,cnode,cmin) =
                    let
                       (* An exeception will be raised if the node is defined
                        * but not used. This is not a suitable node to spill.
                        *)
                        val cost = 
                          (length(getdInfo number) handle _ => 0) +
                              (length(getuInfo number) handle _ => infinityi)
                        val heuristic = real cost / real (!degree)
                    in
                        if heuristic < cmin andalso not(isSpilled number)
                        then iter(rest, number, heuristic)
                        else iter(rest, cnode, cmin)
                    end
            in iter(spillable, ~1, infinity)
            end
       in case mode of 
            COPY_PROPAGATION =>
              {spillWkl=[], simplifyWkl=[], stack=[], moveWkl=[], freezeWkl=[]}
          | REGISTER_ALLOCATION => 
            (case remainingNodes() of 
               [] => {spillWkl=[], simplifyWkl=simplifyWkl, 
                      stack=stack, moveWkl=moveWkl, freezeWkl=freezeWkl}
             | spillWkl => 
               let val spillNode = chaitinHeuristic(spillWkl)
                   val simpWkl = 
                       if isMoveRelated spillNode
                       then spillNode::Core.wklFromFrozen(K,spillNode)
                       else [spillNode]
               in  spillFlag:=true;
                   {simplifyWkl=simpWkl,
                    spillWkl = spillWkl,
                    freezeWkl = freezeWkl,
                    stack = stack,
                    moveWkl = moveWkl}
               end
               (*esac*))
       end (* selectSpill *)


       (** rewriteGraph(spillList) - 
        **   an unsuccessful round of coloring has taken
        **   place with nodes in spillList having been spilled. The
        **   flowgraph must be updated and the entire process repeated. 
        **)
       fun rewriteGraph (graph as GRAPH{nodes,...}, spillList) = 
       let val _       = rewrite := !rewrite + 1
           val SOME(dInfo,uInfo) = !remainInfo
           val getnode = Intmap.map nodes
           val enternode = Intmap.add nodes
           val chaseReg = chase o getnode
           val chaseRegs = map chaseReg
          
           fun newdu (d, u) = 
           let fun rmv([],nodes) = nodes
                 | rmv(r::rs,nodes) = 
                   let val node = chase(getnode r) handle _ => 
                                  let val n = newNode(r, PSEUDO)
                                  in  enternode (r, n); n
                                  end
                   in rmv(rs,node::nodes) end
               fun rmv' rs = rmv(rmvDedicated rs,[])
           in (rmv' d, rmv' u)
           end (* newdu *)

           val defUse = newdu o RaArch.defUse

           (* blocks where spill code is required for node n *)
           fun affectedBlocks node = 
           let val n = nodeNumber node
           in  SL.merge(SL.uniq(Intmap.mapWithDefault (dInfo,[]) n),
                        SL.uniq(Intmap.mapWithDefault (uInfo,[]) n))
           end

           val mapr = C.lookup regmap
           val markProh = app markAsSpilled

           (* Insert spill code into the affected blocks *)
           fun doBlocks([], _) = ()
             | doBlocks(blknum::rest, node) = 
           let val F.BBLOCK{insns, liveOut, name, ...} = 
                     A.sub(cblocks, blknum)
               val bdu = A.sub(blockDU, blknum)
               val liveOut = collectNodes(getnode,RaArch.regSet(!liveOut))
               val spillReg = nodeNumber node

               (* note: the instruction list start out in reverse order. *)
               fun doInstrs([], [], newI, newBDU) =
                     (rev newI, rev newBDU)
                 | doInstrs(instr::rest, (du as (d,u))::bDU, newI, newBDU) = 
                   let val defs=map chase d
                       val uses=map chase u

                       fun outputInstrs(instrs, I, bDU) = 
                           {newI=instrs @ I, 
                            newBDU=(map defUse instrs) @ bDU}

                       fun newReloadCopy(rds, rss) = 
                       let fun f(rd::rds, rs::rss, rds', rss') = 
                               if mapr rs = spillReg 
                               then(([rd], [rs]), (rds@rds', rss@rss'))
                               else f(rds, rss, rd::rds', rs::rss')
                             | f([], [], _, _) = error "newReloadCopy.f"
                       in f(rds, rss, [], []) end
            
                       (* insert reloading code and continue *)
                       fun reloadInstr(instr, du, newI, newBDU)=
                       let val {code, proh} = 
                               RaUser.reload{regmap=mapr, instr=instr, 
                                             reg=spillReg, id=name}
                           val _ = markProh proh
                           val {newI, newBDU} = 
                               outputInstrs(code, newI, newBDU)
                       in doInstrs(rest, bDU, newI, newBDU) end

                       (* insert reload code for copies. *)
                       fun reloadCopy(du, instr, newI, newBDU) =
                           if nodeMember(node, #2 du) then 
                           (case (P.moveDstSrc(instr)) 
                            of ([d], [u]) => 
                              reloadInstr(instr,du,newI,newBDU)
                            | (defs, uses) => 
                              let val (mv, cpy) = newReloadCopy(defs, uses)
                                  val cpyInstr = RaUser.copyInstr(cpy, instr)
                                  val duCpy = defUse cpyInstr
                                  val {code, proh} =
                                    RaUser.reload
                                     {regmap=mapr, 
                                      instr=RaUser.copyInstr(mv, instr), 
                                      reg=spillReg, id=name}
                                  val _ = markProh proh
                                  val {newI, newBDU} = 
                                      outputInstrs(code, newI, newBDU)
                             in  (* recurse to deal with multiple uses *)
                                 reloadCopy(duCpy, cpyInstr, newI, newBDU) 
                             end
                             (*esac*))
                          else
                             doInstrs(rest, bDU, instr::newI, du::newBDU)

                      (* insert reload code *)
                      fun reload(du as (d,u), instr, newI, newBDU) = 
                        if P.moveInstr(instr) then 
                           reloadCopy(du, instr, newI, newBDU)
                        else if nodeMember(node, u) then 
                          let val {code, proh} = 
                                RaUser.reload{regmap=mapr, instr=instr, 
                                              reg=spillReg, id=name}
                              val {newI, newBDU} = 
                                     outputInstrs(code, newI, newBDU)
                              val _ = markProh proh
                          in doInstrs(rest, bDU, newI, newBDU)
                          end
                       else
                         doInstrs(rest, bDU, instr::newI, du::newBDU)


                      fun spillInstr(instr, newI, newBDU) = 
                      let val {code, instr, proh} = 
                           RaUser.spill{regmap=mapr,  instr=instr, reg=spillReg, id=name}
                          val _ = markProh proh
                          val {newI, newBDU} = outputInstrs(code, newI, newBDU)
                      in case instr
                          of NONE => doInstrs(rest, bDU, newI, newBDU)
                           | SOME instr => 
                               reload(defUse instr, instr, newI, newBDU)
                      end
            
                      fun spillCopy() = 
                      let (* Note:: There is a guarantee that the node 
                           * will never be aliased to another register.
                           *)
                          fun newSpillCopy(rds, rss) = 
                          let fun f(rd::rds, rs::rss, rds', rss') = 
                              if mapr rd = spillReg then 
                                 (([rd], [rs]), (rds@rds', rss@rss'))
                              else f(rds, rss, rd::rds', rs::rss')
                                | f([], [], _, _) = error "newSpillCopy"
                          in f(rds, rss, [], []) end

                          fun spillCpyDst() = 
                          let val (mv, cpy) = newSpillCopy(P.moveDstSrc(instr))
                              val (newI, newBDU) = 
                              (case cpy
                               of ([],[]) => (newI, newBDU)
                                | _ => let val cpyInstr = RaUser.copyInstr(cpy, instr)
                                       in (cpyInstr::newI, defUse cpyInstr::newBDU)
                                       end
                              (*esac*))
                              val instr = RaUser.copyInstr(mv, instr)
                          in spillInstr(instr, newI, newBDU)
                          end
                      in case P.moveTmpR instr
                          of NONE => spillCpyDst()
                           | SOME r => 
                             if mapr r=spillReg 
                             then spillInstr(instr, newI, newBDU)
                             else spillCpyDst()
                         (*esac*)
                      end (* spillCopy *)
                   in (* insert spill code *)
                      if nodeMember(node, defs) then 
                         if P.moveInstr instr then spillCopy() 
                         else spillInstr(instr, newI, newBDU)
                      else
                        reload((defs,uses), instr, newI, newBDU)
                   end

               (* special action if the last instruction is an escaping
                * branch and the node is live across the branch.
                * We discover if the node needs to be spilled or reloaded.
                *)
               fun blockEnd(instrs as instr::rest, bDU as du::bdu) = 
               let fun escapes [] = false
                     | escapes (P.ESCAPES::_) = true
                     | escapes (_::targets) = escapes targets
               in if nodeMember(node, liveOut) then
                  (case P.instrKind instr
                   of P.IK_JUMP =>
                       if escapes(P.branchTargets instr) then let
                           val {code,...} = 
                             RaUser.reload{regmap=mapr, instr=instr, reg=spillReg, id=name}
                           val reloadDU = map defUse code
                         in (rev code@rest, rev reloadDU@bdu)
                         end
                       else (instrs, bDU)
                    | _ => (instrs, bDU)
                  (*esac*))
                  else (instrs, bDU)
               end
                 | blockEnd([],[]) = ([], [])

              val (newInstrs, newBdu) = 
                   doInstrs(!insns, bdu, [], [])
              val (newInstrs, newBdu) = blockEnd(newInstrs, newBdu)
          in  insns := newInstrs;
              A.update(blockDU, blknum, newBdu);
              doBlocks(rest, node)
          end (* doBlocks *)

          (* The optimistic coloring selection may come up with a node
           * that has already been spilled. Must be careful not to spill
           * it twice.
           *)
          fun glue [] = ()
            | glue((node as NODE{number, color, ...})::rest) =
               (if not(isSpilled number) then 
                  (doBlocks(affectedBlocks node, node)
                   before color := COLORED(spillRegSentinel)
                  )
                else ();
                glue rest
               )

          (* redoAlgorithm
           *  -- rerun graph coloring but note that spilling may 
           *     have introduced new registers.
           *)
          fun redoAlgorithm(spillList) = 
          let val _ = app (markAsSpilled o nodeNumber) spillList
              fun init(_, NODE{color=ref PSEUDO, degree, adj,  
                               movecnt, movelist, ...}) =
                     (degree:=0; adj := []; movecnt:=0; movelist:=[])
                | init _ = ()
          in Intmap.app init nodes
          end
       in glue(spillList);
          redoAlgorithm(spillList);
          debug(cfg_after_spill,"after spilling",blocks,regmap,an)
       end (* rewriteGraph *)

       (** 
        ** The main driver 
        **) 
       fun graphColoring(nodes) =
       let (* Create an empty interference graph *) 
           val graph = newGraph
                       {nodes=nodes,
                        K=RaUser.nFreeRegs,
                        numRegs=RaArch.numRegs(),
                        regmap=regmap,
                        getreg=RaUser.getreg,
                        firstPseudoR=firstPseudoR
                       }
           val moves      = build graph        (* build interference graph *)
           val worklists  = Core.makeWorkLists graph moves 
           val simpCoalFz = Core.simplifyCoalesceFreeze graph

           (* Note: freezeWkl or spillWkl are maintained lazily. *)
           fun iterate wl =  
               case simpCoalFz wl of
                 wl as {spillWkl= _::_, ...} => iterate(selectSpill(graph,wl))
               | wl =>
                 (case mode of
                   COPY_PROPAGATION => Core.finishCP graph
                 | REGISTER_ALLOCATION =>
                   (case Core.optimisticSpilling graph wl of
                     [] => Core.finishRA graph
                   | spills => (rewriteGraph(graph,spills); 
                                graphColoring(nodes))
                   )
                 )
       in  if !dump_graph then Core.dumpGraph graph else ();
           debug(cfg_before_ra,"before register allocation",blocks,regmap,an);
           iterate worklists
       end
       val nodes = initialize()
   in  liveness(nodes,blocks);   (* run liveness analysis *)
       graphColoring(nodes);
       debug(cfg_after_ra,"after register allocation",blocks,regmap,an);
       ra_count := !ra_count + 1;
       cluster
   end

end
