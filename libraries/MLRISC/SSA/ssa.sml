(*---------------------------------------------------------------------------
 * Machine SSA representation.
 *
 * -- Allen (leunga@cs.nyu.edu) 
 *---------------------------------------------------------------------------*)
functor SSA
  (structure SSAProps   : SSA_PROPERTIES
   structure InsnProps  : INSN_PROPERTIES
   structure FormatInsn : FORMAT_INSTRUCTION
   structure MLTreeComp : MLTREECOMP
   structure CFG        : SSA_FLOWGRAPH
   structure DJ         : DJ_GRAPH
   structure GCMap      : GC_MAP
     sharing SSAProps.I = InsnProps.I = CFG.I = FormatInsn.I = MLTreeComp.I
     sharing MLTreeComp.T = SSAProps.RTL.T
  ) : SSA =
struct
   structure CFG        = CFG
   structure Dom        = DJ.Dom
   structure DJ         = DJ
   structure SP         = SSAProps
   structure GCMap      = GCMap
   structure MLTreeComp = MLTreeComp
   structure W          = CFG.W
   structure I          = SP.I
   structure C          = I.C
   structure RTL        = SP.RTL
   structure T          = RTL.T
   structure OT         = SP.OT
   structure G          = Graph
   structure A          = Array
   structure W8A        = Word8Array
   structure DA         = DynArray
   structure HA         = HashArray
   structure L          = GraphLayout
   structure SL         = SortedList

   fun error msg = MLRiscErrorMsg.error("SSA",msg)

   (*------------------------------------------------------------------------
    * Flags 
    *------------------------------------------------------------------------*)
   val showAsm   = MLRiscControl.getFlag "ssa-show-asm" 
   val showPos   = MLRiscControl.getFlag "ssa-show-pos"
   val debug     = MLRiscControl.getFlag "ssa-debug"   
   val listLimit = MLRiscControl.getInt  "ssa-list-limit"  
   val _ = listLimit := 5

   (*------------------------------------------------------------------------
    * Counters (for statistics)
    *------------------------------------------------------------------------*)
   val replacements    = MLRiscControl.getCounter "ssa-replacements"
   val constantsFolded = MLRiscControl.getCounter "ssa-constants-folded"
   val branchesFolded  = MLRiscControl.getCounter "ssa-branches-folded"
   val moved           = MLRiscControl.getCounter "ssa-instructions-moved"

   (*------------------------------------------------------------------------
    * Basic type definitions used in the SSA form
    *------------------------------------------------------------------------*)
   type value  = int             (* value id *)
   type pos    = int             (* position within a block *)
   type block  = Graph.node_id   (* block id *)
   type ssa_id = Graph.node_id   (* ssa id *)
   type rtl    = RTL.rtl         (* RTL *)
   type const  = OT.const        (* constants *)
   type cfg = CFG.cfg            (* control flow graph *)
   type dom = (CFG.block,CFG.edge_info,CFG.info) Dom.dominator_tree  
   type nameTbl = {oldName:C.cell, index:int} IntHashTable.hash_table 

   (*------------------------------------------------------------------------
    * An SSA op is an instruction 
    *------------------------------------------------------------------------*)
   type ssa_op = I.instruction

   (*------------------------------------------------------------------------
    * Information about the SSA graph 
    *------------------------------------------------------------------------*)
   datatype ssa_info = 
      INFO of
      {cfg             : cfg,
       dom             : cfg -> dom,
       defSiteTbl      : ssa_id DA.array,       
       blockTbl        : block DA.array,
       posTbl          : pos DA.array,
       rtlTbl          : rtl DA.array,
       usesTbl         : value list DA.array,
       defsTbl         : value list DA.array, 
       succTbl         : value Graph.edge list DA.array,
       ssaOpTbl        : ssa_op DA.array,
       cellKindTbl     : C.cellkind IntHashTable.hash_table,
       operandTbl      : OT.operandTable,
       nameTbl         : nameTbl option,
       gcmap           : GCMap.gcmap option,
       nextImmed       : int ref,
       edgeCount       : int ref,
       nodeCount       : int ref,
       garbageNodes    : ssa_id list ref,
       hasDefUseChains : bool ref,
       nodes           : { sources: ssa_id list A.array,
                           phis:    ssa_id list A.array,
                           ops:     ssa_id list A.array,
                           sinks:   ssa_id list A.array
                         } option ref,
       maxPos          : int ref,
       minPos          : int ref,
       freqTbl         : W.freq A.array option ref
      }

   type ssa = (ssa_op,value,ssa_info) Graph.graph

   exception NoDefSite
   exception NoCellKind

   val i2s = Int.toString

   (*------------------------------------------------------------------
    * Hacks to deal with zero registers in the architecture
    *------------------------------------------------------------------*)
   val R        = C.firstPseudo
   val zeroRegs = W8A.array(R, 0w0)
   val _        = app (fn k =>
                    case C.zeroReg k of
                      NONE => ()
                    | SOME r => W8A.update(zeroRegs, r, 0w1))
                  C.cellkinds

   (*------------------------------------------------------------------
    * Deal with pinned resources
    *------------------------------------------------------------------*)
   val pinnedUseTbl = W8A.array(R,0w0)
   val pinnedDefTbl = W8A.array(R,0w0)
   val _ = app (fn r => W8A.update(pinnedUseTbl,r,0w1)) SP.pinnedUse
   val _ = app (fn r => W8A.update(pinnedDefTbl,r,0w1)) SP.pinnedDef

   (*------------------------------------------------------------------------
    * How to create a new SSA graph
    *------------------------------------------------------------------------*)
   fun newSSA{cfg, dom, gcmap, nameTbl} =  
   let val nextImmed   = ref ~1
       val defSiteTbl  = DA.array(13, ~1)
       val blockTbl    = DA.array(13, ~1) 
       val posTbl      = DA.array(13, ~1) 
       val rtlTbl      = DA.array(13, T.SEQ [])
       val usesTbl     = DA.array(13, [])
       val defsTbl     = DA.array(13, [])
       val succTbl     = DA.array(13, [])
       val ssaOpTbl    = DA.array(13, InsnProps.nop())
       val cellKindTbl = IntHashTable.mkTable(13, NoCellKind) 
       val operandTbl  = OT.create nextImmed
       val nodeCount   = ref 0
       val edgeCount   = ref 0
       val garbageNodes = ref []
       val hasDefUseChains = ref false
       val info        =
           INFO{ cfg             = cfg,
                 dom             = dom,
                 defSiteTbl      = defSiteTbl,
                 blockTbl        = blockTbl,
                 posTbl          = posTbl,
                 rtlTbl          = rtlTbl,
                 usesTbl         = usesTbl,
                 defsTbl         = defsTbl,
                 succTbl         = succTbl,
                 ssaOpTbl        = ssaOpTbl,
                 cellKindTbl     = cellKindTbl,
                 operandTbl      = operandTbl, 
                 nameTbl         = nameTbl,
                 gcmap           = gcmap,
                 nextImmed       = nextImmed,
                 edgeCount       = edgeCount,
                 nodeCount       = nodeCount,
                 garbageNodes    = garbageNodes,
                 hasDefUseChains = hasDefUseChains,
                 nodes           = ref NONE,
                 maxPos          = ref 0,
                 minPos          = ref 0,
                 freqTbl         = ref NONE
               }
       (*--------------------------------------------------------------------
        * Graph methods 
        *--------------------------------------------------------------------*)
       fun nop _ = ()
       fun unimplemented title = error("unimplemented: "^title)
       val entries    = ref []
       val exits      = ref []
       val newNodes   = ref []
       fun order() = !nodeCount
       fun size()  = !edgeCount
       fun capacity() = DA.length ssaOpTbl
       fun new_id() = 
           case !newNodes of 
             []   => DA.length ssaOpTbl
           | h::t => (newNodes := t; h)
       fun garbage_collect () =
          (newNodes := (!newNodes) @ (!garbageNodes); garbageNodes := [])
       fun add_edge(e as (i,j,r)) = 
           (edgeCount := !edgeCount + 1;
            DA.update(succTbl, i, (i,j,r)::DA.sub(succTbl, i))
           )
       fun set_out_edges(n,es) = unimplemented "set_out_edges" 
       fun set_in_edges(n,es) = unimplemented "set_in_edges" 
       fun add_node n = unimplemented "add_node" 

       (* Invariant: all uses of these nodes must have already been removed! *)
       and remove_node n = 
       if DA.sub(blockTbl, n) < 0 then () (* already removed *)
       else let fun removeUses([], c) = c
                  | removeUses(v::vs, c) = 
                    if v < 0 then removeUses(vs, c)
                    else
                    let val i = DA.sub(defSiteTbl, v)
                    in  if i = n then removeUses(vs, c)
                        else 
                        let fun filterEdges([], es', c) = (es', c)
                              | filterEdges((e as (i,j,_))::es, es', c) =
                                if j = n then filterEdges(es, es', c-1)
                                else filterEdges(es, e::es', c)
                            val es = DA.sub(succTbl, i)
                            val (es, c) = filterEdges(es, [], c)
                        in  DA.update(succTbl, i, es);
                            removeUses(vs, c)
                        end
                    end 
                val uses = DA.sub(usesTbl, n)
                val size = removeUses(uses, !edgeCount)
            in  edgeCount := size - length (DA.sub(succTbl, n));
                nodeCount := !nodeCount - 1;
                garbageNodes := n :: !garbageNodes; 
                DA.update(blockTbl, n, ~1);
                DA.update(usesTbl, n, []);
                DA.update(defsTbl, n, []);
                DA.update(succTbl, n, [])
            end      
       
       fun set_entries ns = entries := ns
       fun set_exits ns = exits := ns
       fun get_entries() = !entries
       fun get_exits() = !exits
       fun get_succ n = map #2 (DA.sub(succTbl, n))
       fun get_pred n = 
           let val defSite = DA.baseArray defSiteTbl
               fun collect([], preds) = preds
                 | collect(v::vs, preds) = 
                   if v < 0 then collect(vs, preds)
                   else let val n = A.sub(defSite, v)
                        in  collect(vs, n::preds) end
           in  collect(DA.sub(usesTbl,n), []) end 
       fun get_nodes() =
       let val block  = DA.baseArray blockTbl
           val ssaOps = DA.baseArray ssaOpTbl
           val n      = DA.length blockTbl
           fun collect(i, nodes) =  
               if i >= 0 then 
                 collect(i-1, 
                   if A.sub(block, i) >= 0 then 
                     (i,A.sub(ssaOps,i))::nodes else nodes)
               else nodes
       in  collect(n-1, []) end

       fun get_edges() =  
       let val succ = DA.baseArray succTbl
           val n    = DA.length succTbl
           fun collect(i, edges) = 
               if i >= 0 then 
                  collect(i-1, List.revAppend(A.sub(succ, i), edges))
               else edges
       in  collect(n-1, []) end

       fun out_edges n = DA.sub(succTbl, n)
       fun in_edges n =
       let val defSite = DA.baseArray defSiteTbl
           fun collect([], edges) = edges
             | collect(v::vs, edges) = 
               if v < 0 then collect(vs, edges)
               else let val n' = A.sub(defSite, v)
                    in  collect(vs, (n',n,v)::edges) end
       in  collect(DA.sub(usesTbl,n), []) end
       fun has_edge(i,j) = List.exists(fn (_,k,_) => j = k) (DA.sub(succTbl, i))
       fun has_node n = DA.sub(blockTbl,n) >= 0
       fun node_info n = DA.sub(ssaOpTbl, n)

       fun forall_nodes f =
       let val block  = DA.baseArray blockTbl
           val ssaOps = DA.baseArray ssaOpTbl
           val n      = A.length block
           fun app i =  
               if i >= n then () else
               (if A.sub(block, i) >= 0 then f(i, A.sub(ssaOps,i)) else ();
                app(i+1))  
       in  app 0 end

       fun forall_edges f =
       let val succ = DA.baseArray succTbl
           val n    = A.length succ
           fun loop i = if i >= n then () else (iter(A.sub(succ, i)); loop(i+1))
           and iter [] = ()
             | iter (e::es) = (f e; iter es)
       in  loop 0 end

   in  G.GRAPH
         {name            = "SSA",
          graph_info      = info,
          new_id          = new_id,
          add_node        = add_node,
          add_edge        = add_edge,
          remove_node     = remove_node,
          set_in_edges    = set_in_edges,
          set_out_edges   = set_out_edges,
          set_entries     = set_entries,
          set_exits       = set_exits,
          garbage_collect = garbage_collect,
          nodes           = get_nodes,
          edges           = get_edges,
          order           = order,
          size            = size,
          capacity        = capacity,
          out_edges       = out_edges,
          in_edges        = in_edges,
          succ            = get_succ,
          pred            = get_pred,
          has_edge        = has_edge,
          has_node        = has_node,
          node_info       = node_info,
          entries         = get_entries,
          exits           = get_exits,
          entry_edges     = fn _ => [],
          exit_edges      = fn _ => [],
          forall_nodes    = forall_nodes,
          forall_edges    = forall_edges
         }
   end  

   (*------------------------------------------------------------------------
    * Extract info from the SSA graph
    *------------------------------------------------------------------------*)
   fun info(G.GRAPH ssa) = let val INFO info = #graph_info ssa in info end
   fun dom SSA = 
   let val {cfg, dom, ...} = info SSA   (* extracts the dominator *)
   in  dom cfg end
   fun cfg SSA = #cfg(info SSA)   (* extracts the CFG *)
   fun immed SSA = OT.int(#operandTbl(info SSA)) (* create a new operand *)
   (*fun label SSA = OT.label(#operandTbl(info SSA))*) (* create a new label *)
   fun const SSA = OT.const(#operandTbl(info SSA)) (* lookup const values *)
   fun operand SSA = OT.operand(#operandTbl(info SSA))

   fun maxVar SSA = C.maxCell()        (* maximum number of ssa names *)
   fun numberOfOperands SSA = (* number of operands *)
   let val {nextImmed, ...} = info SSA
   in  ~(!nextImmed) - 1 end

   (*------------------------------------------------------------------------
    * Reserve n nodes of storage in all the dynamic tables.
    *------------------------------------------------------------------------*)
   fun reserve SSA n = 
   let val {defsTbl, usesTbl, ssaOpTbl, rtlTbl, 
            blockTbl, posTbl, succTbl, ...} = info SSA
   in  (* if !debug then
           print("[SSA: reserving "^i2s n^" nodes]\n")
       else ();  *)
       DA.expandTo(defsTbl, n);
       DA.expandTo(usesTbl, n);
       DA.expandTo(ssaOpTbl, n);
       DA.expandTo(rtlTbl, n);
       DA.expandTo(blockTbl, n);
       DA.expandTo(posTbl, n);
       DA.expandTo(succTbl, n)
   end

   (*------------------------------------------------------------------------
    * Extract the raw tables.  
    * These should only be used when the optimization guarantees that
    * no new ssa ops are added to the graph, since that may involve resizing
    * these tables, rendering them obsolete.  
    *------------------------------------------------------------------------*)
   fun defSiteTbl SSA = DA.baseArray(#defSiteTbl(info SSA))
   fun blockTbl SSA = DA.baseArray(#blockTbl(info SSA))
   fun posTbl SSA = DA.baseArray(#posTbl(info SSA))
   fun rtlTbl SSA = DA.baseArray(#rtlTbl(info SSA))
   fun usesTbl SSA = DA.baseArray(#usesTbl(info SSA))
   fun defsTbl SSA = DA.baseArray(#defsTbl(info SSA))
   fun succTbl SSA = DA.baseArray(#succTbl(info SSA))
   fun ssaOpTbl SSA = DA.baseArray(#ssaOpTbl(info SSA))
   fun cellKindTbl SSA = #cellKindTbl(info SSA)
   fun operandTbl SSA = #operandTbl(info SSA)
   fun maxPos SSA = #maxPos(info SSA)
   fun minPos SSA = #minPos(info SSA)

   (*------------------------------------------------------------------------
    * Lookup information (the safe way)
    *------------------------------------------------------------------------*)
   fun defSite G = let val t = #defSiteTbl(info G) in fn v => DA.sub(t, v) end 
   fun block G = let val t = #blockTbl(info G) in fn n => DA.sub(t, n) end 
   fun rtl G = let val t = #rtlTbl(info G) in fn n => DA.sub(t, n) end 
   fun uses G = let val t = #usesTbl(info G) in fn n => DA.sub(t, n) end
   fun defs G = let val t = #defsTbl(info G) in fn n => DA.sub(t, n) end
   fun freqTbl G =
       case #freqTbl(info G) of
         ref(SOME t) => t
       | t as ref NONE =>
         let val G.GRAPH cfg = cfg G
             val N = #capacity cfg ()
             val freqTbl = A.array(N, 0)
         in  #forall_nodes cfg (fn (n,n') =>
                 A.update(freqTbl, n, !(CFG.freq n')));
             t := SOME freqTbl;
             freqTbl
         end

   (*------------------------------------------------------------------------
    * Pretty printing a value
    *------------------------------------------------------------------------*)
   fun prInt i = if i < 0 then "-"^i2s(~i) else i2s i
   fun prIntInf i = if IntInf.sign i < 0 then 
                       "-"^IntInf.toString(IntInf.~ i) 
                    else IntInf.toString i
   fun showVal SSA = 
   let val {nameTbl, cellKindTbl, gcmap, ...} = info SSA
       val const = const SSA
       val cellKind = IntHashTable.find cellKindTbl
       val cellKind = fn r => case cellKind r of SOME k => k | NONE => C.GP 

       (* Display gc type if a gc map is present *)
       val showGC = 
           case gcmap of
             NONE => (fn r => "")
           | SOME gcmap => 
             let val look = IntHashTable.lookup gcmap
             in  fn r => ":"^GCMap.GC.toString(look r) handle _ => ":?" end

       (* Display fancy name if a name table is present *)
       val translate =
           case nameTbl of
             NONE =>     (fn (k,v) => C.toString k v)
           | SOME tbl => 
             let val look = IntHashTable.lookup tbl
             in  fn (k,v) => 
                    let val {oldName,index} = look v
                    in  C.toString k oldName^"."^i2s index end
                    handle _ => (C.toString k v)
             end

       (* Lookup name *)
       fun lookupName v = 
       let val k = cellKind v
           val gcTy = if k = C.MEM orelse k = C.CTRL then "" else showGC v
       in  translate(k,v) ^ gcTy end

       (* Show a value *)
       fun show v = 
           if v >= 0 then lookupName v
           else (case const v of
                  SP.OT.INT i => prInt i
                | SP.OT.INTINF i => prIntInf i
                | SP.OT.OPERAND opnd => "v"^i2s(~v)
                (*| SP.OT.LABEL l => Label.nameOf l*)
                ) handle SP.OT.NoConst => "?"^i2s(~v)
   in  show end

   (*------------------------------------------------------------------------
    * Pretty printing an ssa op 
    *------------------------------------------------------------------------*)
   fun showOp SSA = 
   let val {usesTbl, defsTbl, ssaOpTbl, rtlTbl, succTbl, 
            blockTbl, posTbl, cellKindTbl, ...} = info SSA
       val K       = !listLimit 
       val showVal = showVal SSA
       val cfg     = cfg SSA
       val regmap  = CFG.regmap cfg
       val asm     = FormatInsn.toString (!(CFG.annotations cfg))
                                         (C.lookup regmap)
       fun block b = "b"^i2s b
       fun blockOf ssa_id = block(DA.sub(blockTbl,ssa_id))
       val cellKindOf = IntHashTable.find cellKindTbl
       val cellKindOf = 
           fn r => case cellKindOf r of SOME k => k | NONE => C.GP 

       fun listify(vs, rs) =
       let fun h r = C.toString (cellKindOf r) r
           fun g(v,r) = showVal v^"="^h r
           fun f(_,[],[])       = ""
             | f(0,vs,rs)       = "\n   "^f(K,vs,rs)
             | f(n,[v],[r])     = g(v,r)
             | f(n,v::vs,r::rs) = g(v,r)^","^f(n-1,vs,rs)
             | f _ = error "showOp.listify"
       in  f(K,vs,rs) end

       fun listify2([b],[v])     = "["^block b^"]"^showVal v
         | listify2(b::bs,v::vs) = "["^block b^"]"^showVal v^","^listify2(bs,vs)
         | listify2 _ = ""

       fun show ssa_id = 
       let val ssa_op  = DA.sub(ssaOpTbl, ssa_id) 
           val defs    = DA.sub(defsTbl,ssa_id)
           val uses    = DA.sub(usesTbl,ssa_id)
           val rtl     = DA.sub(rtlTbl,ssa_id)
       in  case rtl of
             T.PHI{preds, ...} => 
               showVal(hd defs)^" := phi("^listify2(preds,uses)^")"
           | T.SINK{block=b,liveOut,...} => 
               "sink["^block b^"]("^listify(uses, liveOut)^")"
           | T.SOURCE{block=b,liveIn,...} => 
                (* Only pretty print the registers that are currently live *)
               let val edges = DA.sub(succTbl,ssa_id)
                   fun isLive r = List.exists (fn (_,_,r') => r = r') edges
                   fun collect([], [], ds', rs') = (rev ds', rev rs') 
                     | collect(d::ds, r::rs, ds', rs') =
                       if isLive d then collect(ds, rs, d::ds', r::rs')
                       else collect(ds, rs, ds', rs')
                   val (defs, liveIn) = collect(defs, liveIn, [], [])
               in  "source["^block b^"]("^listify(defs, liveIn)^")"
               end
           | _ => 
              let fun def v = showVal(List.nth(defs, v))
                  fun use v = showVal(List.nth(uses, v))
                  val ssa = RTL.rtlToString rtl
                  (* val ssa = #stm
                               (RTL.showRTL{def=def, use=use,
                                            regionDef=def, regionUse=use}) rtl
                   *)

                  val ssa = if !showPos then 
                               ssa^" #"^prInt(DA.sub(posTbl, ssa_id))  
                            else ssa
              in  if !showAsm then asm ssa_op^" ["^ssa^"]" else ssa end
       end
   in  show
   end

   (*------------------------------------------------------------------------
    * Pretty printing the rtl
    *------------------------------------------------------------------------*)
   fun showRTL SSA = RTL.rtlToString

   (*------------------------------------------------------------------------
    * Generate a renamed variable.  Propagate cellkind and gc type information
    *------------------------------------------------------------------------*)
   fun newRenamedVar SSA = 
   let val {nameTbl, cellKindTbl, gcmap, ...} = info SSA
       val lookupCellKind = IntHashTable.lookup cellKindTbl
       val addCellKind    = IntHashTable.insert cellKindTbl
       val updateGC =
           case gcmap of
             NONE   => (fn (r, r') => r')
           | SOME m => 
             let val lookup = IntHashTable.lookup m
                 val add    = IntHashTable.insert m
             in  fn (r,r') => (add(r', lookup r) handle _ => (); r') 
             end
       fun newVar r =
       let val r' = C.newVar r
       in  addCellKind(r', lookupCellKind r) handle _ => () ;
           updateGC(r, r')
       end

   in  case nameTbl of
         NONE => newVar
       | SOME nameTbl =>
         let val enterName = IntHashTable.insert nameTbl
             exception NoIndex
             val indexTbl  = IntHashTable.mkTable(31, NoIndex)
             val addIndex  = IntHashTable.insert indexTbl
             val findIndex = IntHashTable.find indexTbl
             val findIndex = 
                 fn r => case findIndex r of SOME i => i | NONE => 0
             fun newVarKeepName r = 
             let val r' = newVar r
                 val i  = findIndex r
             in  addIndex(r,i+1);
                 enterName(r', {oldName=r, index=i});
                 r'
             end
         in  newVarKeepName 
         end
   end

   (*------------------------------------------------------------------------
    * Generate variable.  Propagate gc type information only.
    *------------------------------------------------------------------------*)
   fun newVar SSA = 
   let val {gcmap, ...} = info SSA
   in  case gcmap of
          NONE => C.newVar
        | SOME m => 
          let val lookup = IntHashTable.lookup m
              val add    = IntHashTable.insert m
          in  fn r => let val r' = C.newVar r
                      in  add(r', lookup r) handle _ => (); r' end
          end
   end

   (*------------------------------------------------------------------------
    * Create a new SSA op.  The node must not already exist.
    *------------------------------------------------------------------------*)
   fun newOp SSA =
   let val {defSiteTbl, nodeCount, ...} = info SSA
       val defsTbl  = defsTbl SSA 
       val usesTbl  = usesTbl SSA
       val ssaOpTbl = ssaOpTbl SSA
       val blockTbl = blockTbl SSA
       val posTbl   = posTbl SSA
       val rtlTbl   = rtlTbl SSA

       fun new{id, instr, rtl, defs, uses, block, pos} = 
       let fun addDefSite [] = ()
              | addDefSite(r::rs) =
                ((*print("defSite["^showVal SSA r^"]="^i2s id^"\n");*)
                 DA.update(defSiteTbl,r,id); 
                 addDefSite rs
                )
       in  nodeCount := !nodeCount + 1;
           addDefSite defs;
           A.update(rtlTbl, id, rtl);
           A.update(defsTbl, id, defs);
           A.update(usesTbl, id, uses);
           A.update(ssaOpTbl, id, instr);
           A.update(blockTbl, id, block);
           A.update(posTbl, id, pos);
           (*print("["^i2s id^"] = "^showOp SSA id^"\n"); *)
           ()
       end
   in  new end

   (*------------------------------------------------------------------------
    * Iterators 
    *------------------------------------------------------------------------*)
   fun forallNodes SSA f =
   let val blockTbl = blockTbl SSA
       val n = A.length blockTbl 
       fun loop(i) = 
           if i >= n then () else 
           (if A.sub(blockTbl,i) >= 0 then f i else (); loop(i+1))
   in  loop 0 end

   fun foldNodes SSA f x =
   let val {blockTbl,...} = info SSA
       val n         = DA.length blockTbl
       val blockTbl  = DA.baseArray blockTbl
       fun fold(i,x) =
           if i < n then fold(i+1, if A.sub(blockTbl,i) >= 0 then f(i,x) else x)
           else x
   in  fold(0,x) end
  
   (*------------------------------------------------------------------------
    * Insert edges
    *------------------------------------------------------------------------*)
   fun computeDefUseChains SSA =
   let val usesTbl    = usesTbl SSA
       val succTbl    = succTbl SSA
       val defSiteTbl = defSiteTbl SSA
       val blockTbl   = blockTbl SSA
       val n = A.length succTbl
       fun iter(i, size) =
       if i < n then
       let fun addEdges([], size) = size
             | addEdges(v::vs, size) =
               if v < 0 then addEdges(vs, size)
               else let val j = A.sub(defSiteTbl, v) 
                    in  (* print(i2s i^" -> "^i2s j^"\n"^
                              showOp SSA i^"->"^showOp SSA j^" ("^
                              showVal SSA v^" "^i2s v^")\n"); *)
                        A.update(succTbl, j, (j,i,v)::A.sub(succTbl, j));
                        addEdges(vs, size+1)
                    end
           val uses = A.sub(usesTbl, i)
       in  iter(i+1, addEdges(uses, size))
       end
       else size

       val numberOfEdges = iter(0, 0)
       val {edgeCount, defsTbl, ... } = info SSA
   in  edgeCount := numberOfEdges
   end

   (*------------------------------------------------------------------
    * Function to remove useless phi-node from the graph
    *------------------------------------------------------------------*)
   fun removeUselessPhiFunctions(SSA as G.GRAPH ssa) =
   let val usesTbl     = usesTbl SSA
       val defsTbl     = defsTbl SSA
       val rtlTbl      = rtlTbl SSA
       val succTbl     = succTbl SSA
       val blockTbl    = blockTbl SSA
       val defSiteTbl  = defSiteTbl SSA
       val onWorklist  = W8A.array(#capacity ssa (), 0w0)
       val {nodeCount, edgeCount, ...} = info SSA
       val showVal = showVal SSA
       val showOp  = showOp SSA

       (* Replace all uses of t defined by phi-node i to t' *)
       fun removeUselessNode(i, t, t', WL) =
       let (* val _  = print("Useless ["^i2s i^"] "^showOp i^" "^
                             showVal t^" -> "^showVal t'^"\n"); *)
           val i' = A.sub(defSiteTbl, t')
           fun processEdges([], es_i', size, WL) = (es_i', size, WL)
             | processEdges((i,j,t)::es_i, es_i', size, WL) =
                (* remove self loops; *)
               if i = j
               then processEdges(es_i, es_i', size-1, WL)
               else
               let fun renameUses [] = []
                     | renameUses (v::vs) =
                        (if v = t then t' else v)::renameUses vs
                   val WL = case A.sub(rtlTbl,j) of
                      T.PHI _ =>
                         if W8A.sub(onWorklist, j) = 0w0 then
                           (W8A.update(onWorklist, j, 0w1); j::WL)
                         else WL
                    | _ => WL
                   val uses_j = A.sub(usesTbl, j)
               in  (* print("\t"^showOp j^" =>\n\t"); *)
                   A.update(usesTbl, j, renameUses uses_j);
                   (* print(showOp j^"\n"); *)
                   processEdges(es_i, (i',j,t')::es_i', size, WL)
               end

            (* Filter i from the use sites of i' *)
            fun filterI([], es_i', size) = (es_i', size)
              | filterI((e as (i',k,_))::es, es_i', size) = 
                if k = i then filterI(es, es_i', size-1)
                else filterI(es, e::es_i', size)

            val es_i  = A.sub(succTbl, i)
            val es_i' = A.sub(succTbl, i')
            val (es_i', size) = filterI(es_i', [], !edgeCount)
            val (es_i', size, WL) = processEdges(es_i, es_i', size, WL)
       in   edgeCount := size;
            nodeCount := !nodeCount - 1;
            A.update(succTbl, i', es_i');
            (* Remove node i *)
            A.update(succTbl, i,  []);
            A.update(defsTbl, i,  []);
            A.update(usesTbl, i,  []);
            A.update(blockTbl, i, ~1);
            WL
       end

       fun processWorkList [] = ()
         | processWorkList(i::WL) =
           let val _ = W8A.update(onWorklist,i,0w0)
               (* val _ = print("Processing "^i2s i^"\n") *)
               val _ = if A.sub(blockTbl,i) < 0 then error(i2s i)
                       else ()
               val [t] = A.sub(defsTbl,i)
               val s   = A.sub(usesTbl,i)
                (* Check if i is useless *)
               fun loop([],t') = removeUselessNode(i,t,t',WL)
                 | loop(v::vs,t') =
                   if v = t then loop(vs,t')
                   else if t' = ~1 then loop(vs,v)
                   else WL (* not useless *)
               val WL = loop(s, ~1)
           in  processWorkList WL end

       fun collectPhis(i,WL) =
           (case A.sub(rtlTbl,i) of
              T.PHI _ => (W8A.update(onWorklist, i, 0w1); i::WL)
           | _ => WL
           )

       val WL = foldNodes SSA collectPhis []

   in  processWorkList(WL)
   end

   (*------------------------------------------------------------------------
    * Remove all nodes.
    * Note: no duplicates allowed.
    *------------------------------------------------------------------------*)
   fun removeAllNodes SSA nodes =
   let val succTbl  = succTbl SSA
       val defsTbl  = defsTbl SSA
       val usesTbl  = usesTbl SSA
       val blockTbl = blockTbl SSA
       val {edgeCount, nodeCount, garbageNodes, ...} = info SSA 
       fun removeAll([], nodes, edges, garbage) = 
            (nodeCount := nodes; edgeCount := edges; garbageNodes := garbage)
         | removeAll(n::ns, nodes, edges, garbage) =
           if A.sub(blockTbl, n) < 0 
           then removeAll(ns, nodes, edges, garbage)
           else
           let val outEdges = A.sub(succTbl, n)
           in  nodeCount := !nodeCount - 1;
               A.update(blockTbl, n, ~1);
               A.update(usesTbl, n, []);
               A.update(defsTbl, n, []);
               A.update(succTbl, n, []);
               removeAll(ns, nodes-1, edges - length outEdges, n::garbage)
           end      
   in  removeAll(nodes,!nodeCount,!edgeCount,!garbageNodes) 
   end
 
   (*------------------------------------------------------------------------
    * Replace all use of one value with another.  Return true iff
    * all uses of "from" has been replaced by "to".
    * Note: The definition of "from" must dominate all uses of "to", as
    * required by the SSA form.
    *------------------------------------------------------------------------*)
   fun replaceAllUses(SSA as G.GRAPH ssa) =
   let val defSiteTbl = defSiteTbl SSA
       val usesTbl    = usesTbl SSA
       val succTbl    = succTbl SSA
       val posTbl     = posTbl SSA
       val rtlTbl     = rtlTbl SSA
       val {edgeCount, ...} = info SSA

       val cellKind = IntHashTable.find(cellKindTbl SSA)
       val cellKind = fn r => case cellKind r of SOME k => k | NONE => C.GP

       fun isReplaceable k = k = C.GP orelse k = C.FP

       fun replace{from, to, vn} = 
           isReplaceable(cellKind from) andalso
           let
               val old = A.sub(defSiteTbl, from)
               val new = A.sub(defSiteTbl, to)

               (* val _ = print("REPLACING "^showOp SSA old^
                             "("^showVal SSA from^") by "^
                             showOp SSA new^"( "^showVal SSA to^") vn="^
                             i2s vn^"\n") *)
               (* 
                * We directly manipulate the graph structure here.
                * Since the number of edges does not change, there is
                * no need to update the edge count.
                *)
               fun renameUses([], to) = []
                 | renameUses(r::rs, to) = 
                    (if r = from then to else r)::renameUses(rs, to)

               fun removeUse([], es') = es'
                 | removeUse((e as (_,j,r))::es,es') = 
                   if r = from then
                    (* Rename an argument of j *)
                    (replacements := !replacements + 1;
                     if vn < 0 (* is a constant that we are replacing *)
                        andalso 
                         (case A.sub(rtlTbl, j) of
                           (T.PHI _ | T.SINK _ ) => true
                         | _ => false
                         ) then
                      (* phi or sink node *)
                      (A.update(usesTbl, j, renameUses(A.sub(usesTbl,j), vn)); 
                       (* print("Replacing constant: "^showOp SSA j^"\n"); *)
                       edgeCount := !edgeCount - 1
                      )
                     else (* normal node *)
                      (A.update(usesTbl, j, renameUses(A.sub(usesTbl, j), to));
                       A.update(succTbl, new, (new,j,to)::A.sub(succTbl, new))
                      );
                     removeUse(es,es')
                    )
                   else 
                    removeUse(es, e::es')

               val edges = removeUse(A.sub(succTbl, old), []) 
           in  A.update(succTbl, old, edges);
               true
           end
           
   in  replace
   end

   (*------------------------------------------------------------------------
    * Replace the definition of value by const.  This will change the
    * instruction that defines value to a constant computation instruction.
    * Return true iff this operation is successful.
    *------------------------------------------------------------------------*)
   fun foldConstant SSA =
   let val constOf = const SSA
       val showOp  = showOp SSA
       val {edgeCount, posTbl, defsTbl, usesTbl, succTbl, rtlTbl, ssaOpTbl,
            defSiteTbl, minPos, blockTbl, ...} = info SSA 
       fun fold{value, const} = 
           let val i    = DA.sub(defSiteTbl, value)
               val defs = DA.sub(defsTbl, i)
           in  case (defs, constOf const) of
                 ([_], SP.OT.INT imm) => (* only one value defined; okay *)
                  if (case DA.sub(usesTbl, i) of
                        [v] => v < 0  (* already a constant! don't fold *)
                      | _   => false) then false
                  else
                  let (* Remove existing incoming edges *)
                      fun removeUses [] = ()
                        | removeUses(v::vs) = 
                          if v < 0 then removeUses vs else
                          let val j = DA.sub(defSiteTbl, v)
                              fun rmv([], es') = es'
                                | rmv((e as (j,k,_))::es, es') = 
                                  if k = i then rmv(es, es') 
                                  else (edgeCount := !edgeCount - 1; 
                                        rmv(es, e::es'))
                              val succ_j = DA.sub(succTbl, j)
                          in  DA.update(succTbl, j, rmv(succ_j, []));
                              removeUses vs
                          end

                      (* val _       = print("REPLACING "^showOp i^" -> "); *)
                      val instr   = InsnProps.loadImmed{t=value, immed=imm}
                      val oldRtl  = DA.sub(rtlTbl, i)
                      val _       = DA.update(ssaOpTbl, i, instr)
                      val newRtl  = SP.RTLProps.rtl instr
                      val _       = DA.update(rtlTbl, i, newRtl)
                      val oldUses = DA.sub(usesTbl, i)
                  in  removeUses(oldUses);
                      (* now has only one input! *)
                      DA.update(usesTbl, i, [const]); 
                      
                      (* If the instruction used to be a phi-node or
                       * a source node, find an appropriate place for
                       * this new instruction.
                       *)
                      case oldRtl of
                         (T.PHI _ | T.SOURCE _) =>
                         let val newPos = !minPos 
                         in  minPos := !minPos - 128;
                             DA.update(posTbl, i, newPos)
                         end
                      | _ => () (* keep the same position *)
                      ;
                      (* print(showOp i^"\n");
                      app (fn (_,j,_) => print ("\t"^showOp j^"\n")) 
                           (DA.sub(succTbl, i)); *)
                      true
                  end
              | _ => false (* can't fold *)
           end
   in  fold 
   end 

   (*------------------------------------------------------------------------
    * Move an instruction from one block to another
    *------------------------------------------------------------------------*)
   fun moveOp SSA = 
   let val posTbl     = posTbl SSA
       val blockTbl   = blockTbl SSA
       val defSiteTbl = defSiteTbl SSA
       val usesTbl    = usesTbl SSA
       val succTbl    = succTbl SSA
       val rtlTbl     = rtlTbl SSA
       val showOp     = showOp SSA
       val showVal    = showVal SSA
      
       val {maxPos, minPos, ...} = info SSA

       fun mv{id, block} = 
       let val _ = moved := !moved + 1
           fun earliest([], pos) = pos
             | earliest(v::vs, pos) =
               if v < 0 then earliest(vs, pos)
               else let val j   = A.sub(defSiteTbl,v)
                        val b_j = A.sub(blockTbl, j)
                    in  if block = b_j then
                          (case A.sub(rtlTbl, j) of
                             T.PHI _ => earliest(vs, pos)
                          | _ => earliest(vs, Int.max(pos, A.sub(posTbl, j)))
                          )
                        else
                          earliest(vs, pos)
                    end
           fun latest([], pos) = pos
             | latest((_,j,_)::es, pos) =
               let val b_j = A.sub(blockTbl,j)
               in  if block = b_j then
                      (case A.sub(rtlTbl, j) of
                         T.PHI _ => latest(es, pos)
                       | _ => latest(es, Int.min(pos, A.sub(posTbl, j)))
                      )
                   else
                      latest(es, pos)
               end

           fun sanityCheck(lo, hi)  =
               if lo > hi then
               let fun prOp j =
                   let val b_j   = A.sub(blockTbl, j)
                       val pos_j = A.sub(posTbl, j)
                   in  print("\t"^showOp j^" in block "^
                                    i2s b_j^":"^i2s pos_j^"\n")
                   end
                   fun prUse v = 
                       if v < 0 then print("\t"^showVal v^"\n")
                       else let val j = A.sub(defSiteTbl, v)
                            in  prOp j
                            end
                   fun prDef(_,j,_) = prOp j
               in  print "Uses=\n"; app prUse (A.sub(usesTbl, id));
                   print "Defs=\n"; app prDef (A.sub(succTbl, id));
                   error("move "^showOp id^" lo="^i2s lo^
                            " hi="^i2s hi^" block="^i2s block)
               end 
               else ()


           val uses = A.sub(usesTbl, id)
           val lo   = earliest(uses, !minPos)
           val hi   = latest(A.sub(succTbl, id), !maxPos)
           val pos  = if !minPos = lo then (maxPos := !maxPos + 128; hi-1)
                      else if !maxPos = hi then (minPos := !minPos - 128; lo+1)
                      else (minPos := !minPos - 128; 
                            maxPos := !maxPos + 128;
                            sanityCheck(lo, hi);
                            (lo + hi) div 2
                           )
       in  A.update(blockTbl, id, block);
           A.update(posTbl, id, pos)
       end
   in  mv
   end

   (*------------------------------------------------------------------------
    * Set the target of a conditional branch as true or false.
    * This removes the branch and eliminates all unreachable code.
    *------------------------------------------------------------------------*)
   fun setBranch(SSA as G.GRAPH ssa) =
   let val {cfg, ssaOpTbl, blockTbl, nodeCount, ...} = info SSA
       fun set{id, cond} = 
       let val b = DA.sub(blockTbl,id)
           val jmp = CFG.setBranch(cfg, b, cond)
       in  #remove_node ssa id;
           DA.update(ssaOpTbl, id, jmp);
           DA.update(blockTbl, id, b);
           nodeCount := !nodeCount + 1;
           branchesFolded := !branchesFolded + 1
       end
   in  set 
   end

   (*------------------------------------------------------------------------
    * Make sure that none of the tables have been resized
    *------------------------------------------------------------------------*)
   fun noResize SSA f x =
   let val b = blockTbl SSA
       val d = defSiteTbl SSA
       fun check() =
        (if b <> blockTbl SSA then error "node tables have changed" else ();
         if d <> defSiteTbl SSA then error "variable table has changed" else ()
        )
   in  let val y = f x in check(); y  end
       handle e => (check(); raise e)
   end
    

   (*------------------------------------------------------------------------
    * Signal that an SSA has been changed
    *------------------------------------------------------------------------*)
   fun changed SSA = 
   let val {nodes, ...} = info SSA
   in  nodes := NONE end

   (*------------------------------------------------------------------------
    * Linearize the representation
    *------------------------------------------------------------------------*)
   fun nodes SSA =
   let val {nodes, ...} = info SSA
   in  case !nodes of
         SOME nodes => nodes
       | NONE => let val n = linearizeNodes SSA
                 in  nodes := SOME n; n end
   end

   and linearizeNodes SSA = 
   let val G.GRAPH cfg = cfg SSA
       val N = #capacity cfg ()

       val blockTbl = blockTbl SSA
       val posTbl   = posTbl SSA
       val rtlTbl   = rtlTbl SSA

       val sinks   = A.array(N,[])
       val sources = A.array(N,[])
       val phis    = A.array(N,[])
       val ops     = A.array(N,[]) 

       fun ins(n) = 
       let val b = A.sub(blockTbl,n) 
       in  if b >= 0 then
             let val tbl = 
                 case A.sub(rtlTbl, n) of
                   T.PHI _ => phis
                 | T.SINK _ => sinks
                 | T.SOURCE _ => sources
                 | _ => ops
             in  A.update(tbl, b, n::A.sub(tbl, b))
             end
           else ();
           ins(n-1)
       end
       fun byPos(a,b) = A.sub(posTbl,a) > A.sub(posTbl,b)
   in  ins(A.length blockTbl - 1) handle Subscript => ();
       A.modify (ListMergeSort.sort byPos) ops;
       {sinks=sinks, sources=sources, phis=phis, ops=ops}
   end


   (*------------------------------------------------------------------------
    * Graphical Viewing
    *------------------------------------------------------------------------*)
   fun viewAsCFG SSA = 
   let val cfg = cfg SSA
       val {graph, node, edge} = CFG.viewStyle cfg
       val showOp = showOp SSA
       val {sinks, sources, phis, ops} = nodes SSA
       fun node(b,b') = 
       let val instrs = A.sub(sources, b) @ 
                        A.sub(phis, b) @
                        A.sub(ops, b) @
                        A.sub(sinks, b) 
           val text = String.concat (map (fn i => showOp i^"\n") instrs)
       in  [L.LABEL(CFG.headerText b' ^ text)]
       end
   in  L.makeLayout
         { graph = graph,
           node  = node,
           edge  = edge
         } cfg
   end

   fun viewAsSSA SSA = 
   let val showOp  = showOp SSA
       val showVal = showVal SSA
       fun graph _    = []
       fun node(i,_)  = [L.LABEL(showOp i)]
       fun edge(_,_,v) = [L.COLOR "red",L.LABEL(showVal v)]
   in  L.makeLayout
        { graph = graph,
          node  = node,
          edge  = edge
        } SSA
   end

   (*------------------------------------------------------------------------
    * Consistency checking
    *------------------------------------------------------------------------*)
   fun consistencyCheck(SSA as G.GRAPH ssa) = 
   let val defSiteTbl = defSiteTbl SSA
       val usesTbl    = usesTbl SSA
       val defsTbl    = defsTbl SSA
       val rtlTbl     = rtlTbl SSA
       val blockTbl   = blockTbl SSA
       val succTbl    = succTbl SSA
       val posTbl     = posTbl SSA
       val Dom        = dom SSA
       val showOp     = showOp SSA
       val showVal    = showVal SSA
       val dominates  = Dom.dominates Dom

       val hasError = ref false 

       fun posOf i = 
           case A.sub(rtlTbl,i) of
             T.PHI _ =>  ~10000000
           | _       => A.sub(posTbl,i)
 
       fun bug(i,msg) = 
          (print("ERROR [b"^i2s(A.sub(blockTbl,i))^":p"^i2s(posOf i)^
                 ":"^i2s i^"] "^showOp i^": "^msg^"\n");
           hasError := true
          )

       fun checkDefs i =
       let val defs = A.sub(defsTbl, i)
       in  app (fn r => 
                let val i' = A.sub(defSiteTbl,r) 
                in  if i <> i' then
                       bug(i,"wrong def site "^i2s i'^" for "^
                           showVal r) 
                    else ()
                end)
               defs
       end

       fun checkBlock(i, block) = 
           if A.sub(blockTbl,i) <> block then bug(i,"wrong block") else ()

       fun printEdge (i,j,r) =
           print("\t"^i2s i^" -> "^i2s j^" "^showVal r^"\n")

       fun domTest(i,j,r) = 
       let val b_i = A.sub(blockTbl, i)
           val b_j = A.sub(blockTbl, j)
       
           val ok = 
              case A.sub(rtlTbl,j) of
                T.PHI{preds, ...} =>
                 let fun scan(p::preds, v::vs) = 
                         r = v andalso dominates(b_i,p) orelse scan(preds, vs)
                       | scan _ = false
                 in  scan(preds, A.sub(usesTbl,j)) end
              |  _ => if b_i = b_j then posOf i < posOf j
                      else dominates(b_i, b_j)
       in  if ok then () 
           else bug(i,showVal r^
                    " does not dominate "^showOp j^
                    " b"^i2s(A.sub(blockTbl,j))^" p"^i2s(posOf j))
       end

       fun checkEdges i =
           let val defs = A.sub(defsTbl, i)
               val edges = A.sub(succTbl, i)
               fun checkEdge(i',j',r) = 
                   (if i' <> i then bug(i, "bad edge source") else ();
                    if A.sub(blockTbl,j') < 0 then
                      bug(i, "use in node "^i2s j'^" is dead") else ();
                    if not(List.exists (fn r' => r = r') defs) then
                      bug(i, showVal r^" is not a definition") else ();
                    if not(List.exists (fn r' => r = r') 
                              (A.sub(usesTbl,j'))) then 
                      bug(i, showOp j'^" has no use of "^showVal r) else ();
                    domTest(i',j',r)
                   )
           in  app checkEdge edges
           end

       fun showVals(title,rs) =
            print(title^"="^foldr (fn (r,"") => showVal r 
                                    | (r,s) => showVal r^","^s) "" rs^
                  " ("^i2s(length rs)^")\n")

       fun checkLiveIn(i, liveIn) =
           let val defs = A.sub(defsTbl, i)
               val n    = length defs
               val m    = length liveIn
           in  if n <> m then 
                  (bug(i, "|liveIn| <> |defs|");
                   showVals("liveIn", liveIn);
                   showVals("defs", defs)
                  )
               else () 
           end

       fun checkLiveOut(i, liveOut) =
           let val uses = A.sub(usesTbl, i)
               val n    = length uses
               val m    = length liveOut
           in  if n <> m then 
                  (bug(i, "|liveOut| <> |uses|");
                   showVals("liveOut", liveOut);
                   showVals("uses", uses)
                  )
               else () 
           end

       fun checkNode(i, _) =
           case A.sub(rtlTbl, i) of
             T.PHI{preds, block} => checkPhi(i, preds, block)
           | T.SOURCE{liveIn, block} => checkSource(i, liveIn, block)
           | T.SINK{liveOut, block} => checkSink(i, liveOut, block)
           | _ => checkOp i

       and checkPhi(i, preds, block) = 
           (checkBlock(i,block);
            checkDefs i;
            checkEdges i;
            let val n = length preds
                val m = length(A.sub(usesTbl, i))
            in  if m <> n then
                  bug(i, "|preds|="^i2s n^" |uses|="^i2s m)
                else ()
            end
           )

       and checkSource(i, liveIn, block) =
           (checkBlock(i, block);
            checkLiveIn(i, liveIn);
            if length(A.sub(usesTbl,i)) <> 0 then
               bug(i,"|uses| <> 0")
            else ();
            checkDefs i;
            checkEdges i
           )

       and checkSink(i, liveOut, block) =
           (checkBlock(i, block);
            checkLiveOut(i, liveOut);
            if length(A.sub(defsTbl,i)) <> 0 then
               bug(i,"|defs| <> 0")
            else ();
            if length(A.sub(succTbl,i)) <> 0 then
               (bug(i,"|succs| <> 0" );
                app printEdge (A.sub(succTbl,i))
               )
            else ();
            checkDefs i;
            checkEdges i
           )

       and checkOp(i) =
           (checkDefs i;
            checkEdges i
           )

   in  #forall_nodes ssa checkNode;
       if !hasError then error "SSA graph is corrupted" else ()
   end
end
