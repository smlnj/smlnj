(*
 * Annotate GC safety-invariants before performing SSA optimizations
 *
 * -- Allen (leunga@cs.nyu.edu)
 *)
functor SSAGCInvariants
   (structure SSA     : SSA
    structure TypeSys : GC_TYPE_SYSTEM
      sharing SSA.RTL = TypeSys.RTL 
   ) : SSA_OPTIMIZATION =
struct
   structure SSA   = SSA
   structure CFG   = SSA.CFG
   structure I     = SSA.I
   structure C     = I.C
   structure RTL   = SSA.RTL
   structure T     = RTL.T
   structure GCMap = TypeSys.GCMap
   structure GC    = GCMap.GC
   structure G     = Graph
   structure A     = Array

   type flowgraph = SSA.ssa

   val name = "GC Safety-Invariants"

   val debug = MLRiscControl.getFlag "ssa-debug-gc"

   fun error msg = MLRiscErrorMsg.error("SSAGCInvariants",msg)

   fun run SSA =
   let val CFG = SSA.cfg SSA
   in  case #get GCMap.GCMAP (!(CFG.annotations CFG)) of
         NONE => SSA
       | SOME gcmap => doTheWork(SSA, CFG, gcmap)
   end

   and doTheWork(SSA, CFG, gcmap) =
   let val G.GRAPH ssa = SSA
       val V           = SSA.maxVar SSA   (* maximum encoding of variables *)
       val N           = #capacity ssa ()
       val const       = SSA.const SSA    (* constants map *)
       val defSiteTbl  = SSA.defSiteTbl SSA  (* definition site *)
       val cellKindMap = SSA.cellKindTbl SSA (* cellKind map *)
       val defsTbl     = SSA.defsTbl SSA
       val usesTbl     = SSA.usesTbl SSA
       val rtlTbl      = SSA.rtlTbl SSA
       val cellKind    = IntHashTable.find cellKindMap
       val cellKind    = fn r => case cellKind r of SOME k => k | NONE => C.GP
       val updateTy    = IntHashTable.insert gcmap
       val zeroR       = case C.zeroReg C.GP of 
                           SOME z => z
                         | NONE => ~1

       val gcTypes     = A.array(V, GC.BOT)
       val onQueue     = BitSet.create N
       val hasType     = BitSet.create V

       fun joins []      = GC.BOT
         | joins [x]     = x
         | joins (x::xs) = GC.join(x, joins xs)

       fun initializeTypes() =
           (IntHashTable.appi (fn (r,t) => 
               (BitSet.set(hasType,r); A.update(gcTypes, r, t))) gcmap;
            if zeroR >= 0 then A.update(gcTypes, zeroR, GC.CONST 0) else ()
           )

       fun enqueueInsn(j, WL) = 
           if BitSet.markAndTest(onQueue, j) then WL 
           else j::WL
 
       fun enqueue(r, WL) = 
       let val i = A.sub(defSiteTbl,r)
           fun ins([], WL) = WL
             | ins((_,j,_)::es, WL) = ins(es, enqueueInsn(j, WL))
       in  ins(#out_edges ssa i, WL) end

       fun update(r, t, WL) = 
           if r = zeroR orelse BitSet.contains(hasType, r) 
           then WL
           else let val t' = A.sub(gcTypes, r)
                in  if GC.==(t,t') then WL 
                    else 
                     ((* print("r"^Int.toString r^":"^GC.toString t^"\n");*)
                      A.update(gcTypes, r, t); 
                      enqueue(r, WL))
                end

       fun lookup r = A.sub(gcTypes, r)

       val effectOf = TypeSys.effectOf {lookup=lookup, update=update}

       fun iterate([]) = ()
         | iterate(i::WL) = 
           let val _  = BitSet.reset(onQueue,i)
           in  iterate(process(i, WL)) end

       and process(i,WL) = 
           let val rtl  = A.sub(rtlTbl,i) 
               val defs = A.sub(defsTbl,i)
               val uses = A.sub(usesTbl,i)
           in  case (rtl,defs,uses) of
                 (T.PHI _,[t],s) => phi(t,s,WL)
               | (T.SOURCE _,t,_) =>
                   let fun init([],WL) = WL
                         | init(t::ts,WL) = init(ts,setTop(t,WL))
                       and setTop(t,WL) = 
                           if BitSet.contains(hasType, t) then WL
                           else (update(t, GC.TOP, WL))
                   in  init(t, WL) end  
               | (T.SINK _, _, _) => WL
               | (e, t, s) =>
                 let fun lookupN n = 
                     let val r = List.nth(s,n) 
                     in  if r < 0 then GC.INT else A.sub(gcTypes,r) end
                     fun updateN(n, ty, WL) = update(List.nth(t,n), ty, WL)
                 in  TypeSys.effectOf{lookup=lookupN, update=updateN} 
                                     {action=e,src=s,dst=t,effect=WL}
                     handle e => (print("["^Int.toString i^"] "^
                                        SSA.showOp SSA i^"\n"); raise e)
                 end
           end

       and phi(t, s, WL) =
           if BitSet.contains(hasType, t) then WL
           else let val tys = map lookup s
                    val ty  = joins tys
                in  update(t, ty, WL) end

       fun updateTypes() =
           A.appi (fn (r,t) => if GC.==(t,GC.TOP) then () 
                               else if GC.==(t,GC.BOT) then ()
                               else updateTy(r,t)) (gcTypes, 0, NONE)

       fun typeInference() =
       let val WL = map #1 (#nodes ssa ())
       in  app (fn i => BitSet.set(onQueue,i)) WL;
           iterate WL
       end

       fun markConstraints() =
       let val G.GRAPH dom = SSA.dom SSA
           val {ops, ...} = SSA.nodes SSA
           val showOp = SSA.showOp SSA
           fun walk b =
           let fun markAsNonMovable i =
                 (if !debug then
                    print("Block "^Int.toString b^" can't move: "^
                          showOp i^"\n")
                  else ();
                  A.update(rtlTbl, i, RTL.pin(A.sub(rtlTbl, i)))
                 )
               fun mark(i) = 
                   let fun isRecoverable [] = true
                         | isRecoverable(t::ts) = 
                           (cellKind t = C.MEM orelse
                            cellKind t = C.CTRL orelse
                           TypeSys.isRecoverable(A.sub(gcTypes,t))) andalso
                           isRecoverable ts
                   in  case A.sub(rtlTbl,i) of
                          (T.PHI _ | T.SOURCE _ | T.SINK _) => ()
                       | _ =>
                         if isRecoverable (A.sub(defsTbl,i)) then () (* okay *)
                         else markAsNonMovable i
                   end
           in  app mark (A.sub(ops, b));
               app walk (#succ dom b)
           end
     
       in  walk(hd(#entries dom ()))
       end

   in  print "GC type inference\n";
       initializeTypes();
       typeInference();
       updateTypes();
       print "GC type inference done\n";
       markConstraints();
       SSA
   end

end
