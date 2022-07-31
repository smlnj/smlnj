(* Conditional constant propagation.
 * This module performs the conditional constant propagation analysis,
 * but does not update the SSA graph.
 *
 * -- Allen (leunga@cs.nyu.edu)
 *)

functor SSACondConstProp(CF  : SSA_CONSTANT_FOLDING)
     : SSA_CONDITIONAL_CONSTANT_PROPAGATION =
struct

   structure CF  = CF
   structure SSA = CF.SSA
   structure RTL = SSA.RTL
   structure T   = RTL.T
   structure G   = Graph
   structure A   = Array
   structure W8A = Word8Array

   type valueMap = CF.valnum Array.array 

   fun error msg = MLRiscErrorMsg.error("SSACondConstProp",msg)

   fun condConstProp(SSA as G.GRAPH ssa) = 
   let val CFG as G.GRAPH cfg = SSA.cfg SSA
       val N          = #capacity cfg ()
       val M          = #capacity ssa ()
       val {ops, phis, sources, ...} = SSA.nodes SSA
       val defSiteTbl = SSA.defSiteTbl SSA
       val usesTbl    = SSA.usesTbl SSA
       val defsTbl    = SSA.defsTbl SSA
       val rtlTbl     = SSA.rtlTbl SSA
       val blockTbl   = SSA.blockTbl SSA
       val reachable  = W8A.array(N,0w0) (* blocks that are reachable *)
       val onWorkList = W8A.array(M,0w0) (* ssa ops that are on work list *)
       val bot        = CF.bot
       val top        = CF.top 
       val nodes      = SSA.nodes SSA
       val V          = SSA.maxVar SSA
       val values     = A.array(V,bot) (* current values *)

       (* Add value v onto the worklist *)
       fun addWL(v,WL) =
           let val i = A.sub(defSiteTbl,v)
           in  if W8A.sub(onWorkList,i) <> 0w0 then WL 
               else (W8A.update(onWorkList,i,0w1); i::WL)
           end

       (* Add all operations onto the worklist *)
       fun addWLs([],WL) = WL
         | addWLs(i::ops,WL) = 
            (W8A.update(onWorkList,i,0w1); addWLs(ops, i::WL))

       (* Constant folding function *)
       val fold = CF.constantFolding SSA (fn _ => top)

       (* Perform propagation on the worklist *)
       fun propagate [] = ()
         | propagate(i::WL) =
              (W8A.update(onWorkList,i,0w0);
               propagate(eval(i,A.sub(rtlTbl,i),
                              A.sub(defsTbl,i),A.sub(usesTbl,i),WL)))

       (* Evaluate an SSA node *)
       and eval(i,T.PHI _,[t],s,WL) =
           let fun join([],v) = v
                 | join(s::ss,v) = 
                   let val x = A.sub(values,s)
                   in  if x = bot then join(ss,v)
                       else if v = bot then join(ss,x)
                       else if x = v then join(ss,v)
                       else top
                   end
           in  update(t,join(s,bot),WL) 
           end
         | eval(i,T.SOURCE _,t,_,WL) = updateTops(t,WL)
         | eval(i,T.SINK _,_,_,WL) = WL
         | eval(i,i',s,t,WL) =
           let fun getVal v = if v >= 0 then A.sub(values, v) else v
               val v = fold(i',map getVal s,0,())
               val b = A.sub(blockTbl,i)  
           in  case t of
                 [t] => let val WL = update(t,v,WL)
                            fun chase e =
                            case (e,v) of 
                              (T.IF _,~1) => enableSucc(b,false,WL)
                            | (T.IF _,~2) => enableSucc(b,true,WL)
                            | (T.IF _,_)  => enableAllSucc(b,WL)
                            | (T.JMP _,_)  => enableAllSucc(b,WL)
                            | (T.RTL{e,...},_) => chase e
                            | _ => WL
                        in  chase i' 
                        end
               | _   => updateTops(t,WL)
           end  

       (* Update the result *)
       and update(t,v,WL) =
           if A.sub(values,t) = v then WL
           else (A.update(values,t,v); addWL(t,WL))

       (* Update top to all results *)
       and updateTops([],WL) = WL
         | updateTops(t::ts,WL) = 
           if A.sub(values,t) = top then updateTops(ts,WL)
           else (A.update(values,t,top); updateTops(ts,addWL(t,WL)))

       (* Mark a basic block as reachable *)
       and enableNode(b,WL) =
           if W8A.sub(reachable,b) <> 0w0 then WL
           else 
           let val _ = W8A.update(reachable,b,0w1)
               val WL = addWLs(A.sub(ops,b), WL)
               val WL = addWLs(A.sub(phis,b), WL)
               val WL = addWLs(A.sub(sources,b), WL)
           in  case #out_edges cfg b of
                 [(_,b',_)] => enableNode(b',WL)
               |  _ => WL
           end

       (* Mark an edge as reachable *)
       and enableEdge((_,b,_),WL) = enableNode(b,WL)

       (* Mark a successor of b as reachable *)
       and enableSucc(b,cond,WL) =
             foldr (fn ((_,b',_),WL) => enableNode(b',WL)) WL (#out_edges cfg b)
              
       (* Mark all successors of b as reachable *)
       and enableAllSucc(b,WL) = foldr enableEdge WL (#out_edges cfg b)

       (* Initialize the table *)
       fun init() = 
           (case SSA.C.zeroReg SSA.C.GP of
              SOME zeroR => A.update(values, zeroR, CF.zero)
            | NONE => ()
           )

       (* Constant propagation main driver *)
       fun constantPropagation() = 
           (init();
            propagate(enableAllSucc(hd(#entries cfg ()),[]))
           )
   in  constantPropagation();
       values        
   end

end
