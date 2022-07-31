(* 
 * Dead code elimination
 *
 * -- Allen (leunga@cs.nyu.edu)
 *)
functor SSADCE(SSA : SSA) : SSA_OPTIMIZATION =
struct
   structure SSA  = SSA
   structure SP   = SSA.SP
   structure RTL  = SSA.RTL
   structure T    = RTL.T
   structure G    = Graph 
   structure W8A  = Word8Array
   structure A    = Array
  
   type flowgraph = SSA.ssa

   fun error msg = MLRiscErrorMsg.error("SSADCE",msg)

   val codeRemoved   = MLRiscControl.getCounter "ssa-dead-instructions-removed"
   val copiesRemoved = MLRiscControl.getCounter "ssa-dead-copies-removed"

   val name = "Dead Code Elimination"

   fun run(SSA as G.GRAPH ssa) = 
   let val CFG as G.GRAPH cfg = SSA.cfg SSA
       val live      = W8A.array(#capacity ssa (), 0w0) (* live instr? *) 
       val liveBlock = W8A.array(#capacity cfg (), 0w0) (* live block? *)
       val liveVar   = W8A.array(SSA.maxVar SSA, 0w0)   (* live variable? *)

       val defSiteTbl= SSA.defSiteTbl SSA
       val defsTbl   = SSA.defsTbl SSA
       val usesTbl   = SSA.usesTbl SSA
       val rtlTbl    = SSA.rtlTbl SSA
       val blockTbl  = SSA.blockTbl SSA

       val showOp    = SSA.showOp SSA
       val oldCode   = !codeRemoved
       val oldCopies = !copiesRemoved

           (* mark all reachable blocks *)
       fun markBlock(b) =
           if W8A.sub(liveBlock,b) <> 0w0 then ()
           else (W8A.update(liveBlock,b,0w1);
                 app (fn (_,b,_) => markBlock b) (#out_edges cfg b))

       val _ = app markBlock (#entries cfg ())

           (* Mark ssa op i as live.
            * The instruction is live only if it is in a block that is live.
            *)
       fun mark i = 
           if W8A.sub(live,i) <> 0w0 then () 
           else if W8A.sub(liveBlock,A.sub(blockTbl, i)) <> 0w0 then 
              (W8A.update(live,i,0w1); markSrcs(A.sub(usesTbl, i)))
           else ()

           (* Mark all registers as live *)
       and markSrcs [] = ()
         | markSrcs(r::rs) = (markSrc r; markSrcs rs)

           (* Mark register r as live *)
       and markSrc r =
           if r < 0 orelse W8A.sub(liveVar,r) <> 0w0 then () else
           let val _   = W8A.update(liveVar,r,0w1)
               val i   = A.sub(defSiteTbl,r)  (* r is defined in i *)
               val rtl = A.sub(rtlTbl, i)
           in  markDependents(i,r,rtl) end

           (* Mark all dependents in instruction i (which defines r) 
            * For copies, only register s in parallel copy r <- s in live
            * For other instructions, every input is live
            *)
       and markDependents(i,r,T.COPY _) = 
           let fun find(t::ts,s::ss) = 
                   if r = t then markSrc s else find(ts,ss)
                 | find _ = error "markDependents"
               val b = A.sub(blockTbl,i)
               val s = A.sub(usesTbl,i)
               val t = A.sub(defsTbl,i)
           in  if W8A.sub(liveBlock,b) <> 0w0 then 
                  (W8A.update(live,i,0w1); find(t,s)) else ()
           end
         | markDependents(i,r,_) = mark i

           (* 
            * All control flow instructions, and stores are not removed 
            * for now, since memory dependence analysis is flaky.
            *)
       fun findRoots() =
           let fun markRoot i = 
                   if RTL.can'tBeRemoved (A.sub(rtlTbl,i)) then 
                       ((* print("Root: "^showOp i^"\n"); *) mark i)
                   else ()
           in  SSA.forallNodes SSA markRoot end

      fun removeDeadCode() =
      let fun kill n = 
              (codeRemoved := !codeRemoved +1; 
               (* print("SSA DCE: removing "^showOp n^"\n"); *)
               #remove_node ssa n 
              )
      in  SSA.forallNodes SSA 
          (fn n => 
           if W8A.sub(live,n) <> 0w0 then 
               (*
              (case A.sub(rtlTbl,n) of
                 (* simplify partially-dead parallel copies *)
                 T.COPY,
                 let fun simplify(t::ts,s::ss,d::ds,u::us,
                                  ts',ss',ds',us',ch) =
                         if W8A.sub(liveVar,t) <> 0w0 then 
                            simplify(ts,ss,ds,us,
                                     t::ts',s::ss',d::ds',u::us',true)
                         else
                            (copiesRemoved := !copiesRemoved + 1;
                             simplify(ts,ss,ds,us,ts',ss',ds',us',ch))
                       | simplify([],[],[],[],ts',ss',ds',us',ch) = 
                               (ts',ss',ds',us',ch) 
                       | simplify _ = error "simplify"
                     val (defs,uses) = getOperands i
                     val t = A.sub(defsTbl,i)
                     val s = A.sub(usesTbl,i)
                     val (t,s,dst,src,ch) = simplify(t,s,defs,uses,
                                                   [],[],[],[],false)
                 in  case t of
                        [] => kill n
                     |  _  => 
                     if ch then
                     let (* val i = SP.copy{instr=i,dst=dst,src=src} *)
                         val ssa_op = SSA.OP{e=RTL.COPY,i=i,s=s,t=t,p=p,b=b}
                     in #add_node ssa (n,ssa_op) end
                     else ()
                 end
               |  _ => ()
              ) *) ()
           else kill n)
      end

      val _ = findRoots()
      val _ = removeDeadCode()
   in if !codeRemoved <> oldCode orelse
         !copiesRemoved <> oldCopies then 
         (#set_exits ssa (List.filter (#has_node ssa) (#exits ssa ()));
          SSA.changed SSA
         )
      else ();
      SSA
   end
end
 
