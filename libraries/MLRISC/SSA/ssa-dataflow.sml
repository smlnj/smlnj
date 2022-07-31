(* 
 * This is a generic module for performing (forward) dataflow 
 * analysis on the SSA graph.  It behavior is somewhat parameterizable.
 *
 * -- Allen (leunga@cs.nyu.edu)
 *)

functor SSADataflow(SSA : SSA) : SSA_DATAFLOW_ANALYSIS =
struct

   structure SSA = SSA
   structure RTL = SSA.RTL
   structure C   = SSA.C
   structure T   = RTL.T
   structure G   = Graph
   structure A   = Array
   structure W8A = Word8Array

   fun error msg = MLRiscErrorMsg.error("SSADataflowAnalysis",msg)

   fun dataflowAnalysis
       {ssa=SSA as G.GRAPH ssa, meet, bot, top, ==, transfer, const } = 
   let val M          = #capacity ssa ()
       val values     = A.array(M, bot) (* flow values *)
       val onWorkList = W8A.array(M, 0w1) (* initially everything in on WL *)
       val defsTbl    = SSA.defsTbl SSA
       val usesTbl    = SSA.usesTbl SSA
       val rtlTbl     = SSA.rtlTbl SSA
       val succTbl    = SSA.succTbl SSA
       val defSiteTbl = SSA.defSiteTbl SSA

       fun valueOf v = if v < 0 then const v else A.sub(values, v)

       val zeroR = case C.zeroReg C.GP of SOME r => r | NONE => ~1

       fun iterate [] = () (* done *)
         | iterate(i::WL) =  
           let val _ = W8A.update(onWorkList, i, 0w0)
               val uses = A.sub(usesTbl, i)
               val defs = A.sub(defsTbl, i)
               val rtl  = A.sub(rtlTbl, i)
               val inputs = map valueOf uses
           in  case rtl of
                 T.PHI _ => phi(i, inputs, defs, WL)
               | T.SOURCE{liveIn, ...} => source(i, defs, liveIn, WL)
               | T.SINK _ => iterate WL
               |  _ =>
                 let val outputs = transfer{rtl=rtl, inputs=inputs, defs=defs}
                 in  process(i, defs, outputs, WL, false) 
                 end
           end

       and phi(i, inputs, [t], WL) = 
           let val old = A.sub(values, t)
               val new = meet inputs
           in  if ==(old,new) then iterate WL
               else (A.update(values, t, new); propagate(i, WL))
           end
         | phi _ = error "phi"

       and source(i, [], [], WL) = propagate(i, WL)
         | source(i, v::vs, r::rs, WL) =
           (A.update(values, v, if r = zeroR then const ~1 else top);   
                (* XXX *)
            source(i, vs, rs, WL)
           )
         | source _ = error "source"

       and process(i, [], [], WL, affected) = 
             if affected then propagate(i, WL) else iterate WL
         | process(i, v::vs, d::ds, WL, affected) =
           let val old = A.sub(values, v)
           in  if ==(old,d) then process(i, vs, ds, WL, affected)
               else (A.update(values, v, d);
                     process(i, vs, ds, WL, true)
                    )
           end
         | process _ = error "process"

       and propagate(i, WL) = insert(A.sub(succTbl, i), WL)

       and insert([], WL) = iterate WL
         | insert((_,j,_)::es, WL) = 
             if W8A.sub(onWorkList, j) <> 0w0 then insert(es, WL)
             else (W8A.update(onWorkList, j, 0w1); insert(es, j::WL))
   
       val ops = map #1 (#nodes ssa ())

   in  iterate ops;
       values        
   end

end
