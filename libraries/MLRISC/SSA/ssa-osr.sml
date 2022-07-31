(* 
 * Operator Strength Reduction.
 *
 * -- Allen (leunga@cs.nyu.edu)
 *)
functor SSAOSR(CF : SSA_CONSTANT_FOLDING) : SSA_OPTIMIZATION =
struct
   structure SSA = CF.SSA
   structure Dom = SSA.Dom
   structure I   = SSA.I
   structure RTL = SSA.RTL
   structure T   = RTL.T
   structure G   = Graph
   structure A   = Array
   structure DA  = DynArray
   structure H   = HashTable

   type flowgraph = SSA.ssa

   fun error msg = MLRiscErrorMsg.error("SSAOperatorStrengthReduction",msg)

   val name = "Operator Strength Reduction"

   fun run(SSA as G.GRAPH ssa) = SSA
   (*
   let val Dom as G.GRAPH dom = SSA.dom SSA
       val sdom               = Dom.strictly_dominates Dom
       val replaceAllUses     = SSA.replaceAllUses SSA
       val showOp             = SSA.showOp SSA
       val N                  = #capacity ssa ()
       val V                  = SSA.maxVar SSA
       val defSiteTbl         = SSA.defSiteTbl SSA
       val defsTbl            = SSA.defsTbl SSA
       val usesTbl            = SSA.usesTbl SSA
       val rtlTbl             = SSA.rtlTbl SSA
       val blockTbl           = SSA.blockTbl SSA

       val headers = DA.array(V,~1)    (* headers of instructions *)
                        (* ~1 if it is not an induction variable *)
       val inSCC   = DA.array(N,~1)    (* is the instruction in the SCC? *)

       exception NotThere

       val table  = CF.hashTable(13,NotThere)
       val search = H.lookup table
       val insert = H.insert table
       val inventName = I.C.newCell I.C.GP

       fun add(e,operands,name) = insert((e,operands,0),name)

       (* Check whether value x is a region constant *)
       fun isRegionConstant(x,header) =
           x < 0 orelse sdom(A.sub(blockTbl,A.sub(defSiteTbl,x)),header)

       (* Copy an instruction *)
       fun copyDef'(SSA.OP{b,i,e,s,p,...},t) = SSA.OP{b=b,i=i,e=e,s=s,p=p,t=[t]}
         | copyDef'(SSA.PHI{b,s,t',preds,...},t) = 
             SSA.PHI{b=b,t'=t',s=s,preds=preds,t=t}
         | copyDef' _ = error "copyDef'"

       fun copyDef(x,t) = 
           let val i' = node_info x
               val i = #new_id ssa ()
           in  #add_node ssa (i,copyDef'(i',t)); 
               (i,i') 
           end

       (* Process each scc *)
       fun processSCC ([],_)  = ()
         | processSCC ([n],_) = strengthReduce(n,node_info n)
         | processSCC (scc as witness::_,_) =
           let (* find the header block of the SCC *)
               fun findHeader([],scc,h) = (scc,h)
                 | findHeader(i::ops,scc,h) =
                     let val i' = node_info i
                     in  DA.update(inSCC,i,witness);
                         findHeader(ops,(i,i')::scc,
                            case i' of
                               SSA.PHI{b,...} => if h = ~1 orelse sdom(b,h) 
                                                 then b else h
                            |  _ => h
                         )
                     end 
               val (scc,header) = findHeader(scc,[],~1)

               (* Check whether the scc is an inductive variable *)
               fun isIVSCC [] = true
                 | isIVSCC ((_,i')::ops) = isIVOp i' andalso isIVSCC ops

               (* is the operation a legal inductive cycle? *)
               and isIVOp(SSA.PHI{s,...}) = List.all isIVorRC s
                 | isIVOp(SSA.OP{e=T.COPY _,s=[s],...}) = isIVorRC s
                 | isIVOp(SSA.OP{e=T.RTL{e, ...}, s=[a,b], ...}) = 
                   let fun f(T.MV(_,_,T.ADD(_,T.REG(_,0) ,T.REG(_,1)))) =
                             isIVRC(a,b) orelse isIVRC(b,a)
                         | f(T.MV(_,_,T.ADD(_,T.REG(_,1) ,T.REG(_,0)))) =
                             isIVRC(a,b) orelse isIVRC(b,a)
                         | f(T.MV(_,_,T.ADDT(_,T.REG(_,0) ,T.REG(_,1)))) =
                             isIVRC(a,b) orelse isIVRC(b,a)
                         | f(T.MV(_,_,T.ADDT(_,T.REG(_,1) ,T.REG(_,0)))) =
                             isIVRC(a,b) orelse isIVRC(b,a)
                         | f(T.MV(_,_,T.SUB(_,T.REG(_,0),T.REG(_,1)))) =
                             isIVRC(a,b)
                         | f(T.MV(_,_,T.SUB(_,T.REG(_,1),T.REG(_,0)))) =
                             isIVRC(b,a)
                         | f(T.MV(_,_,T.SUBT(_,T.REG(_,0),T.REG(_,1)))) =
                             isIVRC(a,b)
                         | f(T.MV(_,_,T.SUBT(_,T.REG(_,1),T.REG(_,0)))) =
                             isIVRC(b,a)
                         | f _ = false
                   in f e end
                 | isIVOp _ = false
               and isIV x = x >= 0 andalso DA.sub(inSCC,x) = witness 
               and isRC x = isRegionConstant(x,header)
               and isIVRC(a,b) = isIV a andalso isRC b
               and isIVorRC x = isIV x orelse isRC x

               fun dumpSCC(title,scc) =
                   (print(title^"="^Int.toString header^"\n");
                     app (fn (_,i) => print("\t"^showOp i^"\n")) scc)

           in  if isIVSCC scc then
                  (* found an induction variable *)
                  let fun mark t = DA.update(headers,t,header)
                  in  dumpSCC("IV",scc);
                      app (fn (_,SSA.OP{t,...}) => app mark t
                            | (_,SSA.PHI{t,...}) => mark t
                            | _ => error "headers") scc
                  end
               else
                  (app strengthReduce scc)
           end

           (* perform strength reduction *)
       and strengthReduce(n,n' as SSA.OP{e,t=[t],s=[a,b],...}) =
           (case isInReducibleForm(e,a,b) of
               SOME(iv,rc) => replace(t,e,iv,rc) 
            |  NONE => ())
         | strengthReduce _ = ()

           (* Check whether an instruction is in reducible form *)
       and isInReducibleForm(rtl,a,b) =
           let fun isIVRC(a,b) =
                   a >= 0 andalso
                   let val header_a = DA.sub(headers,a)
                   in  header_a <> ~1 andalso isRegionConstant(b,header_a)
                   end 
               fun addOrMul() = 
                   if isIVRC(a,b) then SOME(a,b)
                   else if isIVRC(b,a) then SOME(b,a)
                   else NONE
               fun sub() =  
                   if isIVRC(a,b) then SOME(a,b) else NONE
               fun reducible rtl =
               case rtl of
                 T.MV(_,_,T.ADD(_,T.REG(_,0),T.REG(_,1))) => addOrMul()
               | T.MV(_,_,T.ADDT(_,T.REG(_,0),T.REG(_,1))) => addOrMul()
               | T.MV(_,_,T.MULS(_,T.REG(_,0),T.REG(_,1))) => addOrMul()
               | T.MV(_,_,T.MULU(_,T.REG(_,0),T.REG(_,1))) => addOrMul()
               | T.MV(_,_,T.MULT(_,T.REG(_,0),T.REG(_,1))) => addOrMul()
               | T.MV(_,_,T.SUB(_,T.REG(_,0),T.REG(_,1))) => sub()
               | T.MV(_,_,T.SUBT(_,T.REG(_,0),T.REG(_,1))) => sub()
               | T.RTL{e, ...} => reducible e
               | _ => NONE
           in  reducible rtl 
           end

           (*
            * Replace the current operation with a copy from its 
            * reduced counterpart.
            *)
       and replace(t,e,iv,rc) = 
           let val t' = reduce(e,iv,rc)
           in  replaceAllUses{from=t,to=t'};
               DA.update(headers,t,DA.sub(headers,iv))
           end

           (*
            * Insert code to strength reduce an induction variable
            * and return the SSA name of the result
            *)
       and reduce(e,iv,rc) =
           let val operands = [iv,rc]
           in  search(e,operands,0)
               handle _ =>
               let val result = inventName()
                   val _ = add(e,operands,result)
                   val (newDef,_) = copyDef(iv,result)
                   val iv_header = DA.sub(headers,iv)
                   val _ = DA.update(headers,result,iv_header)
                   fun isMult(T.RTL{e, ...}) = isMult e
                     | isMult(T.MV(_,_,T.MULS(_,T.REG(_,0),T.REG(_,1)))) = true
                     | isMult(T.MV(_,_,T.MULU(_,T.REG(_,0),T.REG(_,1)))) = true
                     | isMult(T.MV(_,_,T.MULT(_,T.REG(_,0),T.REG(_,1)))) = true
                     | isMult _ = false
                   fun doOperand r =
                      if r >= 0 andalso DA.sub(headers,r) = iv_header then
                         (replaceAllUses{from=r,to=reduce(e,r,rc)};())
                      else if r >= 0 orelse isMult e andalso
                              (case node_info (defSite r) of
                                  SSA.PHI _ => true
                               |  _ => false) then
                         (replaceAllUses{from=r,to=apply(e,r,rc)}; ())
                      else ()
               in  app doOperand operands;
                   result
               end 
           end
                             
       and apply(e,op1,op2) = 
           let val operands = [op1,op2]
           in  search(e,operands,0)
               handle _ =>
                  if op1 < 0 orelse
                     let val header_op1 = DA.sub(headers,op1)
                     in  header_op1 <> ~1 andalso 
                         isRegionConstant(op2,header_op1)
                     end then
                     reduce(e,op1,op2)
                  else if op2 < 0 orelse 
                     let val header_op2 = DA.sub(headers,op2)
                     in  header_op2 <> ~1 andalso 
                         isRegionConstant(op1,header_op2)
                     end then
                     reduce(e,op2,op1)
                  else 
                     let val result = inventName()
                         val _ = add(e,operands,result)
                     in  result 
                     end
           end
   
   in  (* process all loops *)
       GraphSCC.strong_components(ReversedGraphView.rev_view SSA) processSCC ();
       SSA.changed SSA;
       SSA
   end   
   *)
   
end
