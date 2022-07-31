(*
 * SCC based global value numbering algorithm (L Taylor Simpson's algorithm)
 * 
 * -- Allen (leunga@cs.nyu.edu)
 *)

functor SSAGlobalValueNumbering
    (CF : SSA_CONSTANT_FOLDING) : SSA_GLOBAL_VALUE_NUMBERING =
struct

   structure SSA  = CF.SSA
   structure CF   = CF
   structure SP   = SSA.SP
   structure CFG  = SSA.CFG
   structure Dom  = SSA.Dom
   structure I    = SSA.I
   structure RTL  = SSA.RTL
   structure T    = RTL.T
   structure G    = Graph
   structure A    = Array
   structure H    = HashTable

   fun error msg = MLRiscErrorMsg.error("SSAGlobalValueNumbering",msg) 

   val top = CF.top

   val dump = MLRiscControl.getFlag "ssa-dump-value-numbers"
   (*
    * SCC based value numbering/constant folding
    *)
   fun computeValueNumbers (SSA as G.GRAPH ssa) =
   let val CFG as G.GRAPH cfg = SSA.cfg SSA
       val Dom as G.GRAPH dom = SSA.dom SSA
       val {sources,phis,ops,sinks} = SSA.nodes SSA
       val N = #capacity ssa ()   (* number of instructions *)
       val M = #capacity cfg ()   (* control flow graph *)
       val V = SSA.maxVar SSA     (* number of variables *)
       val defsTbl    = SSA.defsTbl SSA
       val usesTbl    = SSA.usesTbl SSA
       val rtlTbl     = SSA.rtlTbl SSA
       val blockTbl   = SSA.blockTbl SSA
       val defSiteTbl = SSA.defSiteTbl SSA
       val showOp     = SSA.showOp SSA
       val showVal    = SSA.showVal SSA

         (* 
          * Table mapping variables -> value numbers 
          *)
       val VN = A.array(V,CF.top) (* value numbers *)
       val DomN = A.array(N,~1) (* dominator numbers *)
       val visited = BitSet.create M
       fun walk(b,n) =
           let fun number([],n) = n
                 | number(i::ops,n) = 
                      (A.update(DomN,i,n); number(ops,n+1))
               val n = number(A.sub(sources,b),n)
               val n = number(A.sub(phis,b),n)
               val n = number(A.sub(ops,b),n)
               val n = number(A.sub(sinks,b),n)
               fun walkSucc([],n) = n
                 | walkSucc((_,b',_)::es,n) = walkSucc(es,walk(b',n))
           in  walkSucc(#out_edges dom b,n) end

       val _ = walk(hd(#entries dom ()),0)

       exception NotFound
       val validTable       = CF.hashTable(V,NotFound)
       val optimisticTable  = CF.hashTable(V,NotFound)
       val validLookup      = H.lookup validTable
       val validInsert      = H.insert validTable
       val optimisticLookup = H.lookup optimisticTable
       val optimisticInsert = H.insert optimisticTable

       fun bad(T.PHI _,operands) = List.all (fn r => r = top) operands
         | bad(_,operands) = List.exists (fn r => r = top) operands

       fun check(e,operands) = 
          (if bad(e,operands) then
              print("Bad rtl: "^RTL.rtlToString e^" "^
                    String.concat(map (fn r => Int.toString r^" ") operands)
                    ^"\n")
           else (); 
           (e,operands))

        (* lookup value number; create new vn if not found *)
       val validSearch = CF.constantFolding SSA 
             (fn (e,operands,p,t) => 
                 validLookup(e,operands,p) handle NotFound =>
                     (validInsert((e,operands,p),t); t))
                     
       val optimisticSearch = CF.constantFolding SSA 
             (fn (e,operands,p,t) =>
                 optimisticLookup(e,operands,p) handle NotFound =>
                    (optimisticInsert((e,operands,p),t); t))
  
       fun dumpSCC ops = 
       let fun printVN(i,rtl) = 
           let fun pr(t) = 
               let val vn = A.sub(VN,t)
               in  if vn <> t then print(" VN="^showVal vn^"\n") else ()
               end
           in  print("\t("^Int.toString(A.sub(DomN,i))^") "^showOp i);
               case A.sub(defsTbl,i) of
                 [t] => pr t
               |  _ => ();
               print "\n"
           end
       in  print "SCC=\n"; 
           app printVN ops
       end

       fun dumpVN() = 
       let fun pr(r,vn) =
               if vn > top andalso vn <> r then 
               let val i = A.sub(defSiteTbl, r)
               in  print("VN["^showVal r^"] = "^showVal vn^" "^showOp i^"\n")
               end
               else ()
       in  A.appi pr (VN,0,NONE)
       end
 
         (* 
          * compute the fixpoint of an scc 
          *) 
       fun unique ts = app (fn t => A.update(VN,t,t)) ts

       fun isVolatile r = List.exists (fn r' => r' = r) SP.volatile

       val zeroR = case I.C.zeroReg I.C.GP of
                     SOME zeroR => zeroR
                   | NONE => CF.top 

       fun initSource(t,t') = 
       let fun init(t::ts,t'::ts') = 
               (A.update(VN,t,
                 if t = zeroR then CF.zero
                 else if isVolatile t' then CF.volatile 
                 else t); init(ts,ts'))
             | init _ = ()
       in  init(t,t') end

       fun processSCC (scc,()) =
       let fun init t = A.update(VN,t,top)
           fun inits [] = ()
             | inits (t::ts) = (init t; inits ts)
           fun initialize([],ops) = ops
             | initialize(i::is,ops) =
               let val i' = A.sub(rtlTbl, i)
                   val t = A.sub(defsTbl, i)
               in  case i' of
                      T.SOURCE{liveIn, ...} => initSource(t,liveIn) 
                   |  T.SINK _ => ()
                   |  T.COPY _ => inits t
                   |  T.PHI _ => inits t
                   |   _ => inits t;
                   initialize(is,(i,i')::ops)
               end

           val ops = initialize(scc,[])
           fun byDomN((i,_),(j,_)) = A.sub(DomN,i) > A.sub(DomN,j)
           val ops = ListMergeSort.sort byDomN ops

           fun loop([],look,more) = more
             | loop((_,T.SOURCE _)::ops,look,more) = loop(ops,look,more)
             | loop((_,T.SINK _)::ops,look,more) = loop(ops,look,more)
             | loop((i,T.COPY _)::ops, look,more) = 
                 loop(ops,look,
                    processCopy(A.sub(defsTbl,i),A.sub(usesTbl,i),more))
             | loop((i,e)::ops,look,more) = 
                  loop(ops,look,
                     process(look,e,A.sub(defsTbl,i),A.sub(usesTbl,i),more))

           and compute_vn [] = []
             | compute_vn (r::rs) = 
                 (if r < 0 then r else A.sub(VN,r))::compute_vn rs

           and processOne(look,e,t,vns,p,changed) = 
               let val n = look(e, vns, p, t)
               in  (* if RTL.isConditionalBranch e then
                      print(RTL.rtlToString e^" vn="^Int.toString n^"\n")
                   else (); *)
                   if A.sub(VN,t) = n then changed
                   else (A.update(VN,t,n); true)
               end
 
           and process(look,e,ts,ss,changed) =
               let val vns = compute_vn ss
                   fun processIth([],p,changed) = changed
                     | processIth(t::ts,p,changed) = 
                       processIth(ts, p+1, processOne(look,e,t,vns,p,changed))
               in  processIth(ts,0,changed)
               end

           and processCopy(t,s,changed) =
               let val vn = map (fn r => A.sub(VN,r)) s
                   fun update(t::ts,vn::vns,changed) =
                       if A.sub(VN,t) = vn then update(ts,vns,changed)
                       else (A.update(VN,t,vn); update(ts,vns,true))
                     | update(_,_,changed) = changed
               in  update(t,vn,changed) end

       in  case ops of
             [i] => (loop(ops,validSearch,false); (* dumpSCC ops; *) ())
           | _   => let fun iterate count =
                            if loop(ops,optimisticSearch,false) then
                               iterate(count+1)
                            else count
                        val count = iterate 1
                    in  (* dumpSCC ops;
                        print("["^Int.toString(length ops)^":"^
                                  Int.toString(count)^"]"); *)
                        loop(ops,validSearch,false); ()
                    end
       end

       (*
        * Initialize all value numbers
        *)
       fun initializeValueNumbers() =
       let val ENTRY = hd(#entries dom ())
           fun init s = 
               let val T.SOURCE{block,...} = A.sub(rtlTbl,s)
                   val t = A.sub(defsTbl,s)
               in  unique t; 
                   if block = ENTRY then app initEdge(#out_edges ssa s) else ()
               end
           and initEdge(_,_,r) = A.update(VN,r,r)
       in  app init (#entries ssa ());
           case I.C.zeroReg I.C.GP of
             SOME zeroR => A.update(VN,zeroR,CF.zero) 
           | NONE => ()
       end
          
   in  initializeValueNumbers();
       GraphSCC.scc (ReversedGraphView.rev_view SSA) processSCC ();
       if !dump then dumpVN() else ();
       VN
   end

end
