(* cfg.sml
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies
 *
 * The control flow graph representation used for optimizations.
 *
 * -- Allen
 *)

functor ControlFlowGraph
   (structure I : INSTRUCTIONS
    structure GraphImpl : GRAPH_IMPLEMENTATION
    structure InsnProps : INSN_PROPERTIES where I = I
    structure Asm : INSTRUCTION_EMITTER where I = I
   ) : CONTROL_FLOW_GRAPH =
struct

    structure I = I
    structure P = Asm.S.P
    structure C = I.C
    structure G = Graph
    structure S = Asm.S
    structure A = Array
    structure H = IntHashTable

    type weight = real

    datatype block_kind =
        START          (* entry node *)
      | STOP           (* exit node *)
      | NORMAL         (* normal node *)

    and block =
       BLOCK of
       {  id          : int,                        (* block id *)
          kind        : block_kind,                 (* block kind *)
          freq        : weight ref,                 (* execution frequency *)
          labels      : Label.label list ref,       (* labels on blocks *)
          insns       : I.instruction list ref,     (* in rev order *)
	  align	      : P.pseudo_op option ref,	    (* alignment only *)
          annotations : Annotations.annotations ref (* annotations *)
       }

    and edge_kind	    (* edge kinds (see cfg.sig for more info) *)
      = ENTRY			(* entry edge *)
      | EXIT            	(* exit edge *)
      | JUMP			(* unconditional jump *)
      | FALLSTHRU		(* falls through to next block *)
      | BRANCH of bool		(* branch *)
      | SWITCH of int		(* computed goto *)
      | FLOWSTO			(* FLOW_TO edge *)

    and edge_info = EDGE of {
	k : edge_kind,                  (* edge kind *)
	w : weight ref,                 (* edge freq *)
	a : Annotations.annotations ref (* annotations *)
      }

    type edge = edge_info Graph.edge
    type node = block Graph.node

    datatype info =
        INFO of { annotations : Annotations.annotations ref,
                  firstBlock  : int ref,
                  reorder     : bool ref,
		  data        : P.pseudo_op list ref,
		  decls	      : P.pseudo_op list ref
                }

    type cfg = (block,edge_info,info) Graph.graph

    fun error msg = MLRiscErrorMsg.error("ControlFlowGraph",msg)

   (*========================================================================
    *
    *  Various kinds of annotations
    *
    *========================================================================*)
              (* escaping live out information *)
    val LIVEOUT = Annotations.new
          (SOME(fn c => "Liveout: "^
                        (LineBreak.lineBreak 75
                            (CellsBasis.CellSet.toString c))))
    exception Changed of string * (unit -> unit)
    val CHANGED = Annotations.new'
          {create=Changed,
           get=fn Changed x => x | e => raise e,
           toString=fn (name,_) => "CHANGED:"^name
          }

   (*========================================================================
    *
    *  Methods for manipulating basic blocks
    *
    *========================================================================*)
    fun defineLabel(BLOCK{labels=ref(l::_),...}) = l
      | defineLabel(BLOCK{labels, ...}) = let
	  val l = Label.anon ()
          in
	    labels := [l];
	    l
	  end
    fun insns(BLOCK{insns, ...}) = insns
    fun freq(BLOCK{freq, ...}) = freq
    fun edgeFreq(_,_,EDGE{w, ...}) = w
    fun sumEdgeFreqs es = foldr (fn (e,w) => !(edgeFreq e) + w) 0.0 es

    fun newBlock'(id,kind,insns,freq) =
        BLOCK{ id          = id,
               kind        = kind,
               freq        = freq,
               labels      = ref [],
               insns       = ref insns,
	       align       = ref NONE,
               annotations = ref []
             }

    fun copyBlock(id,BLOCK{kind,freq,align,labels,insns,annotations,...}) =
        BLOCK{ id          = id,
               kind        = kind,
               freq        = ref (!freq),
               labels      = ref [],
	       align	   = ref (!align),
               insns       = ref (!insns),
               annotations = ref (!annotations)
             }

    fun newBlock(id,freq) = newBlock'(id,NORMAL,[],freq)
    fun newStart(id,freq) = newBlock'(id,START,[],freq)
    fun newStop(id,freq) = newBlock'(id,STOP,[],freq)

    fun newNode (G.GRAPH graph) wt = let
	  val id = #new_id graph ()
	  val nd = (id, newBlock (id, ref wt))
	  in
	    #add_node graph nd;
	    nd
	  end

    fun branchOf(EDGE{k=BRANCH b,...}) = SOME b
      | branchOf _ = NONE
    fun edgeDir(_,_,e) = branchOf e

   (*========================================================================
    *
    *  Emit a basic block
    *
    *========================================================================*)
    fun kindName START          = "START"
      | kindName STOP           = "STOP"
      | kindName NORMAL         = "Block"

    fun nl() = TextIO.output(!AsmStream.asmOutStream,"\n")

    fun emitHeader (S.STREAM{comment,annotation,...})
                   (BLOCK{id,kind,freq,annotations,...}) =
       (comment(kindName kind ^"["^Int.toString id^
                    "] ("^Real.toString (!freq)^")");
        nl();
        app annotation (!annotations)
       )

    fun emitFooter (S.STREAM{comment,...}) (BLOCK{annotations,...}) =
        (case #get LIVEOUT (!annotations) of
            SOME s =>
            let val regs = String.tokens Char.isSpace(CellsBasis.CellSet.toString s)
                val K = 7
                fun f(_,[],s,l)    = s::l
                  | f(0,vs,s,l)    = f(K,vs,"   ",s::l)
                  | f(n,[v],s,l)   = v^s::l
                  | f(n,v::vs,s,l) = f(n-1,vs,s^" "^v,l)
                val text = rev(f(K,regs,"",[]))
            in  app (fn c => (comment c; nl())) text
            end
         |  NONE => ()
        ) handle Overflow => print("Bad footer\n")

    fun emitStuff outline annotations
           (block as BLOCK{insns,labels,...}) =
       let val S as S.STREAM{pseudoOp,defineLabel,emit,...} =
               Asm.makeStream annotations
       in  emitHeader S block;
           app defineLabel (!labels);
           if outline then () else app emit (rev (!insns));
           emitFooter S block
       end

    val emit = emitStuff false
    val emitOutline = emitStuff true []

   (*========================================================================
    *
    *  Methods for manipulating CFG
    *
    *========================================================================*)
    fun cfg info = GraphImpl.graph("CFG",info,10)
    fun new() =
        let val info = INFO{ annotations = ref [],
                             firstBlock  = ref 0,
                             reorder     = ref false,
			     data        = ref [],
			     decls       = ref []
                           }
        in  cfg info end

    fun subgraph(CFG as G.GRAPH{graph_info=INFO graph_info,...}) =
        let val info = INFO{ annotations = ref [],
                             firstBlock  = #firstBlock graph_info,
                             reorder     = #reorder graph_info,
			     data        = #data graph_info,
			     decls       = #decls graph_info
                           }
        in  UpdateGraphInfo.update CFG info end

    fun init(G.GRAPH cfg) =
        (case #entries cfg () of
           [] =>
           let val i     = #new_id cfg ()
               val start = newStart(i,ref 0.0)
               val _     = #add_node cfg (i,start)
               val j     = #new_id cfg ()
               val stop  = newStop(j,ref 0.0)
               val _     = #add_node cfg (j,stop)
           in (*  #add_edge cfg (i,j,EDGE{k=ENTRY,w=ref 0,a=ref []}); *)
               #set_entries cfg [i];
               #set_exits cfg [j]
           end
        |  _ => ()
        )

    fun changed(G.GRAPH{graph_info=INFO{reorder,annotations,...},...}) =
        let fun signal [] = ()
              | signal(Changed(_,f)::an) = (f (); signal an)
              | signal(_::an) = signal an
        in  signal(!annotations);
            reorder := true
        end

    fun annotations(G.GRAPH{graph_info=INFO{annotations=a,...},...}) = a

    fun liveOut (BLOCK{annotations, ...}) =
         case #get LIVEOUT (!annotations) of
            SOME s => s
         |  NONE => C.empty
    fun fallsThruFrom(G.GRAPH cfg,b) =
        let fun f [] = NONE
              | f((i,_,EDGE{k=BRANCH false,...})::_) = SOME i
              | f((i,_,EDGE{k=FALLSTHRU,...})::_) = SOME i
              | f(_::es) = f es
        in  f(#in_edges cfg b)
        end
    fun fallsThruTo(G.GRAPH cfg,b) =
        let fun f [] = NONE
              | f((_,j,EDGE{k=BRANCH false,...})::_) = SOME j
              | f((_,j,EDGE{k=FALLSTHRU,...})::_) = SOME j
              | f(_::es) = f es
        in  f(#out_edges cfg b)
        end
    fun removeEdge CFG (i,j,EDGE{a,...}) =
        Graph.remove_edge' CFG (i,j,fn EDGE{a=a',...} => a = a')

    fun setBranch (CFG as G.GRAPH cfg,b,cond) =
    let fun loop((i,j,EDGE{k=BRANCH cond',w,a})::es,es',x,y) =
            if cond' = cond then
               loop(es, (i,j,EDGE{k=JUMP,w=w,a=a})::es',j,y)
            else
               loop(es, es', x, j)
          | loop([],es',target,elim) = (es',target,elim)
          | loop _ = error "setBranch"
        val outEdges = #out_edges cfg b
        val (outEdges',target,elim) = loop(outEdges,[],~1,~1)
        val _ = if elim < 0 then error "setBranch: bad edges" else ();
        val lab = defineLabel(#node_info cfg target)
        val jmp = InsnProps.jump lab
        val insns = insns(#node_info cfg b)
    in  #set_out_edges cfg (b,outEdges');
        case !insns of
          []      => error "setBranch: missing branch"
        | branch::rest =>
           case InsnProps.instrKind branch of
             InsnProps.IK_JUMP => insns := jmp::rest
           | _ => error "setBranch: bad branch instruction";
        jmp
    end

   local
     fun getNode (G.GRAPH{node_info, ...}, id) = (id, node_info id)
   in
   fun entryId (G.GRAPH{entries, ...}) = (case entries()
	   of [id] => id
	    | _ => error "no unique entry block"
	  (* end case *))
   fun entry cfg = getNode(cfg, entryId cfg)
   fun exitId (G.GRAPH{exits, node_info, ...}) = (case exits()
	   of [id] => id
	    | _ => error "no unique exit block"
	  (* end case *))
   fun exit cfg = getNode(cfg, exitId cfg)
   end

   exception Can'tMerge
   exception NotFound

   fun labelOf(G.GRAPH cfg) node = defineLabel(#node_info cfg node)

   fun copyEdge(EDGE{a,w,k}) = EDGE{a=ref(!a),w=ref(!w),k=k}

   (*=====================================================================
    *
    *  Check whether block i must preceed block j in any linear layout.
    *  This may be true if i falls through to j (transitively)
    *
    *=====================================================================*)
   fun mustPreceed (G.GRAPH cfg) (i,j) =
   let val visited = H.mkTable(23,NotFound)
       fun chase [] = false
         | chase((u,v,EDGE{k=(FALLSTHRU|BRANCH false),...})::_) =
           if H.inDomain visited u then false
           else u = i orelse (H.insert visited (u,true); chase(#in_edges cfg u))
         | chase(_::es) = chase es
   in  i = j orelse chase(#in_edges cfg j)
   end

   (*=====================================================================
    *
    *  Predicates on nodes and edges
    *
    *=====================================================================*)
   fun isMerge (G.GRAPH cfg) node = length(#in_edges cfg node) > 1
   fun isSplit (G.GRAPH cfg) node = length(#out_edges cfg node) > 1
(*
   fun hasSideExits (G.GRAPH cfg) node =
         List.exists (fn (_,_,EDGE{k=SIDEEXIT _,...}) => true
                       | _ => false) (#out_edges cfg node)
*)
   fun hasSideExits _ _ = false
   fun isCriticalEdge CFG (_,_,EDGE{k=ENTRY,...}) = false
     | isCriticalEdge CFG (_,_,EDGE{k=EXIT,...}) = false
     | isCriticalEdge CFG (i,j,_) = isSplit CFG i andalso isMerge CFG j

   (*=====================================================================
    *
    *  Update the label of the branch instruction in a certain block
    *  to be consistent with the control flow edges.  This doesn't work
    *  on hyperblocks!!!
    *
    *=====================================================================*)
   fun updateJumpLabel(CFG as G.GRAPH cfg) =
   let val labelOf = labelOf CFG
       fun update node =
       case #node_info cfg node of
          BLOCK{insns=ref [],...} => ()
       |  BLOCK{kind=START,...} => ()
       |  BLOCK{kind=STOP,...} => ()
       |  BLOCK{insns=insns as ref(jmp::rest),...} =>
             (case #out_edges cfg node of
                [] => ()
             |  [(_,_,EDGE{k=(ENTRY | EXIT),...})] => ()
             |  [(i,j,_)] =>
                  if InsnProps.instrKind jmp = InsnProps.IK_JUMP then
                       insns := InsnProps.setJumpTarget(jmp,labelOf j)::rest
                  else ()
             |  [(_,i,EDGE{k=BRANCH x,...}),
                 (_,j,EDGE{k=BRANCH y,...})] =>
                  let val (no,yes) = if x then (j,i) else (i,j)
                  in  insns :=
                        InsnProps.setBranchTargets{i=jmp,
                                f=labelOf no,t=labelOf yes}::rest
                  end
             |  es =>
                  let fun gt ((_,_,EDGE{k=SWITCH i,...}),
                              (_,_,EDGE{k=SWITCH j,...})) = i > j
                        | gt _ = error "gt"
                      val es = ListMergeSort.sort gt es
                      val labels = map (fn (_,j,_) => labelOf j) es
                  in  error "updateJumpLabel"
                  end
             )
   in  update
   end

   (*=====================================================================
    *
    *  Merge a control flow edge i -> j.
    *  Raise Can't Merge if it is illegal.
    *  After merging blocks i and j will become block i.
    *
    *=====================================================================*)
   fun mergeEdge (CFG as G.GRAPH cfg) (i,j,e as EDGE{w,k,...}) =
   let val _ = case k of
                  (ENTRY | EXIT) => raise Can'tMerge
               |  _ => ()
       val _ = case (#out_edges cfg i,#in_edges cfg j) of
                  ([(_,j',_)],[(i',_,_)]) =>
                     if j' <> j orelse i' <> i then raise Can'tMerge
                     else ()
               |  _ => raise Can'tMerge
       val _ = if mustPreceed CFG (i,j) then raise Can'tMerge else ()
       val BLOCK{align=d2,insns=i2,annotations=a2,...} = #node_info cfg j
       val _  = case !d2 of SOME _ => () | _ => raise Can'tMerge
       val BLOCK{align=d1,insns=i1,annotations=a1,...} = #node_info cfg i
          (* If both blocks have annotations then don't merge them.
           * But instead, just try to removed the jump instruction instead.
           *)
       val canMerge = case (!a1, !a2) of
                 (_::_, _::_) => false
               | _ => true
       val insns1 = case !i1 of
                      [] => []
                    | insns as jmp::rest =>
                        if InsnProps.instrKind jmp = InsnProps.IK_JUMP
                        then rest else insns
   in  if canMerge then
        (i1 := !i2 @ insns1;
         a1 := !a1 @ !a2;
         #set_out_edges cfg
           (i,map (fn (_,j',e) => (i,j',e)) (#out_edges cfg j));
         #remove_node cfg j;
         updateJumpLabel CFG i
        )
       else (* Just eliminate the jump instruction at the end *)
         (i1 := insns1;
          #set_out_edges cfg
            (i,map (fn (i,j,EDGE{w,a,...}) =>
                  (i,j,EDGE{k=FALLSTHRU,w=w,a=a}))
                     (#out_edges cfg i))
         );
       true
   end handle Can'tMerge => false

   (*=====================================================================
    *
    *  Eliminate the jump at the end of a basic block if feasible
    *
    *=====================================================================*)
   fun eliminateJump (CFG as G.GRAPH cfg) i =
       (case #out_edges cfg i of
          [e as (i,j,EDGE{k,w,a})] =>
            (case fallsThruFrom(CFG,j) of
                SOME _ => false
             |  NONE =>
                if mustPreceed CFG (j,i) then false
                else
                let val BLOCK{insns,...} = #node_info cfg i
                    val BLOCK{align,...}  = #node_info cfg j
                in  case (!align,!insns) of
                      (NONE,jmp::rest) =>
                       if InsnProps.instrKind jmp = InsnProps.IK_JUMP then
                        (insns := rest;
                         removeEdge CFG e;
                         #add_edge cfg (i,j,EDGE{k=FALLSTHRU,w=w,a=a});
                         true
                        )
                       else false
                    |  _ => false
                end
            )
       |  _ => false
       )

   (*=====================================================================
    *
    *  Insert a jump at the end of a basic block if feasible
    *
    *=====================================================================*)
   fun insertJump (CFG as G.GRAPH cfg) i =
       (case #out_edges cfg i of
           [e as (i,j,EDGE{k=FALLSTHRU,w,a,...})] =>
              let val BLOCK{insns,...} = #node_info cfg i
              in  insns := InsnProps.jump(labelOf CFG j) :: !insns;
                  removeEdge CFG e;
                  #add_edge cfg (i,j,EDGE{k=JUMP,w=w,a=a});
                  true
              end
        |  _ => false
       )


   (*=====================================================================
    *
    *  Split a group of control flow edge.
    *
    *  Split n groups of control flow edges, all initially entering block j,
    *
    *     i_11 -> j,  i_12 -> j, ...         group 1
    *     i_21 -> j,  i_22 -> j, ...         group 2
    *             ....
    *     i_n1 -> j,  i_n2 -> j, ...         group n
    *
    *  into
    *
    *     i_11 -> k_1
    *     i_12 -> k_1
    *        ...
    *     i_21 -> k_2
    *     i_22 -> k_2
    *        ...
    *     i_n1 -> k_n
    *     i_n2 -> k_n
    *        ...
    *
    *  and k_1 -> k_2
    *      k_2 -> k_3
    *        ...
    *      k_n -> j
    *
    *  Return the new edges
    *       k_1->j,...,k_n -> j
    *
    *  and the new blocks
    *       k_1, ..., k_n.
    *
    *  Each block k_1, ..., k_n can have instructions placed in them.
    *
    *  If the jump flag is true, then a jump is always placed in the
    *  new block k_n; otherwise, we try to eliminate the jump when feasible.
    *
    *=====================================================================*)
   fun splitEdges (CFG as G.GRAPH cfg) {groups=[], jump} = []
     | splitEdges (CFG as G.GRAPH cfg) {groups as ((first,_)::_), jump} =
   let (* target of all the edges *)
       val j = let val (_,j,_) = hd first in j end

       (* Insert an edge i->j with frequency freq.
        * It is a jump edge iff jump flag is true or
        * some other block is already falling into j
        *)
       fun insertEdge(i,j,node_i,freq,jump) =
       let val kind =
               if jump orelse isSome(fallsThruFrom(CFG,j)) then
                  let val insns_i = insns node_i
                  in  insns_i := InsnProps.jump(labelOf CFG j) :: !insns_i;
                      JUMP
                  end
               else
                  FALLSTHRU
           val edge_info = EDGE{k=kind, w=ref freq, a=ref []}
           val edge = (i,j,edge_info)
       in  #add_edge cfg edge;
           edge
       end

       (* Redirect all edges *)
       fun redirect([], freq, new) = new
         | redirect((edges, insns)::groups, freq, new) =
       let
           val freq = sumEdgeFreqs edges + freq (* freq of new block *)

           (*  Sanity check
            *)
           fun check [] = ()
             | check((u,v,_)::es) =
               (if v <> j then error "splitEdge: bad edge" else ();
                check es
               )

           val () = check edges

           val k = #new_id cfg () (* new block id *)
           val node_k =
               BLOCK{id=k, kind=NORMAL,
                     freq= ref freq, align=ref NONE, labels = ref [],
                     insns=ref insns, annotations=ref []}

       in  app (removeEdge CFG) edges;
           app (fn (i,_,e) => #add_edge cfg (i,k,e)) edges;
           #add_node cfg (k,node_k);
           redirect(groups, freq, (k, node_k, edges, freq)::new)
       end

       val new = redirect(groups, 0.0, [])

       (* Add the edges on the chain *)
       fun postprocess([], next, new) = new
         | postprocess((k, node_k, edges, freq)::rest, next, new) =
           let val jump = next = j andalso jump
               val edge = insertEdge(k, next, node_k, freq, jump)
           in  postprocess(rest, k, ((k,node_k),edge)::new)
           end

       val new = postprocess(new, j, [])

   in  (* Update the labels on the groups *)
       app (fn (es, _) => app (fn (i,_,_) => updateJumpLabel CFG i) es) groups;
       new
   end

   (*=====================================================================
    *
    *  Split all critical edges in the CFG
    *
    *=====================================================================*)
   fun splitAllCriticalEdges (CFG as G.GRAPH cfg) =
   let val hasChanged = ref false
   in  #forall_edges cfg
         (fn e => if isCriticalEdge CFG e then
           (splitEdges CFG {groups=[([e],[])],jump=false};
            hasChanged := true)
            else ());
       if !hasChanged then changed CFG else ()
   end

   (*=====================================================================
    *
    *  Tail duplicate a region until there are no side entry edges
    *  entering into the region.  Return the set of new edges and nodes
    *
    *=====================================================================*)
   fun tailDuplicate (CFG as G.GRAPH cfg : cfg)
                     {subgraph=G.GRAPH subgraph : cfg,root} =
   let
       val blockMap = H.mkTable(10,NotFound)
       val _ = print("[root "^Int.toString root^"]\n")

       fun duplicate v =
           H.lookup blockMap v handle NotFound =>
           let val w  = #new_id cfg ()
               val w' = copyBlock(w,#node_info cfg v)
           in  #add_node cfg (w,w');
               H.insert blockMap (v,(w,w'));
               app (#add_edge cfg)
                   (map (fn (i,j,e) => (w,j,copyEdge e)) (#out_edges cfg v));
               updateJumpLabel CFG w;
               (w,w')
           end

       fun process((n,_)::rest,ns,Ns,Es) =
            process(rest,collect(#entry_edges subgraph n,ns),Ns,Es)
         | process([],ns,Ns,Es) = dupl(ns,Ns,Es,false)

       and collect([],ns) = ns
         | collect((i,_,_)::es,ns) = collect(es,if i = root then ns else i::ns)

       and dupl([],Ns,Es,changed) = (Ns,Es,changed)
         | dupl(n::ns,Ns,Es,changed) =
              redirect(#out_edges cfg n,ns,Ns,Es,changed)

       and redirect([],ns,Ns,Es,changed) = dupl(ns,Ns,Es,changed)
         | redirect((u,v,e)::es,ns,Ns,Es,changed) =
            if v <> root andalso
               #has_edge cfg (u,v) andalso
               #has_node subgraph v andalso
               not(#has_edge subgraph (u,v)) then
               (*
                * u -> v is a side entry edge, duplicate v
                *)
            let val _ = print("[tail duplicating "^Int.toString u^" -> "^
                              Int.toString v^"]\n")
                val (w,w') = duplicate v
            in  removeEdge CFG (u,v,e);
                #add_edge cfg (u,w,e);
                updateJumpLabel CFG u;
                redirect(es,w::ns,(w,w')::Ns,(u,w,e)::Es,true)
            end
            else redirect(es,ns,Ns,Es,changed)

       fun iter(Ns,Es) =
           let val (Ns,Es,hasChanged) = process(#nodes subgraph (),[],Ns,Es)
           in  if hasChanged then (changed CFG; iter(Ns,Es))
               else {nodes=Ns,edges=Es}
           end

   in  iter([],[])
   end


   (*=====================================================================
    *
    *  Remove unreachable code in the CFG
    *
    *=====================================================================*)
   fun removeUnreachableCode(CFG as G.GRAPH cfg) =
   let val N = #capacity cfg ()
       val visited = A.array(N,false)
       fun mark n = if A.sub(visited,n) then ()
                    else (A.update(visited,n,true); app mark (#succ cfg n))
       val hasChanged = ref false
       fun remove(b,BLOCK{align,insns,...}) =
           if A.sub(visited,b) then ()
           else
           (hasChanged :=true;
            case #in_edges cfg b of
              [] => #remove_node cfg b
            |  _  => (insns := []; #set_out_edges cfg (b,[]))
           )
   in  app mark (#entries cfg ());
       #forall_nodes cfg remove;
       if !hasChanged then changed CFG else ()
   end


   (*=====================================================================
    *
    *  Merge all edges in the CFG.
    *  Merge higher frequency edges first
    *
    *=====================================================================*)
   fun mergeAllEdges(CFG as G.GRAPH cfg) =
   let val mergeEdge = mergeEdge CFG
       fun higherFreq((_,_,EDGE{w=x,...}),(_,_,EDGE{w=y,...}))= !x < !y
       fun mergeAll([],changed) = changed
         | mergeAll(e::es,changed) = mergeAll(es,mergeEdge e orelse changed)
       (* note: sort expects the gt operator and sorts in ascending order *)
       val hasChanged = mergeAll(ListMergeSort.sort higherFreq (#edges cfg ()),
                                 false)
   in  if hasChanged then changed CFG else ()
   end

   (*========================================================================
    *
    *  Miscellaneous
    *
    *========================================================================*)
   fun cdgEdge(EDGE{k, ...}) =
        case k of
           (JUMP | FALLSTHRU) => false
        |  _ => true

   (*========================================================================
    *
    *  Pretty Printing and Viewing
    *
    *========================================================================*)

    structure F = Format

    fun show_edge (EDGE{k,w,a,...}) = let
	  val kind = (case k
		 of JUMP	=> "jump"
        	  | FALLSTHRU	=> "fallsthru"
        	  | BRANCH b	=> Bool.toString b
        	  | SWITCH i	=> Int.toString i
        	  | ENTRY	=> "entry"
        	  | EXIT	=> "exit"
        	  | FLOWSTO	=> "flowsto"
		(* end case *))
	  in
	    F.format "%s[%f]" [F.STR kind, F.REAL(!w)]
	  end

    fun getString f x = let
	  val buffer = StringOutStream.mkStreamBuf()
	  val S      = StringOutStream.openStringOut buffer
	  val _      = AsmStream.withStream S f x
	  in
	    StringOutStream.getString buffer
	  end

    fun show_block an block = let
	  val text = getString (emit an) block
	  in
	    foldr (fn (x,"") => x | (x,y) => x^" "^y) ""
              (String.tokens (fn #" " => true | _ => false) text)
	  end

    fun dumpBlock (outS, cfg as G.GRAPH g) = let
	  fun pr str = TextIO.output(outS, str)
	  fun prList [] = ()
	    | prList [i] = pr i
	    | prList (h::t) = (pr (h ^ ", "); prList t)
	  val Asm.S.STREAM{emit,defineLabel,annotation,...} =
        	AsmStream.withStream outS Asm.makeStream []
	  fun showFreq (ref w) = F.format "[%f]" [F.REAL w]
	  fun showEdge (blknum,e) =
		F.format "%d:%s" [F.INT blknum, F.STR(show_edge e)]
	  fun showSucc (_, x, e) = showEdge(x,e)
	  fun showPred (x, _, e) = showEdge(x,e)
	  fun showSuccs b = (
		pr "\tsucc:     ";
        	prList (map showSucc (#out_edges g b));
        	pr "\n")
	  fun showPreds b = (
        	pr "\tpred:     ";
        	prList (map showPred (#in_edges g b));
        	pr "\n")
	  fun printBlock (_, BLOCK{kind=START, id, freq, ...}) = (
        	pr (F.format "ENTRY %d %s\n" [F.INT id, F.STR(showFreq freq)]);
        	showSuccs id)
            | printBlock (_, BLOCK{kind=STOP, id, freq, ...}) = (
		pr (F.format "EXIT %d %s\n" [F.INT id, F.STR(showFreq freq)]);
        	showPreds id)
            | printBlock (
		_, BLOCK{id, align, freq, insns, annotations, labels, ...}
	      ) = (
	       pr (F.format "BLOCK %d %s\n" [F.INT id, F.STR(showFreq freq)]);
	       case !align of NONE => () | SOME p => (pr (P.toString p ^ "\n"));
               List.app annotation (!annotations);
               List.app defineLabel (!labels);
               showSuccs id;
               showPreds id;
               List.app emit (List.rev (!insns)))
	  in
	    printBlock
	  end

    fun dump (outS, title, cfg as G.GRAPH g) = let
	  fun pr str = TextIO.output(outS, str)
	  val annotations = !(annotations cfg)
	  val Asm.S.STREAM{annotation, ...} =
        	AsmStream.withStream outS Asm.makeStream annotations
	  fun printData () = let
        	val INFO{data, ...} = #graph_info g
		in
		  List.app (pr o P.toString) (rev(!data))
		end
	  in
	    pr(F.format "[ %s ]\n" [F.STR title]);
	    List.app annotation annotations;
	    (* printBlock entry; *)
	    AsmStream.withStream outS (#forall_nodes g) (dumpBlock (outS, cfg));
	    (* printBlock exit; *)
	    AsmStream.withStream outS printData ();
	    TextIO.flushOut outS
	  end

end

