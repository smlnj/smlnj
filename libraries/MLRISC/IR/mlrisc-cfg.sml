(*
 * The control flow graph representation used for optimizations.
 *
 * -- Allen
 *)
functor ControlFlowGraph
   (structure I : INSTRUCTIONS
    structure PseudoOps : PSEUDO_OPS
    structure GraphImpl : GRAPH_IMPLEMENTATION
    structure InsnProps : INSN_PROPERTIES
    structure Asm : INSTRUCTION_EMITTER
       sharing Asm.I = InsnProps.I = I
       sharing Asm.P = PseudoOps
   ) : CONTROL_FLOW_GRAPH =
struct

    structure I = I
    structure P = PseudoOps
    structure C = I.C
    structure W = Freq
    structure G = Graph
    structure L = GraphLayout
    structure A = Annotations
    structure S = Asm.S
   
    type weight = W.freq

    datatype block_kind = 
        START          (* entry node *)
      | STOP           (* exit node *)
      | NORMAL         (* normal node *)
      | HYPERBLOCK     (* hyperblock *)

    and data = LABEL  of Label.label
             | PSEUDO of P.pseudo_op
 
    and block = 
       BLOCK of
       {  id          : int,                        (* block id *)
          kind        : block_kind,                 (* block kind *)
          freq        : weight ref,                 (* execution frequency *) 
          data        : data list ref,              (* data preceeding block *) 
          labels      : Label.label list ref,       (* labels on blocks *) 
          insns       : I.instruction list ref,     (* in rev order *)
          annotations : Annotations.annotations ref (* annotations *)
       }

    and edge_kind = ENTRY           (* entry edge *) 
                  | EXIT            (* exit edge *)
                  | JUMP            (* unconditional jump *)
                  | FALLSTHRU       (* falls through to next block *)  
                  | BRANCH of bool  (* branch *) 
                  | SWITCH of int   (* computed goto *)   
                  | SIDEEXIT of int (* side exit *)   
   
    and edge_info = EDGE of { k : edge_kind,                  (* edge kind *)
                              w : weight ref,                 (* edge freq *)
                              a : Annotations.annotations ref (* annotations *)
                            }

    type edge = edge_info Graph.edge
    type node = block Graph.node

    datatype info = 
        INFO of { annotations : Annotations.annotations ref,
                  firstBlock  : int ref,
                  reorder     : bool ref
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
                            (C.CellSet.toString c))))
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
      | defineLabel(BLOCK{labels,...}) = let val l = Label.newLabel ""
                                         in  labels := [l]; l end
    fun insns(BLOCK{insns, ...}) = insns
    fun freq(BLOCK{freq, ...}) = freq

    fun newBlock'(id,kind,insns,freq) =
        BLOCK{ id          = id,
               kind        = kind,
               freq        = freq,
               data        = ref [],
               labels      = ref [],
               insns       = ref insns,
               annotations = ref []
             }

    fun copyBlock(id,BLOCK{kind,freq,data,labels,insns,annotations,...}) =
        BLOCK{ id          = id,
               kind        = kind,
               freq        = ref (!freq),
               data        = ref (!data),
               labels      = ref [],
               insns       = ref (!insns),
               annotations = ref (!annotations) 
             }

    fun newBlock(id,freq) = newBlock'(id,NORMAL,[],freq)
    fun newStart(id,freq) = newBlock'(id,START,[],freq)
    fun newStop(id,freq) = newBlock'(id,STOP,[],freq)

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
      | kindName HYPERBLOCK     = "Hyperblock"
      | kindName NORMAL         = "Block"

    fun nl() = TextIO.output(!AsmStream.asmOutStream,"\n")

    fun emitHeader (S.STREAM{comment,annotation,...}) 
                   (BLOCK{id,kind,freq,annotations,...}) = 
       (comment(kindName kind ^"["^Int.toString id^
                    "] ("^W.toString (!freq)^")");
        nl();
        app annotation (!annotations)
       ) 

    fun emitFooter (S.STREAM{comment,...}) (BLOCK{annotations,...}) = 
        (case #get LIVEOUT (!annotations) of
            SOME s => 
            let val regs = String.tokens Char.isSpace(C.CellSet.toString s)
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
           (block as BLOCK{insns,data,labels,...}) =
       let val S as S.STREAM{pseudoOp,defineLabel,emit,...} = 
               Asm.makeStream annotations
       in  emitHeader S block;
           app (fn PSEUDO p => pseudoOp p
                 | LABEL l  => defineLabel l) (!data);
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
                             reorder     = ref false
                           }
        in  cfg info end

    fun subgraph(CFG as G.GRAPH{graph_info=INFO graph_info,...}) =
        let val info = INFO{ annotations = ref [],
                             firstBlock  = #firstBlock graph_info,
                             reorder     = #reorder graph_info
                           }
        in  UpdateGraphInfo.update CFG info end

    fun init(G.GRAPH cfg) =
        (case #entries cfg () of
           [] =>
           let val i     = #new_id cfg ()
               val start = newStart(i,ref 0)
               val _     = #add_node cfg (i,start)
               val j     = #new_id cfg ()
               val stop  = newStop(j,ref 0)
               val _     = #add_node cfg (j,stop) 
           in  #add_edge cfg (i,j,EDGE{k=ENTRY,w=ref 0,a=ref []});
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
   fun show_edge(EDGE{k,w,a,...}) = 
       let val kind = case k of
                         JUMP      => ""
                      |  FALLSTHRU => "fallsthru"
                      |  BRANCH b => Bool.toString b
                      |  SWITCH i => Int.toString i
                      |  ENTRY    => "entry"
                      |  EXIT     => "exit"
                      |  SIDEEXIT i => "sideexit("^Int.toString i^")"
           val weight = "(" ^ W.toString (!w) ^ ")"
       in  kind ^ weight 
       end 

   fun getString f x = 
   let val buffer = StringOutStream.mkStreamBuf()
       val S      = StringOutStream.openStringOut buffer
       val _      = AsmStream.withStream S f x 
   in  StringOutStream.getString buffer end

   fun show_block an block = 
   let val text = getString (emit an) block
   in  foldr (fn (x,"") => x | (x,y) => x^" "^y) ""
            (String.tokens (fn #" " => true | _ => false) text)
   end

   fun headerText block = getString 
        (fn b => emitHeader (Asm.makeStream []) b) block
   fun footerText block = getString 
        (fn b => emitFooter (Asm.makeStream []) b) block

   fun getStyle a = (case #get L.STYLE (!a) of SOME l => l | NONE => [])

   val green = L.COLOR "green"
   val red   = L.COLOR "red"
   val yellow = L.COLOR "yellow"

   fun edgeStyle(i,j,e as EDGE{k,a,...}) = 
   let val a = L.LABEL(show_edge e) :: getStyle a
   in  case k of 
         (ENTRY | EXIT) => green :: a
       | (FALLSTHRU | BRANCH false) => yellow :: a
       | _ => red :: a
   end 

   val outline = MLRiscControl.getFlag "view-outline"

   fun viewStyle cfg =
   let val an     = !(annotations cfg)
       fun node (n,b as BLOCK{annotations,...}) = 
           if !outline then
              L.LABEL(getString emitOutline b) :: getStyle annotations
           else
              L.LABEL(show_block an b) :: getStyle annotations
   in  { graph = fn _ => [],
         edge  = edgeStyle,
         node  = node
       } 
   end

   fun viewLayout cfg = L.makeLayout (viewStyle cfg) cfg

   fun subgraphLayout {cfg,subgraph = G.GRAPH subgraph} =
   let val an     = !(annotations cfg)
       fun node(n,b as BLOCK{annotations,...}) = 
          if #has_node subgraph n then
             L.LABEL(show_block an b) :: getStyle annotations
          else
             L.COLOR "lightblue"::L.LABEL(headerText b) :: getStyle annotations
       fun edge(i,j,e) = 
            if #has_edge subgraph (i,j) then edgeStyle(i,j,e)
            else [L.EDGEPATTERN "dotted"]
   in  L.makeLayout {graph = fn _ => [],
                     edge  = edge,
                     node  = node} cfg
   end

end

