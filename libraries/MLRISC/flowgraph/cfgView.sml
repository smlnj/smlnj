(* cfgView.sml -- graphical viewing utilities for cfg 
 *
 * Copyright (c) 2001 Bell Laboratories.
 *)
functor CFGView
  (structure Asm : INSTRUCTION_EMITTER 
   structure CFG : CONTROL_FLOW_GRAPH
		   where I = Asm.I
		     and P = Asm.S.P
  ) : CFG_VIEW = 

struct
 
   structure L   = GraphLayout
   structure CFG = CFG
   structure G = Graph
   structure W = Freq
   structure S = Asm.S
   
   fun nl() = TextIO.output(!AsmStream.asmOutStream,"\n")
   fun kindName CFG.START          = "START"
     | kindName CFG.STOP           = "STOP"
     | kindName CFG.NORMAL         = "Block"


   fun emitHeader (S.STREAM{comment,annotation,...}) 
                   (CFG.BLOCK{id,kind,freq,annotations,...}) = 
       (comment(kindName kind ^"["^Int.toString id^
                    "] ("^W.toString (!freq)^")");
        nl();
        app annotation (!annotations)
       ) 

   fun emitFooter (S.STREAM{comment,...}) (CFG.BLOCK{annotations,...}) = 
        (case #get CFG.LIVEOUT (!annotations) of
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

  fun emitStuff outline annotations (block as CFG.BLOCK{insns,labels,...}) =
       let val S as S.STREAM{pseudoOp,defineLabel,emit,...} = 
               Asm.makeStream annotations
       in  emitHeader S block;
           app defineLabel (!labels); 
           if outline then () else app emit (rev (!insns));
           emitFooter S block
       end

    val emit = emitStuff false 
    val emitOutline = emitStuff true []

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

   val show_edge = CFG.show_edge

   fun edgeStyle(i,j,e as CFG.EDGE{k,a,...}) = 
   let val a = L.LABEL(show_edge e) :: getStyle a
   in  case k of 
         (CFG.ENTRY | CFG.EXIT) => green :: a
       | (CFG.FALLSTHRU | CFG.BRANCH false) => yellow :: a
       | _ => red :: a
   end 

   val outline = MLRiscControl.getFlag "view-outline"
 
   fun annotations(G.GRAPH{graph_info=CFG.INFO{annotations=a,...},...}) = a

   fun viewStyle cfg =
   let val an     = !(annotations cfg)
       fun node (n,b as CFG.BLOCK{annotations,...}) = 
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
       fun node(n,b as CFG.BLOCK{annotations,...}) = 
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



