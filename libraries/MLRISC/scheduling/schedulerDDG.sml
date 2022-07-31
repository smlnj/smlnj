(*
 * This module describes a DDG for acyclic global scheduling 
 * (for non-predicated architectures.) 
 * Hyperblock scheduling uses another data structure.
 *
 * -- Allen
 *)
functor SchedulerDDG(SchedProps : SCHEDULING_PROPERTIES) : SCHEDULER_DDG =
struct

   structure SchedProps = SchedProps
   structure I          = SchedProps.I
   structure C          = I.C
   structure A          = Array
   structure GI         = DirectedGraph(A)
   structure G          = Graph

   datatype dependence = 
        FLOW | OUTPUT | ANTI             (* register based dependence *)
      | MEM_FLOW | MEM_OUTPUT | MEM_ANTI (* memory based dependence *)
      | CTRL | CTRL_ANTI                 (* control dependence *)
      | LIVEIN | LIVEOUT

   type latency = SchedProps.latency

   datatype edge = EDGE of {l : latency,   (* latency *)
                            r : C.cell,    (* register *)
                            d : dependence (* dependence type *)
                           }

   datatype node = NODE of {instr: I.instruction, b:int,
                            defs:(C.cell * latency) list, uses:C.cell list}

   type liveInMap  = node Graph.node IntHashTable.hash_table
   type liveOutMap = node Graph.node IntHashTable.hash_table
   type block      = int
   type blockMap   = block Array.array (* mapping from block id -> block *)
   type liveInMap  = node Graph.node IntHashTable.hash_table
   type liveOutMap = node Graph.node IntHashTable.hash_table

   type ('node,'edge) internalInfo =
        {succ  : 'edge Graph.edge list Array.array,
         pred  : 'edge Graph.edge list Array.array,
         nodes : 'node option Array.array
        }

   type globalInfo =
        {liveInMap  : liveInMap,
         liveOutMap : liveOutMap,
         blockMap   : blockMap
        }

   datatype ('node,'edge) info = 
        INFO of {internalInfo: ('node,'edge) internalInfo,
                 globalInfo  : globalInfo option ref 
                }

   withtype ('node,'edge) ddg  = ('node,'edge,('node,'edge) info) Graph.graph

   fun newDDG(n) =
   let val succ = A.array(n,[])
       val pred = A.array(n,[])
       val nodes= A.array(n,NONE)
       val info = INFO{internalInfo={succ=succ,pred=pred,nodes=nodes},
                       globalInfo=ref NONE}
       val ddg  = GI.newGraph{name="DDG",info=info,
                              pred=pred,succ=succ,nodes=nodes}
   in   ddg
   end

   fun internalInfo(G.GRAPH ddg) =
   let val INFO{internalInfo, ...} = #graph_info ddg
   in  internalInfo end

   fun globalInfo(G.GRAPH ddg) = 
   let val INFO{globalInfo, ...} = #graph_info ddg
   in  globalInfo end

   fun latToString i = if i < 0 then "-"^Int.toString(~i) else Int.toString i

   (* Slow but pretty way of pretty printing registers *)
   fun showReg(prefix,r) = prefix^C.toString r
 
   fun edgeToString(EDGE{l,d,r}) =
   let val (dep,prefix) = 
           case d of
             FLOW       => ("","r")
           | OUTPUT     => ("out","r")
           | ANTI       => ("anti","r")
           | MEM_FLOW   => ("","m")
           | MEM_OUTPUT => ("out","m")
           | MEM_ANTI   => ("anti","m")
           | CTRL       => ("ctrl","c")
           | CTRL_ANTI  => ("anti","c")
           | LIVEIN     => ("livein","r")
           | LIVEOUT    => ("liveout","r")
       val lat = if l = 0 then "" else " "^latToString l

       val reg = "("^showReg(prefix,r)^")" 
   in  dep ^ lat ^ reg end

   fun cellsToString S =
   let fun pr r = showReg("r",r)
   in  LineBreak.lineBreak 50
         (List.foldr (fn (r,l) => if l = "" then pr r else pr r^" "^l) "" S) 
   end

   val LIVENESS = Annotations.new
                    (SOME(fn {liveIn,liveOut} =>
                       "liveIn: "^cellsToString liveIn^"\n"^
                       "liveOut: "^cellsToString liveOut^"\n"
                    ))

end
