(*
 * This module is reponsible for generating garbage collection 
 * code for all gc-points in the program.  That is, we delay the generation
 * of garbage collection code until all optimizations have been performed.
 * The gc code to be generated is determined by a callback to the client.
 *)

functor GCGen
   (structure MLTreeComp : MLTREECOMP
    structure IR         : MLRISC_IR
    structure GCCallBack : GC_CALLBACK
    structure InsnProps  : INSN_PROPERTIES
       sharing GCCallBack.T          = MLTreeComp.T
       sharing GCCallBack.C          = IR.I.C 
       sharing MLTreeComp.T.Constant = IR.I.Constant
       sharing MLTreeComp.T.PseudoOp = IR.CFG.P
       sharing IR.I = InsnProps.I = MLTreeComp.I
   ) : MLRISC_IR_OPTIMIZATION =
struct

   structure C   = IR.I.C
   structure T   = MLTreeComp.T
   structure IR  = IR
   structure CFG = IR.CFG
   structure GC  = GCCallBack.GC
   structure G   = Graph
   structure A   = Array
   structure Liveness =   
      GCLiveness(structure IR = IR
                 structure GC = GC
                 structure InsnProps = InsnProps)

   structure Gen = CFGGen
      (structure CFG       = CFG
       structure MLTree    = T
       structure InsnProps = InsnProps
      )

   type flowgraph = IR.IR

   fun error msg = MLRiscErrorMsg.error("GCGen",msg)

   val gc_bug = MLRiscControl.getCounter "gc-bug"

   val name = "Generate GC code"

   fun run (IR as G.GRAPH cfg) =
   let (*
        * Run gc-typed liveness analysis
        *)
       val table = Liveness.liveness IR
       val instrStream = Gen.newStream{compile=fn _ => (), flowgraph=SOME IR}
       fun dummy _ = error "no extension" 
       val stream as T.Stream.STREAM{beginCluster, endCluster, ...} = 
           MLTreeComp.selectInstructions instrStream
       val cfgAnnotations = CFG.annotations IR
 
       (*
        * For each gc-point, invoke the callback to generate GC code.
        *)
       fun process(b,b' as CFG.BLOCK{annotations,insns,...}) =
           case #get MLRiscAnnotations.GCSAFEPOINT (!annotations) of
             NONE => ()
           | SOME msg =>
           let val {liveIn,liveOut} = A.sub(table,b)
               val roots = liveIn
               val return = #node_info cfg (hd(#succ cfg b))
           in  CFG.changed IR;
               GCCallBack.callgcCallback
               { id          = b,
                 msg         = msg,
                 gcLabel     = CFG.defineLabel b',
                 returnLabel = CFG.defineLabel return,
                 roots       = liveIn,
                 stream      = stream
               } handle _ => gc_bug := !gc_bug + 1 (* continue on error *)
           end
           
   in  beginCluster 0;
       #forall_nodes cfg process;
       endCluster [];
       IR
   end

end
