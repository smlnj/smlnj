(* 
 * This functor factors out the machine independent part of the register
 * allocator.   This works well for RISC machines; not applicable to x86.
 *)
functor BackEnd
  (structure MLTreeComp : MLTREECOMP    (* instruction selection *)
   structure Flowgraph  : FLOWGRAPH
   structure InsnProps  : INSN_PROPERTIES
   structure Asm        : INSTRUCTION_EMITTER
   structure RA         : CLUSTER_OPTIMIZATION
      sharing InsnProps.I = Flowgraph.I = Asm.I = MLTreeComp.I 
      sharing Asm.P = Flowgraph.P = MLTreeComp.T.PseudoOp
      sharing RA.F = Flowgraph
  ) : sig
         structure T : MLTREE
         structure C : CELLS
         val codegen : Label.label * T.stm list -> unit  
      end =
struct

   structure F      = Flowgraph
   structure T      = MLTreeComp.T
   structure I      = F.I
   structure C      = I.C
   structure Stream = T.Stream

   (* Flowgraph generation *)
   structure FlowGraphGen =
       ClusterGen(structure Flowgraph = F
                  structure InsnProps = InsnProps
                  structure MLTree    = MLTreeComp.T
                 )

   (* Assembly output *)
   structure Assembler = 
      ClusterEmit(structure F = F
                  structure E = Asm)

   fun error msg = MLRiscErrorMsg.error("BackEnd",msg)

   (* How to compile a cluster *) 
   fun compile cluster =
   let val cluster = RA.run cluster (* just run register allocation *)
   in  Assembler.asmEmit cluster    (* and output the assembly *)
   end

   fun codegen(functionName, mltreeStms) =
   let val _ =
         (* initialize all hidden states first *)
          Label.reset();  (* okay, just the label counters *) 
       (*
        * Extract the methods from a stream 
        *)
       val stream as Stream.STREAM
          { beginCluster,  (* start a cluster *)
            endCluster,    (* end a cluster *)
            emit,          (* emit MLTREE stm *)
            defineLabel,   (* define a local label *)
            entryLabel,    (* define an external entry *)
            exitBlock,     (* mark the end of a procedure *)
            pseudoOp,      (* emit a pseudo op *)
            annotation,    (* add an annotation *)
            ... } =
            MLTreeComp.selectInstructions
                (FlowGraphGen.newStream{compile=compile, flowgraph=NONE})
   in  beginCluster 0;      (* start a new cluster *)
       entryLabel functionName; (* define the entry label *)
       app emit mltreeStms; (* emit all the statements *)
         (* IMPORTANT: normally you'll have to call the other methods too *)
       endCluster []        (* end the cluster *)
   end 
end
