(*
 * Simple minded basic block scheduling
 *) 
functor ClusterBasicBlockScheduler
   (structure Flowgraph : FLOWGRAPH
    structure BBSched   : BASIC_BLOCK_SCHEDULER
       sharing Flowgraph.I = BBSched.I
    val cpu : string ref
   ) : CLUSTER_OPTIMIZATION =
struct

   structure F = Flowgraph
   type flowgraph = F.cluster

   val name = "Basic Block Scheduling"

   fun run(cluster as F.CLUSTER{blocks, annotations, ...}) = 
   if #contains MLRiscAnnotations.NO_OPTIMIZATION (!annotations)
   then cluster
   else
   let val schedule = BBSched.schedule {cpu= !cpu}
       fun sched(F.BBLOCK{annotations, insns, ...}) = 
            if #contains MLRiscAnnotations.NO_OPTIMIZATION (!annotations) 
            then ()
            else insns := schedule(! insns)
         | sched _ = ()
   in  app sched blocks;
       cluster
   end
end
