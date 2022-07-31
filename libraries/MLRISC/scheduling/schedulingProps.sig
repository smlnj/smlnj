(*
 * This signature describes the machine properties needed by the 
 * global schedulers. 
 *
 * -- Allen
 *)
signature SCHEDULING_PROPERTIES =
sig

   structure I : INSTRUCTIONS

   (* 
    * Type reservation_table is used to represent the state 
    * of the pipeline as a partial schedule is constructed.
    * Type resource represents the resources used by an instruction
    * during its execution.  Normally this is represented by a
    * reservation table.   These are kept abstract so that the
    * client can have freedom on how to implement these things.
    *)
   type reservation_table 
   type pipeline
   type latency = int
   type time = int

   type cpu (* name identifying the implementaton *)

   (* special instructions *)
   val source : I.instruction
   val sink   : I.instruction 

   (* convert a name to a specific implementation *)
   val cpu    : string -> cpu

   datatype cpu_info = 
      CPU_INFO of
      { (* maximum number of instructions issued per cycle *)
        maxIssues : int, 
        (* 
         * Definition/use.  Definitions contain latencies.
         *)
        defUse : I.instruction -> (I.C.cell * latency) list * I.C.cell list,

        (*
         * Create a new reservation table of at most n time steps.
         * If the backward flag is on then we are actually building
         * the schedule in backwards manner.
         *)
        newTable : int -> reservation_table,

        (*
         * Extract the pipeline characteristics of an instruction 
         *)
        pipeline : I.instruction -> pipeline,
        (*
         * Take a reservation table, a time step t, and an instruction. 
         * Find a slot to insert the instruction into the reservation 
         * table at the earliest (latest) feasible time no earlier (later) 
         * than t.  
         *)
        findSlot : reservation_table * time * pipeline -> time,
        insert   : reservation_table * time * pipeline -> unit
      }

   (* This function takes an architecture name and returns
    * a bunch of properties specific to the architecture.
    * It is structured this way so that we can dynamically change the
    * architecture parameter.
    *)
   val info : {cpu:cpu, backward:bool (* backward scheduling? *)} -> cpu_info

   val splitCopies : (I.C.cell -> I.C.cell) -> 
                         I.instruction -> I.instruction list

end
