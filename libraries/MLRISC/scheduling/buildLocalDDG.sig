signature BASIC_BLOCK_SCHEDULER_DDG_BUILDER =
sig

   structure I   : INSTRUCTIONS
   structure C   : CELLS
   structure DDG : SCHEDULER_DDG
      sharing I = DDG.I
      sharing C = I.C

     (* instructions in reverse order *)
   type ddg = (I.instruction,DDG.latency) DDG.ddg
   val buildDDG : { cpu_info : DDG.SchedProps.cpu_info,
                    ddg      : ddg
                  } -> I.instruction list -> unit

end
