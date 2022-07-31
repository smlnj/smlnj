(* mlrisc-timing.sml
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies
 *)

signature MLRISC_TIMING =
sig

    val timePhase : string -> ('a -> 'b) -> 'a -> 'b
end

structure MLRiscTiming : MLRISC_TIMING =
struct

   fun timePhase name f =
   let val timing = MLRiscControl.timing name
       val { gc, usr, sys } = !timing
       fun run x = 
       let val timer = Timer.startCPUTimer()
           fun update timer = 
           let val t = Timer.checkCPUTimes timer
	       val gc' = #usr (#gc t)
	       val usr' = #usr (#nongc t)
	       val sys' = Time.+ (#sys (#gc t), #sys (#nongc t))
           in  timing := {gc=Time.+(gc,gc'),
                          usr=Time.+(usr,usr'),
                          sys=Time.+(sys,sys')}
           end
           val y = f x handle e => (update timer; raise e)
       in  update timer; y
       end
   in  run end
  
end
