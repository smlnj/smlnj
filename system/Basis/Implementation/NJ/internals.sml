(* internals.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * This structure (SMLofNJ.Internals) is a gathering place for internal
 * features that need to be exposed outside the boot directory.
 *)

structure Internals : INTERNALS = struct

    structure CleanUp = CleanUp
    structure ProfControl = ProfControl
    structure GC = GC

    val prHook = PrintHook.prHook

    val initSigTbl = InternalSignals.initSigTbl
    val clearSigTbl = InternalSignals.clearSigTbl
    val resetSigTbl = InternalSignals.resetSigTbl

    val resetTimers = InternalTimer.resetTimers

    structure TDP = struct
        type plugin = Core.tdp_plugin
	type monitor = { name: string, monitor: bool * (unit -> unit) -> unit }

	val active_plugins = Core.tdp_active_plugins

	val active_monitors = ref ([] : monitor list)

	fun reserve n = Core.tdp_reserve n
	fun reset () = Core.tdp_reset ()

	val idk_entry_point = Core.tdp_idk_entry_point
	val idk_tail_call = Core.tdp_idk_tail_call
	val idk_non_tail_call = Core.tdp_idk_non_tail_call

	val mode = ref false

	fun with_monitors report_final_exn work =
	    let fun loop [] = work ()
		  | loop ({ name, monitor } :: ms) =
(print(concat["with_monitors: ", name, "\n"]);
		      monitor (report_final_exn, fn () => loop ms)
)
	    in
		loop (!active_monitors)
	    end
    end
end
