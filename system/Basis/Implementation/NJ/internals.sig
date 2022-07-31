(* internals.sig
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * This structure (SMLofNJ.Internals) is a gathering place for internal
 * features that need to be exposed outside the boot directory.
 *)
signature INTERNALS = sig

    structure CleanUp : CLEAN_UP
    structure ProfControl : PROF_CONTROL
    structure GC : GC

    val prHook : (string -> unit) ref
	(* this hook can be used to change the top-level print function *)

  (* Routines for managing the internal signal handler tables.  These are
   * for programs that must otherwise bypass the standard initialization
   * mechanisms.
   *)
    val initSigTbl : unit -> unit
    val clearSigTbl : unit -> unit
    val resetSigTbl : unit -> unit

  (* reset the total real and CPU time timers *)
    val resetTimers : unit -> unit

  (* generic trace/debug/profile control; M.Blume 10/2004 *)
    structure TDP : sig
	type plugin = { name: string,
			save: unit -> unit -> unit,
			push: int * int -> unit -> unit,
			nopush: int * int -> unit,
			enter: int * int -> unit,
			register: int * int * int * string -> unit }

	type monitor = { name: string, monitor: bool * (unit -> unit) -> unit }

	val active_plugins : plugin list ref
	val active_monitors : monitor list ref

	(* reserve a number of IDs *)
	val reserve : int -> int
	(* reset the ID generator *)
	val reset : unit -> unit

	(* pre-defined ID kinds: *)
	val idk_entry_point   : int
	val idk_non_tail_call : int
	val idk_tail_call     : int

	(* ref cell controlling instrumentation mode *)
	val mode : bool ref

	val with_monitors : bool -> (unit -> unit) -> unit
    end
end
