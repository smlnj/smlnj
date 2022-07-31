(* prof-control.sig
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * This structure implements the interface to the run-time system's profiling
 * support library.  It is not meant for general use.
 *
 *)

signature PROF_CONTROL =
  sig

  (* get the timer count array *)
    val getTimeArray : unit -> int array

    val profMode : bool ref	(* controls profile instrumentation *)
    val current : int ref

  (* turn on/off profile signals.  These functions set/clear the profMode
   * flag.
   *)
    val profileOn  : unit -> unit
    val profileOff : unit -> unit

    val getTimingMode : unit -> bool

  (* get the time quantum in microseconds *)
    val getQuantum : unit -> int

    datatype compunit = UNIT of {
	base: int,
	size: int,
	counts: int Array.array,
	names: string
      }
			   
    val runTimeIndex : int
    val minorGCIndex : int
    val majorGCIndex : int
    val otherIndex : int
    val compileIndex : int
    val numPredefIndices : int

    val units : compunit list ref

    val reset : unit -> unit

  (* space profiling hooks *)
    val spaceProfiling : bool ref
    val spaceProfRegister :
	  (Unsafe.Object.object * string -> Unsafe.Object.object) ref

  end;

