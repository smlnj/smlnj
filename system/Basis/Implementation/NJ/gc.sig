(* gc.sig
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Garbage collector control and stats.
 *)

signature GC =
  sig

    (* invoke GC; the argument specifies the minimum generation to collect *)
    val doGC : int -> unit

    (* turn on/off GC messages (default is off) *)
    val messages : bool -> unit

    (* set the threshold for generating a GC signal.  The default is 1, which
     * means that GC signals are generated for all major collections.  Note that
     * a GC signal handler must also be installed for GC signals to be generated.
     *)
    val signalThreshold : int -> unit

  end
