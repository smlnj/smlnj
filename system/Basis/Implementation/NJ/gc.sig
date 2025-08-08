(* gc.sig
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Garbage collector control and stats.
 *)

signature GC =
  sig

    (* return the number of generations (not including the nursery) *)
    val numGens : unit -> int

    (* return the size of the nursery in bytes *)
    val nurserySizeInBytes : unit -> int

    (* invoke GC; the argument specifies the minimum generation to collect *)
    val doGC : int -> unit

    (* turn on/off GC messages (default is off) *)
    val messages : bool -> unit

    (* set the threshold for generating a GC signal.  The default is 1, which
     * means that GC signals are generated for all major collections.  Note that
     * a GC signal handler must also be installed for GC signals to be generated.
     *)
    val signalThreshold : int -> unit

    (* reset the GC and allocation counters.  Prior to reseting the counters, this
     * function either collects the nursery (when is argument is false) or all of
     * the heap's generations (when its argument is true).
     *)
    val resetCounters : bool -> unit

    (* read the counts accumulated since the last call to `resetCounters`. *)
    val readCounters : unit -> {
            nbAlloc : IntInf.int,       (* the number of bytes allocated in the
                                         * nursery.
                                         *)
            nStores : IntInf.int option,(* an optional count of the number of store-list
                                         * items allocated in the nursery.  It will be
                                         * `NONE` if the count was not collected.
                                         *)
            nbAlloc1 : IntInf.int,      (* the number of bytes of allocation in
                                         * first generation (these are for objects
                                         * that are deemed too large for the nursery)
                                         *)
            nbPromote : IntInf.int,     (* the number of bytes that have been
                                         * promoted from the nursery into the
                                         * first generation.
                                         *)
            nGCs : int list             (* the number of collections by generation.
                                         * The first entry is the nursery, then first
                                         * generation, etc.  If the list is empty,
                                         * there were no collections since the last
                                         * call to `resetCounters`.
                                         *)
          }

  end
