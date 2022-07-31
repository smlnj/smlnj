(* gc.sml
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Garbage collector control and stats.
 *)

structure GC : GC =
  struct

    val gcCtl : ((string * int ref) list -> unit) =
	  CInterface.c_function "SMLNJ-RunT" "gcControl"

    fun doGC n = gcCtl [("DoGC", ref n)]

    fun messages true = gcCtl [("Messages", ref 1)]
      | messages false = gcCtl [("Messages", ref 0)]

    fun signalThreshold n = gcCtl [("SigThreshold", ref n)]

  end
