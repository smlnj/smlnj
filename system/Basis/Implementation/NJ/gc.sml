(* gc.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Garbage collector control and stats.
 *)

structure GC : GC =
  struct

    structure CI = CInterface

    val gcCtl : ((string * int ref) list -> unit) =
	  CI.c_function "SMLNJ-RunT" "gcControl"

    fun numGens () = let
          val r = ref 0
          in
            gcCtl [("NumGens", r)];
            !r
          end

    fun nurserySizeInBytes () = let
          val r = ref 0
          in
            gcCtl [("NurserySize", r)];
            !r
          end

    fun doGC n = gcCtl [("DoGC", ref n)]

    fun messages true = gcCtl [("Messages", ref 1)]
      | messages false = gcCtl [("Messages", ref 0)]

    fun signalThreshold n = gcCtl [("SigThreshold", ref n)]

    val resetCounters : bool -> unit =
          CI.c_function "SMLNJ-RunT" "gcCounterReset"

    val read' : unit -> word * word * word option * word * word * word list =
          CI.c_function "SMLNJ-RunT" "gcCounterRead"

    fun readCounters () = let
          (* results are:
           *   s     -- scaling factor for allocation counts
           *   a     -- scaled nursery allocation count
           *   st    -- optional count of store-list entries (currently == 0)
           *   a1    -- scaled first-generation allocation count
           *   p     -- scaled count of promotions to first generation
           *   ngcs  -- number of collections by generation
           *)
          val (s, a, st, a1, p, ngcs) = read'()
          val scale = InlineT.Word.toLargeInt s
          in {
            nbAlloc = scale * InlineT.Word.toLargeInt a,
            nStores = (case st
               of SOME st => SOME(InlineT.Word.toLargeInt st)
                | _ => NONE
              (* end case *)),
            nbAlloc1 = scale * InlineT.Word.toLargeInt a1,
            nbPromote = scale * InlineT.Word.toLargeInt p,
            nGCs = List.map InlineT.Word.toIntX ngcs
          } end

  end
