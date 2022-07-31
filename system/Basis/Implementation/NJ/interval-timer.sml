(* interval-timer.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * An interface to the runtime-system interval timer.
 *)

local
  structure Int = IntImp
  structure Word64 = Word64Imp
  structure Time = TimeImp
in
structure IntervalTimer : INTERVAL_TIMER =
  struct

    fun cfun x = CInterface.c_function "SMLNJ-RunT" x

    val itick' : unit -> Word64.word = cfun "itick"
    val setIntTimer' : Word64.word option -> unit = cfun "setIntTimer"

    fun minInterval () = Time.fromNanoseconds (Word64.toLargeInt (itick' ()))

    fun fromTimeOpt NONE = NONE
      | fromTimeOpt (SOME t) = SOME(Word64.fromLargeInt (Time.toNanoseconds t))

    fun setIntTimer timOpt = setIntTimer' (fromTimeOpt timOpt)

  end
end (* local *)
