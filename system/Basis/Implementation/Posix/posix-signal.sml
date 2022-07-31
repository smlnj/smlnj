(* posix-signal.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Structure for POSIX 1003.1 signals.
 *)

local
  structure SysWord = SysWordImp
in
structure POSIX_Signal =
  struct

    datatype signal = SIG of SysInt.int

    fun toWord (SIG i) = SysWord.fromInt i
    fun fromWord w = SIG (SysWord.toInt w)

    val osval : string -> SysInt.int =
          CInterface.c_function "POSIX-Signal" "osval"

    val abrt = SIG(osval "abrt")
    val alrm = SIG(osval "alrm")
    val fpe  = SIG(osval "fpe")
    val hup  = SIG(osval "hup")
    val ill  = SIG(osval "ill")
    val int  = SIG(osval "int")
    val kill = SIG(osval "kill")
    val pipe = SIG(osval "pipe")
    val quit = SIG(osval "quit")
    val segv = SIG(osval "segv")
    val term = SIG(osval "term")
    val usr1 = SIG(osval "usr1")
    val usr2 = SIG(osval "usr2")
    val chld = SIG(osval "chld")
    val cont = SIG(osval "cont")
    val stop = SIG(osval "stop")
    val tstp = SIG(osval "tstp")
    val ttin = SIG(osval "ttin")
    val ttou = SIG(osval "ttou")
    val bus  = SIG(osval "bus")

  end (* structure POSIX_Signal *)

end (* local *)
