(* os-io.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure OS_IO : OS_IO =
  struct
    structure IOM = IOManager

    type iodesc = OS.IO.iodesc
    type iodesc_kind = OS.IO.iodesc_kind

    val hash = OS.IO.hash
    val compare = OS.IO.compare
    val kind = OS.IO.kind

    structure Kind = OS.IO.Kind

    type poll_desc = OS.IO.poll_desc
    type poll_info = OS.IO.poll_info

    val pollDesc = OS.IO.pollDesc
    val pollToIODesc = OS.IO.pollToIODesc

    exception Poll = OS.IO.Poll

  (* set polling events; if the polling operation is not appropriate
   * for the underlying I/O device, then the Poll exception is raised.
   *)
    val pollIn  = OS.IO.pollIn
    val pollOut = OS.IO.pollOut
    val pollPri = OS.IO.pollPri

  (* polling functions *)
    local
      fun timeOut t = CML.wrap(CML.timeOutEvt t, fn () => [])
      fun ioEvt pd = CML.wrap(IOM.ioEvt pd, fn info => [info])
    in
    fun pollEvt [pd] = ioEvt pd
      | pollEvt _ = raise Fail "IO.OS.pollEvt not fully implemented"
    fun poll ([pd], NONE) = CML.sync(ioEvt pd)
      | poll ([pd], SOME t) = CML.select[timeOut t, ioEvt pd]
      | poll _ = raise Fail "IO.OS.poll not fully implemented"
    end

  (* check for conditions *)
    val isIn 		= OS.IO.isIn
    val isOut		= OS.IO.isOut
    val isPri		= OS.IO.isPri
    val infoToPollDesc  = OS.IO.infoToPollDesc

  end
