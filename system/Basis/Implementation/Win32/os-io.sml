(* os-io.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Implementation of OS.IO structure for Win32.
 * It implements a simple type of polling for file objects.
 * This file requires a runtime system supporting polling in Win32-IO.
 *)

local
  structure Word = WordImp
  structure Int = IntImp
  structure Int32 = Int32Imp
  structure Time = TimeImp
in
structure OS_IO : OS_IO =
  struct

    structure W32G = Win32_General
    structure W32FS = Win32_FileSys

    exception SysErr = Assembly.SysErr

  (* = IODesc of Handle.t ref | SockDesc of int *)
    datatype iodesc = datatype OS.IO.iodesc

    fun hash (IODesc(ref h)) = Handle.hash h
      | hash (SockDesc s) = Word.fromInt s

    fun compare (IODesc(ref ha), IODesc(ref hb)) = Handle.compare(ha, hb)
      | compare (SockDesc s1, SockDesc s2) = Int.compare(s1, s2)
      | compare (IODesc _, SockDesc _) = LESS
      | compare (SockDesc _, IODesc _) = GREATER

    datatype iodesc_kind = K of string

    structure Kind =
      struct
	val file = K "FILE"
	val dir = K "DIR"
	val symlink = K "LINK"
	val tty = K "TTY"
	val pipe = K "PIPE"
	val socket = K "SOCK"
	val device = K "DEV"
      end

    fun kind (IODesc(ref h)) = (case W32FS.getFileAttributes' h
	   of NONE =>  K "UNKNOWN"
	    | SOME w => if W32FS.isRegularFile h then Kind.file else Kind.dir
	  (* end case *))
      | kind (SockDesc _) = K "SOCK"

  (* no win32 polling devices for now *)
    val noPolling = "polling not implemented for win32 for this device/type"

    type poll_flags = {rd : bool, wr: bool, pri: bool}
    datatype poll_desc = PollDesc of (iodesc * poll_flags)
    datatype poll_info = PollInfo of poll_desc

    fun pollDesc id = SOME(PollDesc(id, {rd=false, wr=false, pri=false}))
    fun pollToIODesc (PollDesc(pd, _)) = pd

    exception Poll

    fun pollIn (PollDesc (iod,{rd,wr,pri})) = PollDesc (iod,{rd=true,wr=wr,pri=pri})
    fun pollOut (PollDesc (iod,{rd,wr,pri})) = PollDesc (iod,{rd=rd,wr=true,pri=pri})
    fun pollPri (PollDesc (iod,{rd,wr,pri})) = PollDesc (iod,{rd=rd,wr=wr,pri=true})

    local
      val poll' : ((W32G.hndl * word) list * (int * word) list * Int32.int option
	    -> ((W32G.hndl * word) list * (int * word) list)) =
	  CInterface.c_function "WIN32-IO" "poll"

      fun join (false, _, w) = w
	| join (true, b, w) = Word.orb(w, b)
      fun test (w, b) = (Word.andb(w, b) <> 0w0)
      val rdBit = 0w1 and wrBit = 0w2 and priBit = 0w4

      fun toPollInfoIO (fd, w) = PollInfo(PollDesc(IODesc(ref fd), {rd= test(w,rdBit),
								    wr= test(w,wrBit),
								    pri= test(w,priBit)}))
      fun toPollInfoSock (i, w) = PollInfo(PollDesc(SockDesc i, {rd = test(w,rdBit),
								 wr = test(w,wrBit),
								 pri = test(w,priBit)}))
      fun fromPollDescIO (PollDesc(IODesc(ref w), {rd,wr,pri})) =
	    (w, join (rd,rdBit, join (wr,wrBit, join (pri,priBit,0w0))))
      fun fromPollDescSock (PollDesc(SockDesc i, {rd,wr,pri})) =
	    (i, join (rd,rdBit, join (wr,wrBit, join (pri,priBit,0w0))))

      (* To preserve equality, return the original PollDesc passed to poll.
       * This is cheesy, but restructuring the IODesc to no longer have a ref
       * cell is a substantial amount of work, as much of the Win32 FS basis
       * relies on mutability.
       *)
      fun findPollDescFromIO (pollIOs, (fd, w)) = let
	    fun same (PollDesc(IODesc(ref fd'), _)) = (fd' = fd)
	      | same (PollDesc(SockDesc s, _)) = false
	    val desc = List.find same pollIOs
	    in
	      case desc
	       of SOME f => SOME(PollInfo f)
		| NONE => NONE
	    end
    in
    fun poll (pdl, timeOut) = let
	  val timeOut = (case timeOut
		 of SOME t => SOME(Int32.fromLarge(Time.toMilliseconds t))
		  | NONE => NONE
		(* end case *))
	  fun partDesc (PollDesc(IODesc _, _)) = true
	    | partDesc (PollDesc(SockDesc _, _)) = false
	  val (pollIOs, pollSocks) = List.partition partDesc pdl
	  val (infoIO, infoSock) = poll' (
		List.map fromPollDescIO pollIOs,
		List.map fromPollDescSock pollSocks,
		timeOut)
	  in
	    List.@ (List.mapPartial (fn (p) => findPollDescFromIO(pollIOs,p)) infoIO,
		    List.map toPollInfoSock infoSock)
	  end
    end (* local *)

    fun isIn (PollInfo(PollDesc(_, flgs))) = #rd flgs
    fun isOut (PollInfo(PollDesc(_, flgs))) = #wr flgs
    fun isPri (PollInfo(PollDesc(_, flgs))) = #pri flgs
    fun infoToPollDesc (PollInfo pd) = pd

  end
end (* local *)
