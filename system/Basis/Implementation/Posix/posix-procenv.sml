(* posix-procenv.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Signature for POSIX 1003.1 process environment submodule
 *)

local
  structure Time = TimeImp
  structure Real = RealImp
  structure SysWord = SysWordImp
in
structure POSIX_ProcEnv =
  struct

    structure FS = POSIX_FileSys
    structure P  = POSIX_Process

    fun cfun x = CInterface.c_function "POSIX-ProcEnv" x

    type pid = P.pid
    type uid = FS.uid
    type gid = FS.gid
    type file_desc = FS.file_desc

    type s_int = SysInt.int

    fun uidToWord (FS.UID i) = i
    fun wordToUid i = FS.UID i

    fun gidToWord (FS.GID i) = i
    fun wordToGid i = FS.GID i

    val getpid' : unit -> s_int = cfun "getpid"
    val getppid' : unit -> s_int = cfun "getppid"
    fun getpid () = P.PID(getpid' ())
    fun getppid () = P.PID(getppid' ())

    val getuid' : unit -> SysWord.word = cfun "getuid"
    val geteuid' : unit -> SysWord.word = cfun "geteuid"
    val getgid' : unit -> SysWord.word = cfun "getgid"
    val getegid' : unit -> SysWord.word = cfun "getegid"
    fun getuid () = FS.UID(getuid' ())
    fun geteuid () = FS.UID(geteuid' ())
    fun getgid () = FS.GID(getgid' ())
    fun getegid () = FS.GID(getegid' ())

    val setuid' : SysWord.word -> unit = cfun "setuid"
    val setgid' : SysWord.word -> unit = cfun "setgid"
    fun setuid (FS.UID uid) = setuid' uid
    fun setgid (FS.GID gid) = setgid' gid

    val getgroups' : unit -> SysWord.word list = cfun "getgroups"
    fun getgroups () = List.map FS.GID (getgroups'())

    val getlogin : unit -> string = cfun "getlogin"

    val getpgrp' : unit -> s_int = cfun "getpgrp"
    val setsid' : unit -> s_int = cfun "setsid"
    val setpgid' : s_int * s_int -> unit = cfun "setpgid"
    fun getpgrp () = P.PID(getpgrp' ())
    fun setsid () = P.PID(setsid' ())
    fun setpgid {pid : pid option, pgid : pid option} = let
          fun cvt NONE = 0
            | cvt (SOME(P.PID pid)) = pid
          in
            setpgid'(cvt pid, cvt pgid)
          end

    val uname : unit -> (string * string) list = cfun "uname"

    val sysconf = P.sysconf

    val time' : unit -> Int32.int = cfun "time"
    val time = Time.fromSeconds o Int32Imp.toLarge o time'

    (* times in clock ticks *)
    val times' : unit -> Int32.int * Int32.int * Int32.int * Int32.int * Int32.int
	  = cfun "times"

    val ticksPerSec = IntImp.toLarge (SysWord.toIntX (sysconf "CLK_TCK"))

    val ticksToTime = (case IntInfImp.quotRem (TimeImp.fractionsPerSecond, ticksPerSec)
           of (factor, 0) =>
	        (fn ticks => Time.fromFractions (factor * Int32Imp.toLarge ticks))
	    | _ =>
	        (fn ticks => Time.fromFractions (
                      IntInfImp.quot (
                        TimeImp.fractionsPerSecond * Int32Imp.toLarge ticks,
                        ticksPerSec)))
          (* end case *))

    fun times () = let
          val (e,u,s,cu,cs) = times' ()
          in {
            elapsed = ticksToTime e,
            utime = ticksToTime u,
            stime = ticksToTime s,
            cutime = ticksToTime cu,
            cstime = ticksToTime cs
          } end

    val getenv  : string -> string option = cfun "getenv"
    val environ : unit -> string list = cfun "environ"

    val ctermid : unit -> string = cfun "ctermid"

    val ttyname' : s_int -> string = cfun "ttyname"
    fun ttyname fd = ttyname' (FS.intOf fd)

    val isatty' : s_int -> bool = cfun "isatty"
    fun isatty fd = isatty' (FS.intOf fd)

  end (* structure POSIX_Proc_Env *)

end (* local *)
