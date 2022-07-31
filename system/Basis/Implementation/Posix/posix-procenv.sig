(* posix-procenv.sig
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Signature for POSIX 1003.1 process environment submodule
 *)

signature POSIX_PROC_ENV =
  sig

    eqtype pid
    eqtype file_desc

    eqtype uid
    eqtype gid

    val uidToWord : uid -> SysWord.word
    val wordToUid : SysWord.word -> uid
    val gidToWord : gid -> SysWord.word
    val wordToGid : SysWord.word -> gid

    val getpid  : unit -> pid
    val getppid : unit -> pid

    val getuid  : unit -> uid
    val geteuid : unit -> uid
    val getgid  : unit -> gid
    val getegid : unit -> gid

    val setuid : uid -> unit
    val setgid : gid -> unit

    val getgroups : unit -> gid list

    val getlogin : unit -> string

    val getpgrp : unit -> pid
    val setsid  : unit -> pid
    val setpgid : {pid : pid option, pgid : pid option} -> unit

    val uname : unit -> (string * string) list

    val time : unit -> Time.time

    val times : unit -> {
            elapsed : Time.time,  (* elapsed system time *)
            utime   : Time.time,  (* user time of process *)
            stime   : Time.time,  (* system time of process *)
            cutime  : Time.time,  (* user time of terminated child processes *)
            cstime  : Time.time   (* system time of terminated child processes *)
          }

    val getenv  : string -> string option
    val environ : unit -> string list

    val ctermid : unit -> string
    val ttyname : file_desc -> string
    val isatty : file_desc -> bool

    val sysconf : string -> SysWord.word

  end (* signature POSIX_PROC_ENV *)
