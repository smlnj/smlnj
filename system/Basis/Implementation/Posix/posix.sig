(* posix.sig
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Signature for POSIX 1003.1 binding
 *)

signature POSIX_2004 =
  sig

    structure Error   : POSIX_ERROR
    structure Signal  : POSIX_SIGNAL
    structure Process : POSIX_PROCESS
    structure ProcEnv : POSIX_PROC_ENV
    structure FileSys : POSIX_FILE_SYS
    structure IO      : POSIX_IO
    structure SysDB   : POSIX_SYS_DB
    structure TTY     : POSIX_TTY_2004

    sharing type Process.pid = ProcEnv.pid = TTY.pid
        and type Process.signal = Signal.signal
        and type ProcEnv.file_desc = FileSys.file_desc = TTY.file_desc
        and type FileSys.open_mode = IO.open_mode
        and type ProcEnv.uid = FileSys.uid = SysDB.uid
        and type ProcEnv.gid = FileSys.gid = SysDB.gid

  end (* signature POSIX *)

signature POSIX_2021 =
  sig

    structure Error   : POSIX_ERROR
    structure Signal  : POSIX_SIGNAL
    structure Process : POSIX_PROCESS
    structure ProcEnv : POSIX_PROC_ENV
    structure FileSys : POSIX_FILE_SYS
    structure IO      : POSIX_IO
    structure SysDB   : POSIX_SYS_DB
    structure TTY     : POSIX_TTY_2021

    sharing type Process.pid = ProcEnv.pid = TTY.pid
        and type Process.signal = Signal.signal
        and type ProcEnv.file_desc = FileSys.file_desc = TTY.file_desc
        and type FileSys.open_mode = IO.open_mode
        and type ProcEnv.uid = FileSys.uid = SysDB.uid
        and type ProcEnv.gid = FileSys.gid = SysDB.gid

  end (* signature POSIX *)

signature POSIX = POSIX_2021
