(* posix.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Structure for POSIX 1003.1 binding
 *)

structure Posix : POSIX =
  struct

    structure Error   = POSIX_Error
    structure Signal  = POSIX_Signal
    structure Process = POSIX_Process
    structure ProcEnv = POSIX_ProcEnv
    structure FileSys = POSIX_FileSys
    structure IO      = POSIX_IO
    structure SysDB   = POSIX_Sys_DB
    structure TTY     = POSIX_TTY

  end (* structure Posix *)
