(* os.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *)

structure OS : OS =
  struct
    structure IO = OS_IO
    structure Path = OS.Path
    structure Process = OS_Process
    structure FileSys = OS.FileSys	(** may need to protect system calls *)

    type syserror = OS.syserror

    exception SysErr = OS.SysErr

    val errorName = OS.errorName
    val errorMsg = OS.errorMsg

  end
