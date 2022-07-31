(* os-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *)

signature CML_OS =
  sig
    type syserror

    val errorName : syserror -> string
    val errorMsg : syserror -> string

    exception SysErr of (string * syserror option)

    structure FileSys : OS_FILE_SYS
    structure Path : OS_PATH
    structure Process : CML_OS_PROCESS
    structure IO : CML_OS_IO

  end;

