(* os.sig
 *
 * COPYRIGHT (c) 2007 Fellowship of SML/NJ
 * All rights reserved.
 *
 *)

signature OS =
  sig
    type syserror

    val errorName : syserror -> string
    val syserror : string -> syserror option
    val errorMsg : syserror -> string

    exception SysErr of (string * syserror option)

    structure FileSys : OS_FILE_SYS
    structure Path : OS_PATH
    structure Process : OS_PROCESS
    structure IO : OS_IO

  end;


