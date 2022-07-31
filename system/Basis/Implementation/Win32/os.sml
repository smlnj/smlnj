(* os.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 * Win32 OS interface
 *
 *)

structure OSImp : OS =
  struct

    open OS (* open type-only structure to get types *)

    type syserror = int

(*    exception SysErr of (string * syserror option)  *)
    exception SysErr = Assembly.SysErr

    fun errorName _ = "<OS.errorName unimplemented>"
    fun errorMsg _ = "<OS.errorMessage unimplemented>"
    fun syserror _ = raise Fail "OS.syserror unimplemented"

    structure FileSys = OS_FileSys
    structure Path = OS_Path
    structure Process = OS_Process
    structure IO = OS_IO

  end (* OS *)

