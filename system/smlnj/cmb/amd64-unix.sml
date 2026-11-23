(* amd64-unix.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure AMD64UnixCMB = BootstrapCompileFn (
    structure Backend = AMD64CCallBackend
    val useStream = Backend.Interact.useStream
    val os = SMLofNJ.SysInfo.UNIX
    val load_plugin = CM0.load_plugin)
