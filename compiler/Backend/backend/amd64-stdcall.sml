(* amd64-stdcall.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * AMD64 backend specialized to the "ccall" (default) calling convention.
 *)

structure AMD64StdCallBackend =
    BackendFn (
      structure M = CodeGeneratorFn (AMD64Spec)
      val cproto_conv = "stdcall")
