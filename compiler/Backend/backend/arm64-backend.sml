(* arm64-backend.sml
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Arm64Backend =
    BackendFn (
      structure M = CodeGeneratorFn (Arm64Spec)
      val cproto_conv = "ccall")
