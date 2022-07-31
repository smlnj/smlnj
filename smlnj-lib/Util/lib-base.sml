(* lib-base.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure LibBase : LIB_BASE =
  struct

  (* raised to report unimplemented features *)
    exception Unimplemented of string

  (* raised to report internal errors *)
    exception Impossible of string

  (* raised by searching operations *)
    exception NotFound

  (* raise the exception Fail with a standard format message. *)
    fun failure {module, func, msg} =
	  raise (Fail(concat[module, ".", func, ": ", msg]))

  end (* LibBase *)
