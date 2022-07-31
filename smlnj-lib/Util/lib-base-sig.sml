(* lib-base-sig.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature LIB_BASE =
  sig

    exception Unimplemented of string
	(* raised to report unimplemented features *)
    exception Impossible of string
	(* raised to report internal errors *)

    exception NotFound
	(* raised by searching operations *)

    val failure : {module : string, func : string, msg : string} -> 'a
	(* raise the exception Fail with a standard format message. *)

  end (* LIB_BASE *)
