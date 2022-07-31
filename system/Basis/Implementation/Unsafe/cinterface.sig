(* cinterface.sig
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature CINTERFACE =
  sig

    exception CFunNotFound of string
    val c_function : string -> string -> ('a -> 'b)
	(* the operation for binding ML callable C functions; raises
	 * the exception CFunNotFound if the function is unknown.
	 *)

    type c_function
    val bindCFun : (string * string) -> c_function

  (* utilities for working with system constants *)
    type system_const = (int * string)

    exception SysConstNotFound of string

    val findSysConst : (string * system_const list) -> system_const option
    val bindSysConst : (string * system_const list) -> system_const

  end
