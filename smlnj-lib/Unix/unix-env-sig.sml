(* unix-env-sig.sml
 *
 * COPYRIGHT (c) 2007 The Fellowship of SML/NJ (http://smlnj.org)
 * All rights reserved.
 *
 * A UNIX environment is a list of strings of the form "name=value", where
 * the "=" character does not appear in name.
 * NOTE: binding the user's environment as an ML value and then exporting the
 * ML image can result in incorrect behavior, since the environment bound in the
 * heap image may differ from the user's environment when the exported image
 * is used.
 *)

signature UNIX_ENV =
  sig

    val getFromEnv : (string * string list) -> string option
	(* return the value, if any, bound to the name. *)

    val getValue : {name : string, default : string, env : string list} -> string
	(* return the value bound to the name, or a default value *)

    val removeFromEnv : (string * string list) -> string list
	(* remove a binding from an environment *)

    val addToEnv : (string * string list) -> string list
	(* add a binding to an environment, replacing an existing binding
	 * if necessary.
	 *)

    val environ : unit -> string list
	(* return the user's environment *)

    val getEnv : string -> string option
	(* return the binding of an environment variable in the
	 * user's environment.
	 *)

  end; (* UNIX_ENV *)

