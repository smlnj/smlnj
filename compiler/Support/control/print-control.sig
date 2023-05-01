(* Admin/control/print-control.sig
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

signature PRINT_CONTROL =
sig

  val printDepth  : int ref
  val printLength : int ref
  val stringDepth : int ref
  val intinfDepth : int ref
  val lineWidth   : int ref
  val printLoop   : bool ref
  val signatures  : int ref
  val printOpens  : bool ref
  val out : {say : string -> unit, flush : unit -> unit} ref
  val say : string -> unit
  val flush : unit -> unit

end (* signature PRINT_CONTROL *)
