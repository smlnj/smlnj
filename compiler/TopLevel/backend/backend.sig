(* backend.sig
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

signature BACKEND =
  sig
    structure Profile : PROFILE
    structure Compile : COMPILE
    structure Interact : INTERACT

    (* the following is used by CM *)
    val architecture : string

  end
