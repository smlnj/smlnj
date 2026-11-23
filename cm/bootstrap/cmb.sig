(* cmb.sig
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * Signature for CMB.
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)

signature CMB =
  sig
    val make' : string option -> bool
    val make : unit -> bool
    val reset : unit -> unit
    val symval : string -> { get: unit -> int option, set: int option -> unit }
  end
