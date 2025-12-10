(*
 * Signature for CMB.
 *
 * (C) 2000 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature CMB = sig
    val make' : string option -> bool
    val make : unit -> bool
    val reset : unit -> unit
    val symval : string -> { get: unit -> int option, set: int option -> unit }
end
