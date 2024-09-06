(* Copyright 1996 by AT&T Bell Laboratories *)
(* persstamps.sig *)

(* PERSSTAMPS should be renamed PID, for "persistent identifier", and the type perstamp should
 * be renamed "pid".  Then the definition pid = PersStamps.persstamp in stamps.sml could
 * be dropped.
*)

signature PERSSTAMPS =  (* signature PID = *)
sig
    eqtype persstamp  (* should be replaced by "pid", throughout *)

    val persStampSize : int  (* size : int *)

    val compare : persstamp * persstamp -> order
    (* total ordering on persstamps *)

    val toHex : persstamp -> string
    (* convert the persstamp to a printable representation (hex digits) *)

    val fromHex : string -> persstamp option

    val toBytes   : persstamp -> Word8Vector.vector
    val fromBytes : Word8Vector.vector -> persstamp

end (* signature PERSSTAMPS (PID) *)
