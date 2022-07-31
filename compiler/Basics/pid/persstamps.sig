(* Copyright 1996 by AT&T Bell Laboratories *)
(* persstamps.sig *)

signature PERSSTAMPS = sig
    eqtype persstamp

    val persStampSize : int

    val compare : persstamp * persstamp -> order
    (* total ordering on persstamps *)

    val toHex : persstamp -> string
    (* convert the persstamp to a printable representation (hex digits) *)

    val fromHex : string -> persstamp option

    val toBytes   : persstamp -> Word8Vector.vector
    val fromBytes : Word8Vector.vector -> persstamp

end (* signature PERSSTAMPS *)
