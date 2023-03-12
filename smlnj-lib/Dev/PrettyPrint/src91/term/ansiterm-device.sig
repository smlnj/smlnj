(* smlnj-lib/Dev/PrettyPrint/src91/device.sig *)

(* signature of a device as a structure *)

signature DEVICE =
sig

    type style

    val renderStyled : style -> unit

    val space  : int -> unit
    val indent : int -> unit
    val newline : unit -> unit
    val string : string -> unit
    val token : string -> unit
    val flush : unit -> unit

    val getLineWidth : unit -> int
    val setLineWidth : int -> unit

end (* signature DEVICE *)
