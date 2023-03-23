(* smlnj-lib/Dev/PrettyPrint/src91/device.sig *)

(* signature of a device as a structure *)

signature DEVICE =
sig

    val space  : int -> unit
    val indent : int -> unit
    val newline : unit -> unit
    val string : string -> unit
    val token : string -> unit
    val flush : unit -> unit

    val lineWidth : int (* not used? *)

    val renderStyled : Style.style * (unit -> 'a) -> 'a
      (* used to render styled formats in render-fct.sml *)

end (* signature DEVICE *)
