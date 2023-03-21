(* smlnj-lib/Dev/PrettyPrint/src91/device.sig *)

(* signature of a device as a structure *)

signature DEVICE =
sig

    type style


    val space  : int -> unit
    val indent : int -> unit
    val newline : unit -> unit
    val string : string -> unit
    val token : string -> unit
    val flush : unit -> unit

    val lineWidth : int ref

    val renderStyled : style -> unit
      (* used to render styled formats in render.sml *)

end (* signature DEVICE *)
