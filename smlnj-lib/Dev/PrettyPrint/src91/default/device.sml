(* default-device.sml *)

structure DefaultDevice : DEVICE where style = Style.style =
struct

  type style = Style.style

  (* a single, fixed outstream defined for this device *)
  (* should the output stream be settable, or should it be a parameter? 
   * should the output functions all take an outstream as an extra argument? *)
  val outstream = TextIO.stdOut

  val lineWidth : int ref = ref 90
      (* is this used? *)

  (* ====== the output functions ====== *)

  (* space : int -> unit *)
  (* output some number of spaces to the device *)
  fun space (n: int) = TextIO.output (outstream, StringCvt.padLeft #" " n "")

  (* indent : int -> unit *)
  (* output an indentation of the given width to the device *)
  val indent = space

  (* newline : unit -> unit *)
  (* output a new-line to the device *)
  fun newline () = TextIO.output1 (outStream, #"\n")

  (* string : string -> unit *)
  (* output a string/character in the current style to the device *)
  fun string (s: string) = TextIO.output (dst, s)

  (* token : string -> unit *)
  (* output a string/character in the current style to the device *)
  val token = string

  (* flush : unit -> unit *)
  (* if the device is buffered, then flush any buffered output *)
  fun flush () = TextIO.flushOut outstream


  (* renderStyled: style * (unit -> unit) -> unit *)
  fun renderStyled (style: style, renderFormat: unit -> unit) : unit =
      renderFormat ()

end (* structure DefaultDevice : DEVICE *)
