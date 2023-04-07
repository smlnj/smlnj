(* PrettyPrint/src91/term/ansiterm-style.sml *)

(* formatting style for ANSITerm device *)

structure Style =
struct

datatype style
  = FG of ANSITerm.color   (* foreground color *)
  | BG of ANSITerm.color   (* background color *)
  | BF (* boldface on *)
  | UL (* underline on *)
  | BL (* blinking on *)
  | DM (* dim on *)
  | RV (* reverse fg/bg on *)
  | IV (* invisible on *)
  | NOSTYLE (* terminal in default mode *)

end (* structure ANSITermStyle *)
