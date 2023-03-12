(* PrettyPrint/src91/term/ansiterm-style.sml *)

(* formatting style for ANSITerm device *)

structure ANSITermStyle =
struct

datatype style
  = FG of color   (* set foreground color *)
  | BG of color   (* set background color *)
  | BF (* boldface on *)
  | UL (* underline on *)
  | BL (* blinking on *)
  | DM (* dim on *)
  | IV (* invisible on *)

end (* structure ANSITermStyle *)
