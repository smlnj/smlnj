(* src90/style.sml *)

structure Style =
struct

datatype color = RED | BLUE | GREEN  (* | ... *)

datatype face = BOLD | ITALIC  (* | ... *)

datatype style
  = Color of color
  | Face of face

end (* structure Style *)
