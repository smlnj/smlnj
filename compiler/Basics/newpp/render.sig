(* sml/Dev/pp/new/new7/render.sig *)

(* Version 7
 * -- no change
 * Version 6
 *   -- factored into separate Format, Render : RENDER, and NewPP : NEW_PP structures
 * Defines: signature RENDER *)

(* In this version, the render function takes an (imperative) output function of type string -> unit. *)

signature RENDER =
sig

  val render : Format.format * (string -> unit) * int -> unit (* int arg is line width *)
  (* render a format given an output function and a line length *)

end (* signature RENDER *)
