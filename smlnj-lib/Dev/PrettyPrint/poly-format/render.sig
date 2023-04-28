(* smlnj-lib/PrettyPrint/src/render.sig *)

(* Version 7.4
 *   -- no change
 * Version 7
 *   -- no change
 * Version 6
 *   -- factored into separate Format, Render : RENDER, and NewPP : NEW_PP structures
 * Defines: signature RENDER *)

(* In this version, the render function takes an (imperative) output function of type string -> unit. *)

signature RENDER =
sig

  val render : Format.format * (string -> unit) * int -> unit
  (* render (fmt, output, lineWidth): render fmt against lineWidth, using output to print a layout *)

end (* signature RENDER *)
