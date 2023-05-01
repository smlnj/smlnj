(* smlnj-lib/PrettyPrint/src/render.sig *)

(* Version 7.4
 *   -- no change
 * Version 7
 *   -- no change
 * Version 6
 *   -- factored into separate Format, Render : RENDER, and NewPP : NEW_PP structures
 * Version 9.1
 *   -- RENDER is now the output signature of a RenderFn functor that takes a 
 *      DEVICE structure as argument
 *
 * Defines: signature RENDER *)

signature RENDER =
sig

  val render : Format.format * int -> HTML.text
  (* render fmt: render fmt, using lineWidth and output functions
   * provided by a device structure parameter. *)

end (* signature RENDER *)
