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

  val render : Format.format * int -> unit
  (* render fmt: render fmt, using lineWidth, and (implicitly) the output function
   * provided by a DEVICE structure as a functor parameter to RenderFn. *)

end (* signature RENDER *)
