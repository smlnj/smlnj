(* measure.sig
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

signature MEASURE =
  sig

    (* return the width of a flat-render of the format *)
    val measure : Format.format -> int

    (* return the width of a flat-render of the element list *)
    val measureElements : Format.element list -> int

    (* return the width of a flat-render of the format list, where the first
     * argument is the width of the breaks that separate the list items.
     *)
    val measureFormats : int * Format.format list -> int

  end  (* signature MEASURE *)
