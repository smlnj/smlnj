(* input-sig.sml
 *
 * COPYRIGHT (c) 2016 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature INPUT =
  sig

  (* parse a file; return NONE on error *)
    val parseFile : string -> LexSpec.spec option

  end
