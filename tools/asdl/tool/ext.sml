(* ext.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * Register the ASDL classifier
 *)

structure ASDLExt =
  struct

    val _ = Tools.registerClassifier (
		Tools.stdSfxClassifier { sfx = "asdl", class = "asdlgen" })

  end
