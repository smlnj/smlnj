(* load.sml
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Load all of the examples.
 *)

CM.make "../src/pretty-print-lib.cm";
CM.make "../devices/sources.cm";

List.app use [
    "pp.sml",
    "expdecl.sml",
    "strdecl.sml",
    "wadler-trees1.sml",
    "wadler-trees2.sml",
    "words.sml"
  ];
